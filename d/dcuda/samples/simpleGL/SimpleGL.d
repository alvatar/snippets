module SimpleGL;

import xf.dcuda.cuda;
import xf.dcuda.cutil;
import xf.dcuda.Utils;

import xf.dog.Dog;
import xf.dog.ext.VERSION_1_5,
		xf.dog.ext.WGL_EXT_swap_control;
//		xf.dog.ext.ARB_vertex_buffer_object;

import xf.hybrid.Hybrid;
import xf.hybrid.backend.GL : HybridRenderer = Renderer, GLViewport, TopLevelWindow;
import xf.core.JobHub;
import xf.omg.core.LinearAlgebra : vec3d, vec2;

import tango.core.Thread;
import tango.time.StopWatch;
import tango.util.log.Trace;
import tango.stdc.stringz : toStringz;

// constants
const uint window_width = 512;
const uint window_height = 512;

const uint mesh_width = 256;
const uint mesh_height = 256;

CUfunction simpleGL = null;

// vbo variables
uint vbo;

float anim = 0.0f;
float rotate_x = 0.0f, rotate_y = 0.0f;
float translate_z = -3.0f;

bool VSYNC = false;
StopWatch timer;
bool programRunning;

CUresult initCUDA(CUfunction *pSimpleGL) {
	initDcuda();
	initDcutil();

	CUcontext cuContext;
	CUmodule cuModule;
	CUfunction cuFunction;

	CUdevice cuDevice;
	int deviceId = 0;
	deviceInitDrv(cuDevice, deviceId);

	cuSafeCallNoSync(cuCtxCreate(&cuContext, 0, cuDevice));

	cuSafeCallNoSync(cuModuleLoad(&cuModule, toStringz(".\\data\\simpleGL_kernel.cubin")));

	cuSafeCallNoSync(cuModuleGetFunction( &cuFunction, cuModule, "kernel" ));
	*pSimpleGL = cuFunction;
	return CUDA_SUCCESS;
}

//TODO: do it only once
void initGL(GL gl) {
/+	if (gl.ext(ARB_vertex_buffer_object).supported) {
//		Trace.formatln("ARB_vertex_buffer_object supported");
	} else {
		Trace.formatln("ARB_vertex_buffer_object NOT supported");
	}+/

	// default initialization
	gl.ClearColor( 0.0f, 0.0f, 1.0f, 1.0f);
	gl.Disable( GL_DEPTH_TEST);

	// viewport
	gl.Viewport( 0, 0, window_width, window_height);

	// projection
	gl.MatrixMode( GL_PROJECTION);
	gl.LoadIdentity();
	gl.gluPerspective(60.0f, cast(float)window_width / cast(float) window_height, 0.1f, 10.0f);
}

void createVBO(GL gl, uint* vbo) {
	gl.ext(VERSION_1_5) in {
		// create buffer object
		gl.GenBuffers( 1, vbo);
		gl.BindBuffer( GL_ARRAY_BUFFER, *vbo);

		// initialize buffer object
		uint size = mesh_width * mesh_height * 4 * float.sizeof;
		gl.BufferData( GL_ARRAY_BUFFER, size, null, GL_DYNAMIC_DRAW);

		gl.BindBuffer( GL_ARRAY_BUFFER, 0);

		// register buffer object with CUDA
		cuSafeCall(cuGLRegisterBufferObject(*vbo));
	};
}

void runCuda( GLuint vbo) {
	// map OpenGL buffer object for writing from CUDA
//	float4 *dptr;
//	CUDA_SAFE_CALL(cudaGLMapBufferObject( (void**)&dptr, vbo));
	CUdeviceptr devPtr;
	uint size;
	cuGLMapBufferObject(&devPtr, &size, vbo);

	//TODO: do it once
	// execute the kernel
//	dim3 block(8, 8, 1);
//	dim3 grid(mesh_width / block.x, mesh_height / block.y, 1);
//	kernel<<< grid, block>>>(dptr, mesh_width, mesh_height, anim);
	int block_size = 8;
	cuSafeCall(cuFuncSetBlockShape( simpleGL, block_size, block_size, 1 ));
	int offset = 0;
	cuSafeCall(cuParamSeti(simpleGL, offset, devPtr));		offset += devPtr.sizeof;
	cuSafeCall(cuParamSeti(simpleGL, offset, mesh_width));	offset += mesh_width.sizeof;
	cuSafeCall(cuParamSeti(simpleGL, offset, mesh_height));	offset += mesh_height.sizeof;
	cuSafeCall(cuParamSetf(simpleGL, offset, anim));		offset += anim.sizeof;
	cuSafeCall(cuParamSetSize(simpleGL, offset));

	// execute the kernel
//	dim3 block(8, 8, 1);
//	dim3 grid(mesh_width / block.x, mesh_height / block.y, 1);
//	kernel<<< grid, block>>>(dptr, mesh_width, mesh_height, anim);
	cuSafeCall(cuLaunchGrid( simpleGL, mesh_width / block_size, mesh_height / block_size ));

	// unmap buffer object
	cuSafeCall(cuGLUnmapBufferObject( vbo));
}

void main() {
	version (DontMountExtra) {} else gui.vfsMountDir(`../../../../xf/hybrid`);
	scope cfg = loadHybridConfig(`./data/simpleGL.cfg`);
	scope hybridRenderer = new HybridRenderer;
	
	void display(vec2i size, GL gl) {

		initGL(gl);
	
		// run CUDA kernel to generate vertex positions
		runCuda(vbo);

		gl.Clear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

		// set view matrix
		gl.MatrixMode(GL_MODELVIEW);
		gl.LoadIdentity();
		gl.Translatef(0.0f, 0.0f, translate_z);
		gl.Rotatef(rotate_x, 1.0f, 0.0f, 0.0f);
		gl.Rotatef(rotate_y, 0.0f, 1.0f, 0.0f);
	
		gl.ext(VERSION_1_5) in {
			// render from the vbo
			gl.BindBuffer(GL_ARRAY_BUFFER, vbo);
			gl.VertexPointer(4, GL_FLOAT, 0, null);

			gl.EnableClientState(GL_VERTEX_ARRAY);
			gl.Color3f(1.0f, 0.0f, 0.0f);
			gl.DrawArrays(GL_POINTS, 0, mesh_width * mesh_height);
			gl.DisableClientState(GL_VERTEX_ARRAY);
		};
		anim += 0.01f;
	}
	
	gui.begin(cfg).retained;
	gui.push(`main`);
	GLViewport(`glview`).renderingHandler(&display)
		.grabKeyboardFocus();
	gui.pop();
	gui.immediate.end;	

	// initialize CUDA
	cuSafeCall(initCUDA(&simpleGL));
	cuSafeCall(cuGLInit());
	{
		GL gl = TopLevelWindow(`main`).context.begin();
		assert(gl !is null);
		version(Windows){
			gl.ext(WGL_EXT_swap_control) in {
				gl.wglSwapInterval(VSYNC);
			};
		}
		createVBO(gl, &vbo);
	}

	programRunning = true;

	jobHub.addPostFrameJob({
		
		gui.begin(cfg);
			if (gui().getProperty!(bool)("main.frame.closeClicked")) {
				programRunning = false;
			}
		gui.end();
		gui.render(hybridRenderer);
	});

	while (programRunning) {
		float delta = timer.stop;
		timer.start;
		
		jobHub.update(delta);
	}

}

