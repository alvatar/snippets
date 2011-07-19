module xf.terrain.Test;

private {
	version (TraceExceptions) {
		import tango.core.stacktrace.TraceExceptions;
	}
	
	import xf.terrain.HeightmapChunkLoader;
	import xf.terrain.ChunkedTerrain;
	import xf.terrain.ChunkData;
	import xf.terrain.ChunkLoader;
	import xf.terrain.Chunk;
	import xf.omg.core.LinearAlgebra : vec3, vec4;
	
	import xf.input.Input;
	import xf.input.KeySym;
	import xf.core.InputHub;
	import xf.core.JobHub;
	import xf.game.TimeHub;
	import xf.game.MainProcess;
	import xf.core.MessageHub;
	import xf.core.Message;
	
	static import xf.nucleus.support.impl.TextureImpl;
	import xf.nucleus.support.Texture;
	
	import xf.utils.Memory;
	
	import xf.utils.SimpleCamera;
	import xf.dog.Dog;
	import xf.dog.ext.VERSION_1_3;
	
	import tango.io.Stdout;
}



class MyChunkHandler : IChunkHandler {
	void	alloc(int cnt) {
		.alloc(data, cnt);
	}
	
	bool	loaded(int idx) {
		return data[idx].loaded;
	}
	
	void	load(int idx, Chunk*, ChunkData data) {
		with (this.data[idx]) {
			.alloc(positions, data.numPositions);
			data.getPositions(positions);
			.alloc(indices, data.numIndices);
			data.getIndices(indices);
			loaded = true;
		}
	}
	
	void	unload(int) {
		// TODO
	}
	
	void	free() {
		// TODO
	}


	struct UserData {
		vec3[]		positions;
		ushort[]	indices;
		bool			loaded;
	}
	
	UserData[]	data;
}



void main() {
	const int chunkSize = 16;
	
	auto ldr = new HeightmapChunkLoader;
	ldr.load("height.png", chunkSize);
	auto terrain = new ChunkedTerrain(ldr);
	auto chunkData = new MyChunkHandler;
	terrain.addChunkHandler(chunkData);
	
	terrain.scale = vec3(10.f, 3.f, 10.f);

	auto keyboard = new SimpleKeyboardReader(inputHub.mainChannel);
	
	bool wireframe	= false;
	bool drawSkirts	= true;
	bool drawTree	= false;

	class KeyboardReader : InputReader {
		void keyInput(KeyboardInput* i) {
			if (i.type.Down == i.type) switch (i.keySym) {
				case KeySym.q: wireframe ^= true; break;
				case KeySym.e: drawSkirts ^= true; break;
				case KeySym.r:	drawTree ^= true; break;
				default: break;
			}
		}
		this() {
			registerReader!(KeyboardInput)(&this.keyInput);
			inputHub.mainChannel.addReader(this);
		}
	}
	
	new KeyboardReader;

	auto wnd = GLWindow();
	wnd
		.title(`Chunked LoD Demo`)
		.showCursor(false)
		.interceptCursor(true)
		.fullscreen(false)
		.width(800)
		.height(600)
	.create();
	
	Texture albedo, lightmap, detail;
	use (wnd) in (GL gl) {
		auto mngr = TextureMngr(gl);
		albedo = mngr.load("albedo.png");
		lightmap = mngr.load("light.png");
		detail = mngr.load("detail.jpg");
	};
	
	auto cam = new SimpleCamera(vec3(0, 3, 0), -30, -135, inputHub.mainChannel);
	wnd.inputChannel = inputHub.mainChannel;

	jobHub.addRepeatableJob({
		wnd.update;

		if (keyboard.keyDown(KeySym.Escape)) {
			messageHub.sendMessage(new QuitMessage);
		}
		
		cam.update(1.f / timeHub.ticksPerSecond);
	}, timeHub.ticksPerSecond);
	
	void renderScene() {
		if (wnd.created) use (wnd) in (GL gl) {
			gl.Clear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
			gl.Enable(GL_DEPTH_TEST);

			gl.MatrixMode(GL_PROJECTION);
			gl.LoadIdentity();
			gl.gluPerspective(90.f, 1.3333f, 0.01f, 100.f);
			gl.MatrixMode(GL_MODELVIEW);

			auto modelview = cam.getMatrix;
			gl.LoadMatrixf(modelview.ptr);
			
			const float maxError = 0.25f;
			terrain.optimize(cam.position, maxError);
					
			int numDrawn = 0;
			void drawChunk(Chunk* ch, vec3 pos, float halfSize) {
				if (!ch.split) {
					if (drawTree) {
						vec3[4] chpos;
						pos.y += .375f;
						chpos[] = pos;
						float chSiz = halfSize;
						chpos[0].x -= chSiz;
						chpos[0].z -= chSiz;
						chpos[1].x += chSiz;
						chpos[1].z -= chSiz;
						chpos[2].x += chSiz;
						chpos[2].z += chSiz;
						chpos[3].x -= chSiz;
						chpos[3].z += chSiz;
						pos.y -= .375f;

						gl.Begin(GL_LINES);
						gl.Color3f(1, 1, 1);
							gl.Vertex3fv(chpos[0].ptr);
							gl.Vertex3fv(chpos[1].ptr);
							gl.Vertex3fv(chpos[1].ptr);
							gl.Vertex3fv(chpos[2].ptr);
							gl.Vertex3fv(chpos[2].ptr);
							gl.Vertex3fv(chpos[3].ptr);
							gl.Vertex3fv(chpos[3].ptr);
							gl.Vertex3fv(chpos[0].ptr);
						gl.End();
					}
					
					auto userData = chunkData.data[terrain.getIndex(ch)];
					if (userData.loaded && userData.positions && userData.indices) {
						++numDrawn;
						
						gl.EnableClientState(GL_VERTEX_ARRAY);
							gl.VertexPointer(3, GL_FLOAT, 0, userData.positions.ptr);
							int len = userData.indices.length;
							if (!drawSkirts) {
								len = chunkSize * chunkSize * 6;
							}
							gl.DrawElements(GL_TRIANGLES, len, GL_UNSIGNED_SHORT, userData.indices.ptr);
						gl.DisableClientState(GL_VERTEX_ARRAY);
					}
				} else {
					vec2[4] chpos;
					ch.getChildPositions(vec2(pos.x, pos.z), halfSize, &chpos);
					
					foreach (i, c; ch.children) {
						drawChunk(c, vec3(chpos[i].x, 0, chpos[i].y), halfSize * .5f);
					}
				}
			}
			
			gl.Enable(GL_CULL_FACE);
			gl.PolygonMode(GL_FRONT_AND_BACK, wireframe ? GL_LINE : GL_FILL);
			gl.Scalef(terrain.scale.tuple);
			
			void setupTexture(int texId, Texture tex, float repeat = 1.f) {
				gl.ActiveTexture(GL_TEXTURE0 + texId);
				gl.Enable(GL_TEXTURE_2D);
				gl.BindTexture(tex.glTarget, tex.id);
				gl.Enable(GL_TEXTURE_GEN_S);
				gl.Enable(GL_TEXTURE_GEN_T);
				gl.TexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
				gl.TexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
				gl.TexGenfv(GL_S, GL_OBJECT_PLANE, (vec4.unitX * repeat).ptr);
				gl.TexGenfv(GL_T, GL_OBJECT_PLANE, (-vec4.unitZ * repeat).ptr);
			}
			
			void disableTexture(int texId) {
				gl.ActiveTexture(GL_TEXTURE0 + texId);
				gl.Disable(GL_TEXTURE_2D);
				gl.Disable(GL_TEXTURE_GEN_S);
				gl.Disable(GL_TEXTURE_GEN_T);
				gl.ActiveTexture(GL_TEXTURE0);
			}
			
			gl.ext(VERSION_1_3) in {
				setupTexture(0, albedo);
				gl.TexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE);
				gl.TexEnvf(GL_TEXTURE_ENV, GL_COMBINE_RGB, GL_REPLACE);
				setupTexture(1, detail, 8.f);
				gl.TexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE);
				gl.TexEnvf(GL_TEXTURE_ENV, GL_COMBINE_RGB, GL_ADD_SIGNED);
				setupTexture(2, lightmap);
				gl.TexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE);
				gl.TexEnvf(GL_TEXTURE_ENV, GL_COMBINE_RGB, GL_MODULATE);
				gl.TexEnvf(GL_TEXTURE_ENV, GL_RGB_SCALE, 4.0f);
				
				drawChunk(terrain.root, vec3(.5f, 0.f, .5f), .5f);
				Stdout.formatln("rendered {} patches", numDrawn);
				
				disableTexture(0);
				disableTexture(1);
			};
		}; else {
			messageHub.sendMessage(new QuitMessage);
		}
		
		wnd.show;
	}
	
	jobHub.addPostFrameJob(&renderScene);
	jobHub.exec(new MainProcess);
}
