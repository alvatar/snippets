module xf.loader.bsp.BSP;

public import xf.loader.bsp.internal.Structs;
import xf.loader.bsp.internal.Logging;
import xf.loader.bsp.internal.Lumps;
import xf.utils.Memory;

import tango.io.device.FileConduit;
import xf.loader.bsp.internal.shader.ShaderMngr;
public import xf.loader.bsp.internal.shader.Types;

import xf.utils.Callback;
import tango.io.FileScan;
import tango.stdc.stringz;

final class BSPLevel (Texture) {
	int[]	indices;
	BSPVertex[] vertices;
	BSPFace[] faces;
	BSPTexture[] textureInfo;
	BSPLightmap[] lightmapInfo;
	Texture[] lightmaps;
	BSPMaterial!(Texture)[] materials;
	BSPHeader header;
	BSPLump [Lumps.MaxLumps] lumps;
	ShaderManager!(Texture) shaders;
	
	private {
		alias Callback!(Texture, char[] , int, int) TextureCallback;
		alias Callback!(Texture, void[] , int, int, int) LightmapCallback;
		alias Callback!(void, BSPFace, BSPMaterial!(Texture), Texture) RenderCallback;
	}
		
	BSPNode[] nodes;
	BSPLeaf[] leafs;
	BSPPlane[] planes;
	int[] leafFaces;
	BSPVisData clusters;
	
	int opApply (int delegate(ref BSPFace, ref BSPMaterial!(Texture)) dg) {
		foreach(face; faces){
			if (auto r = dg(face, materials[face.textureID] )) return r;
		}
		
		return 0;
	}
	
	this() {
		shaders = new ShaderManager!(Texture);
	}
	
	
	void loadShaders(char[] dir){
		auto scan = (new FileScan)(dir, ".shader");
		foreach(file; scan.files){
			shaders.load(file.toString);
		}

	}
	
	bool loadMap(char[] path){
		//try{
			parse(path);
			bspLogger.info("loaded");
			return true;
		/+} catch (Exception e){
			bspLogger.error("Error while loading, Error: {} ", e );
			return false;
		}+/
	}
	
	mixin(TextureCallback.genSetters("textureCallback", "_textureCallback"));
	//pragma(msg, TextureCallback.genSetters("textureCallback", "_textureCallback"));
	mixin(LightmapCallback.genSetters("lightmapCallback", "_lightmapCallback"));
	mixin(RenderCallback.genSetters("renderFaceCallback", "_renderFace"));
	mixin(RenderCallback.genSetters("renderMeshCallback", "_renderMesh"));
	mixin(RenderCallback.genSetters("renderPatchCallback", "_renderPatch"));
	mixin(RenderCallback.genSetters("renderBilboardCallback", "_renderBilboard"));
	
	void shaderTextureCallback( Texture delegate(char[]) cb){
		shaders.textureCallback(cb);
	}
	
	void shaderTextureCallback( Texture function(char[]) cb){
		shaders.textureCallback(cb);
	}
	
	void render() {
		
		foreach(face; faces){
			switch(face.type){
				case 1: if(_renderFace.isSet) 
								_renderFace(face, 
									materials[face.textureID],  
									(face.lightmapID<lightmaps.length) ?
										lightmaps[face.lightmapID]:
										Texture.init
							); break;
				case 2: if(_renderPatch.isSet) 
								_renderPatch(face, 
									materials[face.textureID],  
									(face.lightmapID<lightmaps.length) ?
										lightmaps[face.lightmapID]:
										Texture.init
							); break;
				case 3: if(_renderMesh.isSet) 
								_renderMesh(face, 
									materials[face.textureID],  
									(face.lightmapID<lightmaps.length) ?
										lightmaps[face.lightmapID]:
										Texture.init
							); break;
				case 4: if(_renderBilboard.isSet) 
								_renderBilboard(face, 
									materials[face.textureID],  
									(face.lightmapID<lightmaps.length) ?
										lightmaps[face.lightmapID]:
										Texture.init
							); break;
				default: break;
			}
		}
	} 
	
	private TextureCallback _textureCallback;
	private LightmapCallback _lightmapCallback;
	private RenderCallback _renderFace;
	private RenderCallback _renderMesh;
	private RenderCallback _renderBilboard;
	private RenderCallback _renderPatch;
	
	private void parse(char[] path){
		bspLogger.info("Loading map: {}  ...", path);
		auto file = new FileConduit(path);
		loadHeader(file);
		loadLumps(file);
		loadEntities(file);
		loadVertices(file);
		loadIndices(file);
		loadFaces(file);
		
		
		loadTextures(file);
		loadLightmaps(file);
		
	}
	
	private void loadLightmaps(FileConduit file) {
		lightmapInfo.alloc(lumps[Lumps.Lightmaps].length/BSPLightmap.sizeof);
		lightmaps = new Texture [lumps[Lumps.Lightmaps].length/BSPLightmap.sizeof];
		file.seek(lumps[Lumps.Lightmaps].offset);
		readExact(file, lightmapInfo.ptr, lightmapInfo.length * BSPLightmap.sizeof);
		bspLogger.debug2("Info about lightmaps loaded").tab;
		if(!_lightmapCallback.isSet){
			bspLogger.debug3("No callback set for lightmaps.").utab;
			return;
		} else {
			bspLogger.debug3("Invoking callback for each lightmap:"). tab;
			foreach(index, lightmap; lightmapInfo) {
				ubyte *pImage=cast(ubyte*)lightmap.bits.ptr;
				for(int i = 0; i <  128*128; i++, pImage += 3){
					float scale = 1.0f, temp = 0.0f;
					float r = 0, g = 0, b = 0;
					float factor=1.8; // GAMMA FACTOR
					r = cast(float)pImage[0];
					g = cast(float)pImage[1];
					b = cast(float)pImage[2];
					r = r * factor / 255.0f;
					g = g * factor / 255.0f;
					b = b * factor / 255.0f;
					if(r > 1.0f && (temp = (1.0f/r)) < scale) scale=temp;
					if(g > 1.0f && (temp = (1.0f/g)) < scale) scale=temp;
					if(b > 1.0f && (temp = (1.0f/b)) < scale) scale=temp;
					scale*=255.0f;		
					r*=scale;	
					g*=scale;	
					b*=scale;
					pImage[0] = cast(byte)r;
					pImage[1] = cast(byte)g;
					pImage[2] = cast(byte)b;
				}
				/+void[] data;
				data.alloc(BSPLightmap.sizeof);
				data[] = (cast(void[])lightmap.bits)[];+/
				lightmaps[index]=_lightmapCallback(lightmap.bits,128,128, 1);
				bspLogger.debug4(" lightmap: {} / {}  -> {} ", index, lightmaps.length , lightmaps[index]);
			}
			bspLogger.debug3("finished.").utab.utab;
		}
		
	}
	
	private void loadTextures(FileConduit file){
		textureInfo.alloc(lumps[Lumps.Textures].length/BSPTexture.sizeof);
		//textures = new Texture[lumps[Lumps.Textures].length/BSPTexture.sizeof];
		materials = new BSPMaterial!(Texture)[lumps[Lumps.Textures].length/BSPTexture.sizeof];
		file.seek(lumps[Lumps.Textures].offset);
		readExact(file, textureInfo.ptr, textureInfo.length * BSPTexture.sizeof);
		bspLogger.debug2("Info about textures loaded").tab;
		
		if(!_textureCallback.isSet) {
			bspLogger.debug3("No callback set for textures.").utab;
			return;
		} else {
			bspLogger.debug3("Invoking Callback for each texture:").tab;
			foreach(index, texture; textureInfo){
				auto tmpname = fromStringz (texture.name.ptr);
				bspLogger.debug4("for {}",cast(char[]) tmpname);
				auto shader = shaders.getShader(tmpname);
				if(shader !is null) {
					materials[index].shader = shader;
					bspLogger.debug4("shader found");
					continue;
				}
				bspLogger.debug4("shader not found, looking for texture");
				auto texture2=_textureCallback(tmpname , texture.flags, texture.type);
				if(texture2 is Texture.init){
					bspLogger.warn("neither shader nor texture was found for {}", tmpname);
				}
				materials[index].texture=texture2;
			}
			bspLogger.debug3("finished.").utab.utab;
		}
		
	}
	
	private void loadHeader( FileConduit file){
		readExact(file, &header , header.sizeof);
		bspLogger.debug1("File header:").tab.debug1("{}", header).utab;
	}
	
	private void loadLumps( FileConduit file){
		readExact(file, lumps.ptr ,BSPLump.sizeof*lumps.length);
		bspLogger.debug2("Lumps loaded");
	}
	
	private void loadEntities( FileConduit file){	
		char[] entitis;
		entitis.alloc(lumps[Lumps.Entities].length);
		file.seek(lumps[Lumps.Entities].offset);
		readExact(file, entitis.ptr, char.sizeof*entitis.length);
		bspLogger.debug2("Entities loaded");
		entitis.free();
	}
	
	private void loadVertices( FileConduit file){	
		vertices.alloc(lumps[Lumps.Vertices].length/BSPVertex.sizeof);
		file.seek(lumps[Lumps.Vertices].offset);
		readExact(file, vertices.ptr, BSPVertex.sizeof * vertices.length);
		bspLogger.debug2("Vertices loaded").tab;
		bspLogger.debug3("Vertices number: {}", vertices.length);
		foreach(inout vertex; vertices){
			swizzle(vertex.position.y, vertex.position.z);
			swizzle(vertex.normal.y, vertex.normal.z);
		}
		
		bspLogger.debug3("Vertices swizzled").utab;
	}
	
	private void loadIndices( FileConduit file){	
		indices.alloc(lumps[Lumps.Indices].length/int.sizeof);
		file.seek(lumps[Lumps.Indices].offset);
		readExact(file, indices.ptr, int.sizeof * indices.length);
		bspLogger.debug2("Indices loaded");
	}
	
	private void loadFaces( FileConduit file){	
		faces.alloc(lumps[Lumps.Faces].length / BSPFace.sizeof);
		file.seek(lumps[Lumps.Faces].offset);
		readExact(file, faces.ptr, BSPFace.sizeof * faces.length);
		bspLogger.debug2("Faces loaded").tab;
		bspLogger.debug3("Faces number: {}", faces.length);
		foreach(inout face; faces){
			swizzle(face.normal.y, face.normal.z);
			swizzle(face.imapVectors[0].y, face.imapVectors[0].z);
			swizzle(face.imapVectors[1].y, face.imapVectors[1].z);
		}
		bspLogger.debug3("Data swizzled").utab;
	}
	
	private void swizzle(T)( inout T a, inout T b){
		T tmp = a;
		a= b;
		b= - tmp;
	}
	
	private uint readExact(FileConduit file,  void* ptr, uint length){
		return file.read((cast(ubyte*)ptr)[0..length]);
	}
	
	
}