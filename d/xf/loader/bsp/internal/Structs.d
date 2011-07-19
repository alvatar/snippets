module xf.loader.bsp.internal.Structs;
import xf.omg.core.LinearAlgebra;
import tango.util.Convert;
import xf.loader.bsp.internal.shader.Types;

struct BSPHeader{
	char[4] strID=[0,0,0,0];		/// This should always be 'IBSP'
    int v=0;						/// This should be 0x2e for Quake 3 files
    
    char[] toString(){
    	return "ID: "~strID~" v:"~to!(char[])(v);
    }
}

struct BSPMaterial(Texture) {
	
	void texture(Texture tex){
		_texture=tex;
		state = MaterialState.Texture;
	}
	
	void shader(Q3Shader!(Texture) sh){
		_shader = sh;
		state = MaterialState.Shader;
	}
	
	bool isSet(){
		return state != MaterialState.NotSet;
	}
	
	bool isTexture(){
		return state == MaterialState.Texture;
	}
	
	bool isShader(){
		return state == MaterialState.Shader;
	}
	
	Texture texture() {
		assert(isTexture);
		return _texture;
	}
	
	Q3Shader!(Texture) shader() {
		assert(isShader);
		return _shader;
	}
	
	private union {
		Texture _texture;
		Q3Shader!(Texture) _shader;
	}
	
	private enum MaterialState {
		Texture,
		Shader,
		NotSet
	}
	
	private MaterialState state = MaterialState.NotSet;
	
	
}

struct BSPVertex {
	vec3 position;
	vec2 textureCoords;
	vec2 lightmapCoords;
	vec3 normal;
	vec4ub color;
	
	BSPVertex opAdd(BSPVertex rhs){
		BSPVertex result;
		result.position = position + rhs.position;
		result.textureCoords = textureCoords + rhs.textureCoords; 
		result.lightmapCoords= lightmapCoords + rhs.lightmapCoords;
		result.color = color;
		result.normal = normal;
		return result;
	}
	
	BSPVertex opMul(float rhs){
		BSPVertex result;
		result.position = position * rhs;
		result.textureCoords = textureCoords *rhs;
		result.lightmapCoords = lightmapCoords * rhs;
		result.color = color;
		result.normal = normal;
		return result;
	}
}

struct BSPTexture {
	char[64] name;
	int flags;
	int type; 
}

struct BSPLightmap {
	vec3ub[128][128]  bits;
}

struct BSPNode {
	int plane;
	int front;
	int back;
	vec3i min;
	vec3i max;
}

struct BSPLeaf {
	int cluster;
	int area;
	vec3i min;
	vec3i max;
	int face;
	int faceNumber;
	int brush;
	int brushNumber;
}

struct BSPPlane {
	vec3 normal;
	float d;
}

struct BSPVisData{
	int vectorNubers;
	int vectorSize;
	byte[] vectors;
}

struct BSPBrush {
	int side;
	int sidesNumber;
	int textureID;
}

struct BSPBrushSide {
	int plane;
	int textureID;
}

struct BSPModel {
	vec3 min;
	vec3 max;
	int face;
	int faceNumber;
	int brush;
	int brushNumber;
}

struct BSPShader {
	char[64] name;
	int brush;
	int unknown;
}

struct BSPLight {
	vec3ub ambient;
	vec3ub directional;
	vec2ub direction;
}

struct BSPFace{
    int textureID;        /// The index into the texture array 
    int effect;           /// The index for the effects (or -1 = n/a) 
    int type;             /// 1=polygon, 2=patch, 3=mesh, 4=billboard 
    int startVertIndex;   /// The index into this face's first vertex 
    int verticesNumber;       /// The number of vertices for this face 
    int startIndex;       /// The index into the indices array
    int indicesNumber;     /// The number of level indices 
    int lightmapID;       /// The texture index for the lightmap 
    vec2i imapCorner;    /// The face's lightmap corner in the image 
    vec2i imapSize;      /// The size of the lightmap section 
    vec3 imapPosition;     /// The 3D origin of lightmap. 
    vec3[2] imapVectors; /// The 3D space for s and t unit vectors. 
    vec3 normal;     /// The face normal. 
    vec2i size;          /// The bezier patch dimensions. 
}