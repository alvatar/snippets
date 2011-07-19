module xf.loader.bsp.internal.shader.Types;
import tango.text.convert.Sprint;

/// Struct dedicated to hold tcModCommand
struct TCMod{
	short[] type; ///
	float[] params; ////
}

/// Types TCMod can have
enum TCModType{
	Unknown=0, /// unknown type will just be skipped over
	Scroll, /// scrolling the texture in given direction
	Turb /// making random turbulence to a motion
}

/// Enym holding parameters of blending
enum BlendMode{
	ONE=0,///
	DSTCOLOR,/// 
	SRCALPHA,///
	ZERO///
}

enum SurfaceParam {
	NodLight,
	NoLightmap,
	Trans,
	NoImpact,
	NonSolid,
	Unknown
}

///class holding sub-shader commands
final class SubShader(Texture){
	package Texture _map;
	package Texture _clampmap;
	TCMod[] tcMod;
	short[] blendFunc;
	char[][] rgbGen;
	char[][] animMapSprites;
	int animMapFrequency; 
	char[][]  alphaFunc;
	bool depthWrite=false;
	char[][] depthFunc;
	
	package Texture delegate (Texture lightmap)  _getClampmap;
	package Texture delegate (Texture lightmap)  _getMap;
	
	package Texture getLoadedClampmap(Texture lightmap) {
		return _clampmap;
	}
	
	package Texture getLoadedMap(Texture lightmap){
		return _map;
	}
	
	package Texture getWhiteTexture(Texture lightmap) {
		assert(false, "toDo");
		return Texture.init;
	}
	
	package Texture returnLightmap(Texture lightmap) {
		return lightmap;
	}
	
	Texture clampmap(Texture lightmap) {
		return _getClampmap(lightmap);
	}
	
	Texture map(Texture lightmap) {
		return _getMap(lightmap);
	}
	
	char[] toString(){
		scope sprint = new Sprint!(char);
		char[] ret;
		ret ~= "	{\n";
		ret ~= sprint("		map: {}\n", _map);
		ret ~= sprint("		clampmap: {}\n", _clampmap);
		ret ~= "		tcMod: \n";
		foreach(mod;tcMod){
			ret ~=  sprint ("			{} {} \n",mod.type, mod.params);
		}
		ret ~=  sprint("		blendFunc: {} \n",blendFunc);
		ret ~=  sprint("		rgbGen: {}\n",rgbGen);
		ret ~=  sprint("		animMap: {}\n", animMapSprites);
		ret ~=  sprint("		alphaFunc: {} \n",alphaFunc);
		ret ~=  sprint("		depthWrite: {}\n",depthWrite);
		ret ~= sprint("		depthFunc: {}\n",depthFunc);
		ret ~= "	}\n";
		return ret;
	}
}

class Q3Shader(Texture) {
	char[] name;
	SubShader!(Texture)[] subshaders;
	package SurfaceParam[] surfaceParams;
	package char[][char[]] params;
	
	bool hasSurfaceParam(SurfaceParam param){
		foreach(_param; surfaceParams){
			if( _param == param) return true;
		}
		return false;
	}
	
	char[] param(char[] key){
		if(key in params) return params[key];
		return null;
	}
	
	char[] toString(){
		char[] ret;
		ret ~= name;
		ret ~= "\n{\n";
		foreach(shader;subshaders){
			ret ~= shader.toString;
		}
		ret ~= "}\n";
		return ret;
	}
}
