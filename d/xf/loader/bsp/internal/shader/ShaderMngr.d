module xf.loader.bsp.internal.shader.ShaderMngr;
import xf.loader.bsp.internal.shader.Types;
import xf.loader.bsp.internal.Logging;
import xf.utils.Callback;
import xf.utils.String;
import tango.io.device.FileConduit;
import tango.text.stream.LineIterator;
import tango.text.Util;
import tango.text.Ascii;
import tango.util.Convert;
import xf.loader.scene.Hme;

alias xf.utils.String.count count;

final class ShaderManager(Texture) {
	Q3Shader!(Texture)[] shaders;
	private alias Callback!(Texture, char[] ) TextureCallback;
	private TextureCallback _textureCallback;
	
	mixin(TextureCallback.genSetters("textureCallback", "_textureCallback"));
	
	Q3Shader!(Texture) getShader(char[] name){
		foreach(shader; shaders) {
			if(name == shader.name) return shader;
		}
		return null;
	}
	
	void load(char[] path){
		bspLogger.info("Loading shader file: {}", path);
		if( ! _textureCallback.isSet ) bspLogger.warn("No texture callback was loaded, the textures will not be loaded");
		
		bool inSubshader=false;
		bool inShader=false;
		
		auto file=new FileConduit(path);		
		
		Q3Shader!(Texture) workingShader;
		SubShader!(Texture) workingSubShader;
		
		int tmp;
		int framesToLoad=0;
		bool lineParsed;
		
		scope lines = new LineIterator!(char)(file);
		
		foreach(lineNum,  line ; lines) {
			
			if(line.length == 0) continue;
			
			if(framesToLoad>0){
				workingSubShader.animMapSprites~=line.dup;
				framesToLoad--;
				continue;
			}
			
			
			if(count(line,"//")>0) continue; // commented line
			
			
			
			if((tmp=count(line,"{"))>0){
				if(!inShader) inShader=true;
				else if(!inSubshader) {
					inSubshader=true;
					workingSubShader=new SubShader!(Texture)();
					bspLogger.debug2("Creating subshader").tab;
				}
				else bspLogger.error("third \"{{\" sign while parsing {} in line {}", path, lineNum);
				if(tmp>1) bspLogger.error("more than 1 \"{{\" in single line in line while parsing {} in line:{} ", path , lineNum);
				line=removechar(line,"{");
				//continue;
			}else if((tmp=count(line,"}"))>0){
				if(inSubshader) {
					inSubshader=false;
					//workingShader.subShaders~=workingSubShader;
					//workingSubShader.write();
					workingShader.subshaders.length=workingShader.subshaders.length+1;
					workingShader.subshaders[workingShader.subshaders.length-1]=workingSubShader;
					bspLogger.utab.debug2("Subshader created");
					framesToLoad=0;
				}
				else if(inShader) {
					shaders~=workingShader;
					inShader=false;
				}
				else bspLogger.error("not coupled \"}}\" sign while parsing {} in line: {}", path, lineNum);
				if(tmp>1) bspLogger.error("more than 1 \"}}\" in single line in line while parsing {} in line: ", path, lineNum);
				line=removechar(line,"}");
				//continue;				
			}
			//if(line.length<2) continue; // Probably a blank line
			
			//bspLogger.debug4("{}: {} {} {}", lineNum, inShader, inSubshader, line);
			
			line=toLower(line);
			line=triml(line);
			
			if(line.length==0) continue;
			
			if (inSubshader){
				char[][] fields=split(line," ");
				lineParsed=true;
				switch(fields[0]){
					case "map": { 
						if(fields[1]=="$lightmap") {
							workingSubShader._getMap=&workingSubShader.returnLightmap;
							break;
						}
						if(fields[0]=="$whiteimage"){
							workingSubShader._getMap=&workingSubShader.getWhiteTexture;
							break;
						}
						if( _textureCallback.isSet) {
						
							auto texture = _textureCallback(fields[1].dup);
							if(texture is Texture.init){
								bspLogger.error("Couldn't load map texture {}", fields[1]);
								workingSubShader._getMap=&workingSubShader.getWhiteTexture;
							} else {
								bspLogger.info("Loaded map texture {}", fields[1]);
								workingSubShader._getMap = &workingSubShader.getLoadedMap;
								workingSubShader._map = texture;
							}
							
							break;
						}
						break;
					}
					case "blendfunc": foreach(field;fields[1..$]){
						switch(field){
							case "gl_one": workingSubShader.blendFunc~=BlendMode.ONE; break;
							case "gl_dst_color": workingSubShader.blendFunc~=BlendMode.DSTCOLOR; break;
							case "gl_zero": workingSubShader.blendFunc~=BlendMode.ZERO; break;
							case "gl_src_alpha": workingSubShader.blendFunc~=BlendMode.SRCALPHA; break;
							default : 	bspLogger.error("Unknown blendFunc {} while parsing {} in line: {} , setting as BlendMode.ONE", field, path, lineNum); 
											workingSubShader.blendFunc~=BlendMode.ONE; break;
						}
					} break;
					case "tcmod":{
						TCMod tmp;
						foreach(field;fields[1..$]){
							if(isNumeric(field)){
								tmp.params~=to!(float)(field);
							} else {
								switch(field){
									case "turb": tmp.type~=TCModType.Turb; break;
									case "scroll": tmp.type~=TCModType.Scroll; break;
									default: 	bspLogger.warn("Unknown tcmod {} while parsing {} in line {}, setting as TCModType.Uknown and will be ignored", field, path, lineNum); 
													tmp.type~=TCModType.Unknown; break;
								}
							}
						}
						workingSubShader.tcMod~=tmp;
						break;
					}
					case "rgbgen": {
						workingSubShader.rgbGen~=fields[1..$].dup;
						break;
					}
					case "depthwrite" :{
						workingSubShader.depthWrite=true;
						break;
					}
					case "alphafunc" : workingSubShader.alphaFunc~=fields[1..$].dup; break;
					case "animmap": {
						workingSubShader.animMapFrequency=to!(int)(fields[1]);
						if(fields.length>2){
							workingSubShader.animMapSprites~=fields[2].dup;
						}
						break;
					}
					case "clampmap":{
						if(fields[1]=="$lightmap") {
							workingSubShader._getMap=&workingSubShader.returnLightmap;
							break;
						}
						if(fields[0]=="$whiteimage"){
							workingSubShader._getMap=&workingSubShader.getWhiteTexture;
							break;
						}
						if( _textureCallback.isSet) {
						
							auto texture = _textureCallback(fields[1].dup);
							if(texture is Texture.init){
								bspLogger.error("Couldn't load clampmap texture {}", fields[1]);
								workingSubShader._getMap=&workingSubShader.getWhiteTexture;
							} else {
								bspLogger.info("Loaded map clampmap texture {}", fields[1]);
								workingSubShader._getMap = &workingSubShader.getLoadedMap;
								workingSubShader._clampmap = texture;
							}
							
							break;
						}
						break;
					}
					default: lineParsed=false; if(framesToLoad) workingSubShader.animMapSprites~=fields[0].dup; 
						else bspLogger.warn("Unparsable line in file {} line: {}", path, lineNum); break;
				}
				if(framesToLoad) if(lineParsed) framesToLoad=false;
			} else if (inShader){
				char[][] fields=split(line," ");
				//bspLogger.fatal(line);
				lineParsed=true;
				if(fields[0]=="surfaceparm"){
					switch(fields[1]){
						case "nodlight": workingShader.surfaceParams ~= SurfaceParam.NodLight; break;
						case "nolightmap": workingShader.surfaceParams ~= SurfaceParam.NoLightmap; break;
						case "trans": workingShader.surfaceParams ~= SurfaceParam.Trans; break;
						case "noimpact": workingShader.surfaceParams ~= SurfaceParam.NoImpact; break;
						case "nonsolid": workingShader.surfaceParams ~= SurfaceParam.NonSolid; break;
						default: bspLogger.warn("Uknown surfaceparam {} in file {} in line {}", fields[1], path, lineNum); break;
					}
				} else {
					workingShader.params[fields[0].dup] = join(fields[1..$]).dup;
				}
				continue; 
			} else {
				if(workingShader !is null) {
					bspLogger.utab.debug1("Q3Shader created ({})", workingShader.name);
					//bspLogger.debug4( "{}", workingShader);
				}
				
				workingShader=new Q3Shader!(Texture)();
				workingShader.name=line.dup;
				bspLogger.debug1("Creating q3shader {}", workingShader.name).tab;
			}
			
		}
		bspLogger.utab;
		
		file.close();
	}
}