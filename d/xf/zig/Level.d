/**
	For the moment being it reads Zap levels in a very wrong way. Just for testing purposes. Replace with own levels.
*/

module xf.zig.Level;

private {
	import xf.zig.Renderer;
	import xf.zig.Phys;

	import xf.dog.OpenGL;
	import xf.dog.GLWrap;
	
	import tango.io.device.FileConduit;
	import tango.text.stream.LineIterator;
	import tango.text.Util;
	import Float = tango.text.convert.Float;

	import xf.maths.Vec;

	extern (C) int printf(char* format, ...);
}



class Barrier {
	vec4		color = {r: 0, g: 0, b: 1, a: 1};
	vec2[]	points;
}



class Level : Renderable {
	this (char[] path) {
		scope file = new FileConduit(path);
		scope(exit) { file.close; }
		
		foreach (i, line; new LineIterator!(char)(file)) {
			parseLine(line.trim());
		}

		renderer.register(this);
	}
	

	override void render(GL gl) {
		gl.withState(GL_LINE_SMOOTH, GL_BLEND) in {
			gl.BlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
			gl.LineWidth(4.f);
			
			foreach (b; barriers) {
				gl.immediate(GL_LINE_STRIP, {
					gl.Color4fv(&b.color.x);
					
					foreach (p; b.points) {
						gl.Vertex2fv(&p.x);
					}
				});
			}
		};
	}

	
	private {
		void parseLine(char[] line) {
			void ifStartsWith(char[] text, void delegate(char[]) dg) {
				if (line.length >= text.length && line[0..text.length] == text) {
					dg(line[text.length .. $]);
				}
			}
			
			ifStartsWith(`BarrierMaker`, (char[] text) {
				//printf(`parsing a barrier`\n);
				
				text = text.trim();
				char[][] parts = text.split(" ")[1..$];
				
				auto b = new Barrier;
				for (int i = 0; i < parts.length; i += 2) {
					float f0 = scaleFactor * Float.parse(parts[i]);
					float f1 = scaleFactor * Float.parse(parts[i+1]);
					b.points ~= vec2(f0, f1);
					//printf(`adding a point`\n);
				}
				
				{
					int p0 = 0;
					for (int p1 = 1; p1 < b.points.length; p0 = p1++) {
						phys.addStaticLineSegment(b.points[p0], b.points[p1], .8f);
					}
				}
				
				this.barriers ~= b;
			});
		}
	}
	
	
	const float	scaleFactor = 30.f;
	Barrier[]	barriers;
}
