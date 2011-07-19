module xf.rt.Misc;

private {
	import xf.omg.core.LinearAlgebra : vec3, vec3ub, vec2i;
	import xf.omg.core.CoordSys : CoordSys;
	import tango.io.device.File : FileConduit = File;
	import tango.io.stream.DataFile: DataFileOutput;
}



void writeTGA(vec3ub delegate(int x, int y) data, vec2i origin, vec2i size, char[] filename) {
	auto fc = new FileConduit(filename, FileConduit.WriteCreate);
	
	auto output = new DataFileOutput(fc);
	
	
	output.int8(0);
	output.int8(0);
	output.int8(2);
	output.int16(0);
	output.int16(0);
	output.int8(0);
	
	output.int16(0);
	output.int16(0);
	output.int16(size.x);
	output.int16(size.y);
	output.int8(24);
	output.int8(0);
	
	int y = size.y + origin.y;
	while (y--) {
		for (uint x = origin.x; x < origin.x + size.x; ++x) {
			vec3ub col = data(x, y);
			
			output.int8(col.b);
			output.int8(col.g);
			output.int8(col.r);
		}
	}
	
	output.flush();
	fc.close();
}
