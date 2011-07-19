module xf.xpose.testb;

private {
	import xf.xpose.Serialization;

	import tango.io.Stdout;
	import tango.text.convert.Sprint;

	Sprint!(char) format;
	static this() {
		format = new Sprint!(char);
	}
}


interface IShape {
    float area();
    char[] toString();
}

class Shape {
    float area() { return 1.0f; }

    //pragma(msg, expose!(SerializationExpose)(`area`));
    mixin(expose!(SerializationExpose)(`area`));
}

//pragma(msg, expose_ext!(IShape,SerializationExpose)(`IShape.area`));
//mixin(expose_ext!(IShape,SerializationExpose)(`IShape.area`));


interface IPolygon : IShape {
    int n_vertices();
}

class Square : IPolygon
{
    float dx,dy;
    
    this() {}
    this(float w, float h) { dx = w; dy = h; }

    override float area() {
        return dx*dy;
    }
    override int n_vertices() {
        return 4;
    }

    char[] toString() {
		return format(`Square, dx='{}' dy='{}'`, dx, dy);
	}
	
	void writeDx(Serializer s) {
		s(cast(int)(dx * 8));
	}	
	void readDx(Unserializer s) {
		int tmp;
		s(tmp);
		dx = (1.f / 8) * tmp;
	}
	
	void onUnserialized() {
		Stdout.formatln("Square just got unserialized");
	}

	mixin(expose!(SerializationExpose)(`dx write=writeDx;read=readDx|dy`));

}

class Circle : IShape
{
    float radius;
    
    this() {}
    this(float r) { radius = r; }

    override float area() {
        return 3.14*radius*radius;
    }

    char[] toString() {
		return format(`Circle, radius='{}'`, radius);
	}

	mixin(expose!(SerializationExpose)(`radius`));

}

struct Shapes
{
    IShape[] shapes;

    char[] toString() {
        char[] ret = `[`;
        foreach(i,s; shapes) {
            ret ~= s.toString;
            ret ~= (i==shapes.length-1)? `]` : `, `;
        }
        return ret;
    }
}
struct ShapesExport {
	mixin(expose!(SerializationExpose)(`Shapes`, `shapes|virt read=vread; write=vwrite`));
	
	static void vread(Shapes* obj, Unserializer s) {
		Stdout.formatln("virtual read");
	}	
	static void vwrite(Shapes* obj, Serializer s) {
		Stdout.formatln("virtual write");
	}

	void onUnserialized(Shapes* sh) {
		Stdout.formatln("Shapes just got unserialized");
	}
}



void main() {
    char[] stringified;
	{
        Shapes shapes;
        shapes.shapes ~= new Square(0.5,2.0);
        shapes.shapes ~= new Circle(1.0);
		
		Stdout.formatln("Before: {}", shapes);
        stringified = shapes.toString;

		// serialize -----------------------------------------------------------------------
		(new Serializer(`iface.dat`))(shapes).close;
	}

	{
		// unserialize ---------------------------------------------------------------------
		scope auto u = new Unserializer(`iface.dat`);
        auto shapes = u.get!(Shapes);
		Stdout.formatln("After:  {}", shapes);
        assert(shapes.toString == stringified);
	}

	Stdout.formatln("Terminating test");
}
