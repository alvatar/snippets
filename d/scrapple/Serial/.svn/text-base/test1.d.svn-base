module test1;

import std.stdio;
import std.string;

import Serialize;
import StringSourceSink;
import Third;

class CS
{
	mixin SerializableRecurring!();
	int i = 5;	float j = 3.1415926;
	this(){}	this(int) { i++; j++; }
	char[] toString() { return format("<%s,%s>", this.tupleof); }
}

class DS : CS
{
	mixin SerializableRecurring!();
	real f = 2.717;
	this(){}	this(int) { f++; super(0); }
	char[] toString() { return format("<%s,%s>", super.toString()[1..$-1], this.tupleof); }
}

/// show arrays

struct TS
{
	mixin Serializable!();
	int ti;
	char[] toString() { return format("<%s>", this.tupleof); }
}

struct SS
{
	mixin Serializable!();
	int foo; float bar; CS cs; char[] someString; int[][] data; TS[] tdata;
	char[] toString() { return format("<%s,%s,%s,%s,%s,%s>", this.tupleof); }
}

/// show aliased references

struct SC
{
	mixin Serializable!();
	CS a; CS b;
	char[] toString() { return format("<%s,%s>", this.tupleof); }
}

/// Show 3rd party types

struct TPS
{
	int i = 2; int j = 3; char[] str = "five";
	char[] toString() { return format("<%s,%s,%s>", this.tupleof);}
}

void TPSm(TPS v, SinkHandle h)
{
	Push!("i", int)(v.i, h);	Push!("j", int)(v.j, h);	Push!("str", char[])(v.str, h);
}
TPS TPSd(SourceHandle h)
{
	TPS ret;
	ret.i = Pull!("i", int)(h);	ret.j = Pull!("j", int)(h);	ret.str = Pull!("str", char[])(h);
	return ret;
}
mixin WorkWith!(TPS, TPSm, TPSd);

struct ST
{
	mixin Serializable!();
	char[] name = "some text to test";	TPS a = TPS(3, 5, "eight");
	char[] toString() { return format("<%s,%s>", this.tupleof); }
}

void main()
{
	SS ss = SS(42, 2.717, null, "<weee!'\"&>", [[cast(int)6,9,42][],[cast(int)6,9,54]], [TS(3),TS(1)]);
	SC sc;
	Si c;
	ST st;

	ss.cs = null;		c = new Si();
	ss.Serialize(c);	writef("%s\n\t%s\n", c.get, SS.Deserialize(new So(c.get)));
	
	ss.cs = new CS(1);	c = new Si();
	ss.Serialize(c);	writef("%s\n\t%s\n", c.get, SS.Deserialize(new So(c.get)));
	
	ss.cs = new DS(1);	c = new Si();
	ss.Serialize(c);	writef("%s\n\t%s\n", c.get, SS.Deserialize(new So(c.get)));

	c = new Si();	
	(new DS(1)).Serialize(c);	writef("%s\n\t%s\n", c.get, CS.Deserialize(new So(c.get)));

	st.name = "somthing to expect";	c = new Si();
	st.Serialize(c); writef("%s\n\t%s\n", c.get, ST.Deserialize(new So(c.get)));

	sc.a = new CS(1);	sc.b = new CS(1);	c = new Si();	sc.Serialize(c);
	auto same = SC.Deserialize(new So(c.get));
	writef("%s\n\t%s\n", c.get, same);
	assert(same.a !is same.b);
	
	sc.b = sc.a;	c = new Si();	sc.Serialize(c);
	auto dif = SC.Deserialize(new So(c.get));
	writef("%s\n\t%s\n", c.get, dif);
	assert(dif.a is dif.b);
}