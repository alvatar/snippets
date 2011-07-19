module Marshal;

import std.string;

import Serialize;
import utill;
import Third;
import XML;
public import State;

char[] TrimType(char[] name)
{
	foreach_reverse(i,c; name) if(c == '.') return name[i+1..$];
	return name;
}

void MarshalNative(T)(Sink!(char) sink, T t, ref InMap map)
{
	static if(is(T == char[]))
	{
		XML.WriteContentXML(sink,t);
	}
	else static if(is(T B == B[]))
	{
		if(t.length == 0)
		{
			sink.Dump("null");
		}
		else
		{
			static if(NativeBaseType!(B))
			{
				MarshalNative!(B)(sink,t[0],map);
				foreach(b;t[1..$])
				{
					sink.Dump(",");
					MarshalNative!(B)(sink,b,map);
				}
			}
			else
			{
				const static char[] tag = NameFromType!(B);
				foreach(b;t)
				{
					M!(tag,B)(sink,b,map);
				}
			}
		}
	}
	else static if(NativeBaseType!(T))
		sink.Dump(std.string.format("%s", t));
	else static assert(false, "Don't known how to Marshal "~T.stringof);
}

template MarshalMixin()
{
	void Serialize(Sink!(char) sink)
	{
		InMap map;
		static const char[] name = "root";
		M!(name,typeof(this))(sink,this,map);
	}

	void MarshalMe(Sink!(char) sink, ref InMap map)
	{
		//writef("Doing %s\n", typeof(this).stringof);
		mixin utill.Usefull!(typeof(this));

		static if(is(typeof(this) == class))
		{
			static if(is(typeof(super.MarshalMe(sink,map))))
				super.MarshalMe(sink,map);
		}

		foreach(i,v;this.tupleof)
		{
			static const name = TrimType(BaseType.tupleof[i].stringof);
			M!(name,typeof(v))(sink,v,map);
		}
	}
}

void M(char[] name, V)(Sink!(char)sink, V v, ref InMap map)
{
	static if(is(typeof(&v.MarshalMe)))
	{
		static if(is(V == class))
		{
			if(v is null)
			{
				sink.Dump("<"~name~">NULL</"~name~">");
			}
			else
			{
				sink.Dump("<"~name);
				if(v.TypeTag(sink, map)) return;
				sink.Dump("\">");
				v.MarshalMe(sink,map);
				sink.Dump("</"~name~">");
			}
		}
		else static if(is(utill.Usefull!(V).BaseType == struct))
		{
			sink.Dump("<"~name~">");
			v.MarshalMe(sink,map);
			sink.Dump("</"~name~">");
		}
		else static assert(false, "Internal Error");
	}
	else static if(is(V == struct) || is(V == class))
	{
		sink.Dump("<"~name~">");
		ThirdPartyAccess!(V).marshial(v,SinkHandle(&map, sink));
		sink.Dump("</"~name~">");
	}
	else
	{
		sink.Dump("<"~name~">");
		MarshalNative!(typeof(v))(sink,v,map);
		sink.Dump("</"~name~">");
	}
}