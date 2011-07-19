module Demarshal;

import std.conv;
import std.stdio;

import Serialize;
import utill;
import Third;
import XML;
import State;


T DemarshalNative(T)(Source!(char) source, ref OutMap map)
{

	static if(is(T == char[]))
	{
		char[] str = ReadContentXML(source);
		return str;
	}
	else static if(is(T B == B[]))
	{
		T t;
		//writef("reading list of "~B.stringof~"\n");
		if(source.Pick("null",false))
		{
			//writef("Found empty\n");
			t.length = 0;
		}
		else
		{
			//writef("Found non empty\n");
			t.length = 16;
			int at = 0;
			static if(NativeBaseType!(B))
			{
				do
				{
					if(at >= t.length) t.length = 2*at;
					//writef(".");
					t[at++] = DemarshalNative!(B)(source,map);
					//writef(".\n");
				}while(source.Pick(",",false));
			}
			else
			{
				const static char[] tag = NameFromType!(B);
				while(source.Peek("<"~tag~">"))
				{
					if(at >= t.length) t.length = 2*at;
					D!(tag,B)(source,t[at],map);
					at++;
				}
			}
			t.length = at;
		}
		return t;
	}
	else 
	{
		char[] str = ReadContentXML!("<,")(source);
		if(str == "") throw new Exception("Unexpected end of Source while looking for '"~T.stringof~"'");
	
		static if(is(T == int))			return toInt(str);
		else static if(is(T == uint))	return toUint(str);
		else static if(is(T == long))	return toLong(str);
		else static if(is(T == ulong))	return toUlong(str);
		else static if(is(T == short))	return toShort(str);
		else static if(is(T == ushort))	return toUshort(str);
		else static if(is(T == byte))	return toByte(str);
		else static if(is(T == ubyte))	return toUbyte(str);
		else static if(is(T == float))	return toFloat(str);
		else static if(is(T == double))	return toDouble(str);
		else static if(is(T == real))	return toReal(str);
		else static assert(false, "Don't known how to Demarshal "~T.stringof);
	}
}

template DemarshalMixin()
{
	static utill.Usefull!(typeof(this)).BaseType Deserialize(Source!(char) source)
	{
		mixin utill.Usefull!(typeof(this));
		static const char[] name = "root";

		OutMap map;
		BaseType ret;
		D!(name, BaseType)(source,ret,map);
		return ret;
	}
	
	// Entry point for hierarchy recursion
	static utill.Usefull!(typeof(this)).BaseType DemarshalMe(Source!(char) source, ref OutMap map)
	{
		mixin utill.Usefull!(typeof(this));
	
		BaseType ret;
		
		static if(is(BaseType == class))
			ret = new BaseType();
	
		DoIt!(BaseType)(ret, source, map);
		return ret;
	}

	// Shell for base class recursion 
	private static void DoIt(ref utill.Usefull!(typeof(this)).BaseType ret, Source!(char) source, ref OutMap map)
	{
		mixin utill.Usefull!(typeof(this));

		static if(is(BaseType == class))
		{
			static if(is(BaseType SuperT == super))
			{
				alias SuperT[0] Super;
				static if(!is(Super == Object))
				{
					Super sret = ret;
					Super.DoIt(sret, source, map);
				}
			}
		}
	
		foreach(i,v;ret.tupleof)
		{
			static const name = TrimType(BaseType.tupleof[i].stringof);
			D!(name, typeof(v))(source,(ret.tupleof)[i], map);
		}
	}
}

void D(char[] name, V)(Source!(char) source, ref V v, ref OutMap map)
{
	static if(is(typeof(&v.MarshalMe)))
	{
		static if(is(V == class))
		{
			if(source.Pick("<"~name~">NULL</"~name~">",false))
			{
				v = null;
			}
			else
			{
				source.Pick("<"~name);
	
				static if(is(typeof(typeof(v).CyclicType)) && typeof(v).CyclicType)
				{
					source.Pick(" ID=\"");
					char[] ids = XML.ReadContentXML!("\"")(source);
					source.Pick("\"");
	
					uint id = toUint(ids);
					void* vp;
					if(source.Pick("/>",false))
					{
						if(!map.Get(id, vp)) throw new Error("Back ref not found: "~ids);
						v = cast(typeof(v))vp;
						return;
					}
				}
				
	
				source.Pick(" type=\"");
				char[] type = XML.ReadContentXML!("\"")(source);
				source.Pick("\">");
				if(auto fact = type in .Serialize.Factories)
				{
					v = cast(V)(*fact)(source,map);
					assert(v !is null, "Invalid type returned by "~type~", expected " ~V.stringof);
				}
				else
					throw new Error("Unknown derived type "~type);
	
				source.Pick("</"~name~">");
	
				static if(is(typeof(typeof(v).CyclicType)) && typeof(v).CyclicType)
					map.Hold(id,cast(void*)v);
			}
		}
		else static if(is(V == struct))
		{
			source.Pick("<"~name~">");
			v = V.DemarshalMe(source, map);
			source.Pick("</"~name~">");
		}
		else static assert(false, "Internal Error");
	}
	else static if(is(V == struct) || is(V == class))
	{
		source.Pick("<"~name~">");
		v = ThirdPartyAccess!(V).demarshial(SourceHandle(&map, source));
		source.Pick("</"~name~">");
	}
	else
	{
		source.Pick("<"~name~">");
		v = DemarshalNative!(V)(source,map);
		source.Pick("</"~name~">");
	}

}