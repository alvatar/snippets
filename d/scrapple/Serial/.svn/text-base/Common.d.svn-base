module Common;

public import Interface;
public import Demarshal;
import State;

alias Object function(Source!(char) source, ref OutMap) Factory;
Factory[char[]] Factories;

template NativeBaseType(T)
{
	const bool NativeBaseType = is(T == int) || is(T == uint) || is(T == long)
	|| is(T == ulong) || is(T == short) ||is(T == ushort)
	|| is(T == byte) || is(T == ubyte) || is(T == float)
	|| is(T == double) || is(T == real);
}


template CommonMixin()
{
	static if(is(typeof(this) == class))
	{
		static const char[] StaticTypeTag = typeof(this).mangleof;
		
		bool TypeTag(Sink!(char) sink, ref InMap map)
		{
			static if(CyclicType)
			{
				uint id;
				if(map.Hold(cast(void*)this,id))
				{
					sink.Dump(std.string.format(` ID="%s"/>`, id));
					return true;
				}
				else
				{
					sink.Dump(std.string.format(` ID="%s"`, id));
				}
			}
			sink.Dump(` type="`);
			XML.WriteContentXML(sink,StaticTypeTag);
			return false;
		}
		
		static this()
		{
			Factories[StaticTypeTag] = function Object(Source!(char) source, ref OutMap map)
			{
				return DemarshalMe(source,map);
			};
		}
	}

}