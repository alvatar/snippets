import std.stdio;
/**
The intended use for this lib is for debugging the external API's. For example 
I wrote it because I was having issues with a mysql wrapper lib and wanted to 
known what api calls were being made.

To use it, replace the call to be logged with a wrapped call:

ulong r = mysql_real_escape_string(connection, ret.ptr, string.ptr, string.length);
ulong r = TraceAPI!("mysql_real_escape_string)(__FILE__,__LINE__,connection, ret.ptr, string.ptr, string.length);

a little fun with regex and you can have a whole file done in about a minute

This software is distributed without any warranty of any kind and is NOT suitable for any 
production usage. 

Author: Benjamin Shropshire  shro8822 -> vandals uidaho edu
*/

template MixinTraceAPI()
{
template TraceAPI(char[] fn)
{
	// Handy sed rules
	// s/\(MyLibPrefix_\w*\)(/TraceAPI!("$1")(__FILE__,__LINE__,/
	// s/,\s*)/)/
	static if(is(typeof(mixin(fn)) R == return) && is(typeof(mixin(fn)) T == function))
	{
		debug(compileTimeTrace) pragma(msg, R.stringof~" "~fn~T.stringof);
		R TraceAPI(char[] file, int line, T t)
		{
			debug(runTimeTrace)
			{
				int strlen(char* _){for(int i=0;;i++)if(!_[i])return i;}
				writef(" %s:%s %s ",file, line, fn);
				foreach(i,a;T)
				{
					static if(is(a == char*))
						writef(`, "%s"`, t[i][0..strlen(t[i])]);
					else static if(is(a : int))
						writef(", %d", t[i]);
					else
						writef(", [%s]%s", a.stringof,t[i]);
				}
				writef(\n);
			}
			static if(is(R == void))
			{
				mixin(fn~"(t);");
				debug(runTimeTrace) writef(" %s:%s %s  = void\n",file, line, fn);
				return;
			}
			else
			{
				R ret = mixin(fn~"(t)");
				debug(runTimeTrace) writef(" %s:%s %s  = ",file, line, fn);
				static if(is(R == char*))
					writef(`"%s"\n`, ret[0..strlen(ret)]);
				else static if(is(R : int))
					writef("%d\n", ret);
				else
					writef("[%s]%s\n", R.stringof,ret);
				return ret;
			}
		}
	}
	else
		static assert(false, fn);
}
}

