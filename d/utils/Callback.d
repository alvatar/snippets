module xf.utils.Callback;
/**
 * Usage:
 * ----------------------------------
 * alias Callback!(my_return_value, my_arguments) myCallback
 * ----------------------------------
 * for example
 * ----------------------------------
 * alias Callback!(char[], float, int) myCallback;
 * ----------------------------------
 * 
 * creating and using the callback:
 * ----------------------------------
 * myCallback callback;
 * callback= &my_function;
 * ---------------------------------- 
 * or
 * ----------------------------------
 * callback= &my_delegate;
 * ----------------------------------
 * To call storred delegate or function you just need to 
 * do something like
 * ----------------------------------
 * callbacl(0.1,2);
 * ----------------------------------
 * So the same if calling normal funciton.
 * 
 * Note: 
 * Unfortunatly there is something borked with static opCall so
 * ----------------------------------
 * myCallback callback=&my_function
 * ----------------------------------
 * doas not work :F
 */
struct Callback(Return, T ...){
	private  alias Return function(T) function_T;
	private  alias Return delegate(T) delegate_T;
	union {
		private function_T _func;
		private delegate_T _deleg;
	}
	private enum State : byte{ NotSet, Function, Delegate }
	private State state;
	
	typeof(this) opAssign(function_T f){
		_func = f;
		state = State.Function;
		return this;
	}
	
	typeof(this) opAssign(delegate_T d){
		_deleg = d;
		state = State.Delegate;
		return this;
	}
	
	/+static typeof(this) opCall(delegate_T d){
		typeof(this) tmp;
		tmp._deleg=d;
		tmp.state = State.Delegate;
		return tmp;
	}
	
	static typeof(this) opCall(function_T f){
		typeof(this) tmp;
		tmp._func = f;
		tmp.state = State.Function;
		return tmp;
	}+/
			
	bool isFunction(){
		return state==State.Function;
	}
	
	bool isDelegate(){
		return state==State.Delegate;
	}
	
	bool isSet(){
		return state!=State.NotSet;
	}
	
	delegate_T delegate_(){
		if(!isDelegate){
			assert(false,"not a delegate");
			return null;
		}
		return _deleg;
	}
	
	function_T function_(){
		if(!isFunction){
			assert(false, "not a function");
			return null;
		}
		return _func;
	}
	
	Return opCall(T t){
		if(isFunction){
			return function_()(t);
		} else if(isDelegate){
			return delegate_()(t);
		} 
		
		assert(false, "callback not set");
	}
	
	template genParameterList (T ...) {
		static if( T.length == 0) {
			const char[] genParameterList = "";
		} else {
			const char[] genParameterList = T[0].stringof ~ ", "~ genParameterList!(T[1..$]);
		}
	}
	
	static char[] genSetters(char[] settername, char[] varname) {
		
		
		char[] result;
		result ~= genSetters(settername, varname, "function");
		result ~= "\n\n";
		result ~= genSetters(settername, varname, "delegate");
		
		return result;
	}
	
	private static char[] genSetters(char[] settername, char[] varname, char[] fordel) {
			char [] result;
			auto paramList = genParameterList!(T);
			if(paramList.length > 2) {
				paramList = paramList[0..$-2];
			}
			result ~= "void "~settername~" ( "~ Return.stringof ~ " "~fordel~"( " ~paramList  ~ ") _"~ varname ~ "){ \n";
			result ~= "	"~ varname ~ "=_" ~ varname ~ ";\n";
			result ~= "}";
			return result;
		}
}

version (Unittest){
	int random(){
		return 4; 	// chosen by fair dice roll
					// guaranteed to by random.
		
		// inspired by http://xkcd.com/221/
	}
}

unittest{
	{
		alias Callback!(char[], float, int) TestCallback; // args and return val
		
		char[] testDelegate(float a, int b){
   			if(a==0){
				return "zero";
			}
			if(b>0){
				return "plus";
			} 
		};
		
		TestCallback callback;
		callback = &testDelegate;
		
		assert(callback.isSet);
		assert(callback.isDelegate);
		assert(callback(0,1)=="zero");
		assert(callback(1,1)=="plus");
		assert(callback.delegate_()(1,1)=="plus");
	}
	{
		alias Callback!(int) TestCallback; // return value, no args
		TestCallback callback;
		callback=&random;
		assert(callback()==4);
		assert(callback.isSet);
		assert(callback.isFunction);
		assert(callback.function_()()==4);
	}
	{
		alias Callback!(void) TestCallback; //  no args, no return value
		int a;
		TestCallback callback;
		callback = {
			a=random();
		};
		
		callback();
		assert(a==4);
	}
	{
		alias Callback!(void,int) TestCallback; // no return value, args;
		char[] test;
		TestCallback callback;
		callback = (int a){
			test.length=a;
		};
		callback(random());
		assert(test.length==4);
	}
}