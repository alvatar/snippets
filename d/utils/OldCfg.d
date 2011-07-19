module xf.utils.OldCfg;

private {
	//import maths.Vec;
	
	//import std.conv;
	//import std.string : toString, splitlines, tolower, replace;
	
	import tango.util.Convert;
	import tango.text.Util;
	import Float = tango.text.convert.Float;
	import tango.io.device.File;
	import tango.io.device.FileMap : FileMap;
	import tango.core.Exception;
	
	import xf.omg.core.LinearAlgebra;
	import xf.utils.Memory;
	
	import mintl.sortedaa : SortedAA;
	static import mintl.mem;

	debug import tango.util.log.Trace;
}


enum FieldType : byte {
	String,
	Number,
	Binary,
	Array,
	Boolean,
	Directive,
	None
}


template TypeToFieldType(T) {
			static if (is(T == char[]))		const FieldType TypeToFieldType = FieldType.String;
	else	static if (is(T == int))				const FieldType TypeToFieldType = FieldType.Number;
	else	static if (is(T == float))			const FieldType TypeToFieldType = FieldType.Number;
	else	static if (is(T == double))		const FieldType TypeToFieldType = FieldType.Number;
	else	static if (is(T == bool))			const FieldType TypeToFieldType = FieldType.Boolean;
	else	static if (is(T == Directive))	const FieldType TypeToFieldType = FieldType.Directive;
	else	static assert (false);
}



struct Field {
	union {
		char[]		string;
		double		number;
		void[]		binary;
		Array*		array_;
		bool			boolean;
		Directive*	directive_;
	}
	
	
	Array* array() {
		assert (FieldType.Array == type);
		return array_;
	}
	
	
	void array(Array a) {
		Array[] arr;
		if (array_ is null) arr.alloc(1);
		else arr = array_[0..1];
		arr[0] = a;
		array_ = &arr[0];
	}
	
	
	void array(Array* a) {
		array_ = a;
	}
	


	Directive* directive() {
		assert (FieldType.Directive == type);
		return directive_;
	}
	
	
	void directive(char[] name, Array param) {
		Directive[] dir;
		if (directive_ is null) dir.alloc(1);
		else dir = directive_[0..1];
		dir[0].name = name;
		dir[0].param = param;
		directive_ = &dir[0];
	}
	
	
	void directive(Directive* d) {
		directive_ = d;
	}
	


	FieldType	type = FieldType.None;
	
	
	void destroy() {
		//printf("Field.destroy (%0.8x)\n", this);
		
		if (FieldType.String == type) {
			string.free();
		}
		else if (FieldType.Binary == type) {
			binary.free();
		}
		else if (FieldType.Array == type) {
			assert (array_ !is null);
			array_.destroy();
			Array[] a = array_[0..1];
			a.free();
			array_ = null;
		}
		else if (FieldType.Directive == type) {
			assert (directive_ !is null);
			directive_.param.destroy();
			Directive[] d = directive_[0..1];
			d.free();
			directive_ = null;
		}
		
		type = FieldType.None;

		//printf("<- Field.destroy\n");
	}
	
	
	char[] toString() {
		switch (type) {
			case FieldType.String:		return "str:'" ~ string ~ "'";
			case FieldType.Number:		return "nbr:" ~ to!(char[])(number); // .toString(number);
			case FieldType.Binary:		return "binary:'" ~ to!(char[])(binary.length); //.toString(binary.length);
			case FieldType.Array:			return "array:{\n" ~ array.toString ~ "}"; //array.toString ~ "}";
			case FieldType.Boolean:		return (boolean ? "bool:true" : "bool:false");
			case FieldType.Directive:	return "directive:(" ~ directive.name ~ "\n" ~ directive.param.toString ~ ")";
			case FieldType.None:			return "notype";
			default: assert (false);
		}
	}
	
	
	private T get(T)() {
		static if (is(T == char[]))		return string;
		static if (is(T == int))				return cast(int)number;
		static if (is(T == float))			return cast(float)number;
		static if (is(T == double))		return number;
		static if (is(T == bool))			return boolean;
		static if (is(T == Directive))	return *directive;
	}
}


struct FieldTuple {
	Field[]	fields;
	uint		numFields;
	
	
	void addField(Field f) {
		//fields ~= f;
		append(fields, f, &numFields);
	}
	
	
	void compress() {
		fields.realloc(numFields);
		
		foreach (ref f; fields) {
			if (FieldType.Array == f.type) {
				f.array.compress();
			}
			else if (FieldType.Directive == f.type) {
				f.directive.param.compress();
			}
		}
	}
	
	
	void destroy() {
		//printf("FieldTuple.destroy\n");

		foreach (ref f; fields[0 .. numFields]) {
			f.destroy();
		}
		
		fields.free();
		numFields = 0;

		//printf("<- FieldTuple.destroy\n");
	}
	
	
	char[] toString() {
		char[] result;
		foreach (i, ref f; fields[0 .. numFields]) {
			if (0 == i) result ~= to!(char[])(f); //f.toString();
			else result ~= " " ~ to!(char[])(f); //f.toString();
		}
		return result;
	}
}


/**
	An array of optionally named FieldTuple's. Names need not be unique, as they are indexable.
*/
struct Array {
	FieldTuple[]	fields;
	char[][]			fieldNames;
	uint				numFields;
	uint				numFieldNames;
	
	SortedAA!(char[], int[], false, mintl.mem.Malloc) namedFields;
	
	
	void addFieldTuple(ref FieldTuple ft, char[] name = null) {
		if (name !is null) {
			//namedFields[name] ~= fields.length;
			//append(namedFields[name], fields.length);
			int[]* nfields = namedFields.get(name);
			if (nfields is null) {
				int[] emptyArray;
				namedFields[name] = emptyArray;
				nfields = namedFields.get(name);
				assert (nfields !is null);
			}
			
			(*nfields).append(numFields);
		}
		
		//fields ~= ft;
		//fieldNames ~= name;
		append(fields, ft, &numFields);
		append(fieldNames, name, &numFieldNames);
	}
	
	
	void compress() {
		fields.realloc(numFields);
		fieldNames.realloc(numFieldNames);
		
		foreach (ref ft; fields) {
			ft.compress();
		}
	}
	
	
	void destroy() {
		debug Trace.formatln("Array.destroy");

		foreach (ref n; fieldNames[0 .. numFieldNames]) {
			n.free();
		}
		
		fieldNames.free();
		numFieldNames = 0;
		
		
		foreach (ref int[] nf; namedFields) {
			nf.free();	
		}
		namedFields.clear();


		foreach (ref f; fields[0 .. numFields]) {
			f.destroy();
		}
		
		fields.free();
		numFields = 0;
		
		debug Trace.formatln("<- Array.destroy");
	}
	
	
	///
	char[] toString() {
		char[] result;
		foreach (i, f; fields[0 .. numFields]) {
			char[] data;
			
			if (fieldNames[i] !is null) {
				data ~= fieldNames[i] ~ ":";
			}
			
			data ~= to!(char[])(f); // f.toString();
			
			foreach (line; splitLines(data)) {
				result ~= "  " ~ line ~ "\n";
			}
		}
		
		return result;
	}
	
	
	/**
		Number of fields of the specified name in the array
	*/
	int count(char[] name) {
		if (auto nf = namedFields.get(name)) {
			return nf.length;
		} else {
			return 0;
		}
	}
	
	/**
		Number of fields in the array
	*/
	int count() {
		return numFields;
	}
	
	
	/**
		Length of a named tuple
		
		Params:
		name = the name of the field tuple
		index = optional index of the tuple
	*/
	int tupleLength(char[] name, int index = 0) {
		return fields[namedFields[name][index]].numFields;
	}
	

	/**
		Length of i-th tuple
	*/
	int tupleLength(int index) {
		return fields[index].numFields;
	}
	

	/**
		Get a named child from a specific tuple
		
		Params:
		name = name of the tuple
		index = i-th tuple of this name
		tupleIndex = i-th item in the tuple
	*/
	Array child(char[] name, int index = 0, int tupleIndex = 0) {
		auto item = fields[namedFields[name][index]].fields[tupleIndex];
		assert (FieldType.Array == item.type);
		return *item.array;
	}


	/**
		Get a child from a specific tuple
		
		Params:
		index = i-th tuple
		tupleIndex = i-th item in the tuple
	*/
	Array child(int index = 0, int tupleIndex = 0) {
		auto item = fields[index].fields[tupleIndex];
		assert (FieldType.Array == item.type);
		return *item.array;
	}
	
	
	/**
		Check whether this Array contains a named child
		
		Params:
		name = name of the tuple
		index = i-th tuple of this name
		tupleIndex = i-th item in the tuple
	*/
	bool hasChild(char[] name, int index = 0, int tupleIndex = 0) {
		auto indices = namedFields.get(name);
		if (indices is null) return false;
		if (indices.length <= index) return false;
		auto tuple = fields[(*indices)[index]];
		if (tuple.numFields <= tupleIndex) return false;
		auto item = tuple.fields[tupleIndex];
		return FieldType.Array == item.type;
	}
	
	
	/**
		Check whether this Array contains a named field
	*/
	bool hasField(char[] name) {
		return count(name) > 0;
	}
	
	
	/**
		Returns the type of the field
	*/
	FieldType fieldType(char[] name, int index = 0, int tupleIndex = 0) {
		assert (namedFields.get(name) !is null);
		assert (namedFields[name].length > index);
		assert (fields[namedFields[name][index]].numFields > tupleIndex);
		
		auto item = fields[namedFields[name][index]].fields[tupleIndex];
		return item.type;
	}
	

	/// ditto
	FieldType fieldType(int index = 0, int tupleIndex = 0) {
		auto item = fields[index].fields[tupleIndex];
		return item.type;
	}


	/**
		Get a named field from the Array
		
		Params:
		name = name of the tuple
		index = i-th tuple of this name
		tupleIndex = i-th item in the tuple
	*/
	T simpleFieldAccess(T)(char[] name, int index = 0, int tupleIndex = 0) {
		assert (namedFields.get(name) !is null);
		assert (namedFields[name].length > index);
		assert (fields[namedFields[name][index]].numFields > tupleIndex, to!(char[])(fields[namedFields[name][index]].numFields) ~ " vs " ~ to!(char[])(tupleIndex) ~ " at '" ~ name ~ "'");
		
		auto item = fields[namedFields[name][index]].fields[tupleIndex];
		assert (item.type == TypeToFieldType!(T), "'" ~ name ~ "' : " ~ to!(char[])(cast(int)item.type) ~ " : "); //.toString(cast(int)item.type) ~ " : ");
		return item.get!(T);
	}
	
	
	alias simpleFieldAccess!(char[])		stringNoDup_;	/// ditto
	alias simpleFieldAccess!(int)			int_;					/// ditto
	alias simpleFieldAccess!(float)			float_;				/// ditto
	alias simpleFieldAccess!(double)		double_;			/// ditto
	alias simpleFieldAccess!(bool)			bool_;				/// ditto
	alias simpleFieldAccess!(Directive)	directive;			/// ditto
	
	/// ditto
	char[] string_(char[] name, int index = 0, int tupleIndex = 0) {
		return stringNoDup_(name, index, tupleIndex).dup;
	}


	
	/// ditto
	vec2 vec2_(char[] name, int index = 0, int tupleIndex = 0) {
		return vec2(float_(name, index, tupleIndex), float_(name, index, tupleIndex+1));
	}
	
	/// ditto
	vec3 vec3_(char[] name, int index = 0, int tupleIndex = 0) {
		return vec3(float_(name, index, tupleIndex), float_(name, index, tupleIndex+1), float_(name, index, tupleIndex+2));
	}

	/// ditto
	vec4 vec4_(char[] name, int index = 0, int tupleIndex = 0) {
		return vec4(float_(name, index, tupleIndex), float_(name, index, tupleIndex+1), float_(name, index, tupleIndex+2), float_(name, index, tupleIndex+3));
	}

	/// ditto
	vec2i vec2i_(char[] name, int index = 0, int tupleIndex = 0) {
		return vec2i(int_(name, index, tupleIndex), int_(name, index, tupleIndex+1));
	}
	
	/// ditto
	vec3i vec3i_(char[] name, int index = 0, int tupleIndex = 0) {
		return vec3i(int_(name, index, tupleIndex), int_(name, index, tupleIndex+1), int_(name, index, tupleIndex+2));
	}
	
	/// ditto
	vec4i vec4i_(char[] name, int index = 0, int tupleIndex = 0) {
		return vec4i(int_(name, index, tupleIndex), int_(name, index, tupleIndex+1), int_(name, index, tupleIndex+2), int_(name, index, tupleIndex+3));
	}
	
	
	/**
		Get an array of fields from tuples
		
		Params:
		name = name of the tuple
		result = the place to put the result into. Must match exactly in size.
		tupleIndex = tuple offset
		stride = stride for the destination
	*/
	void simpleArrayAccess(T)(char[] name, T[] result, int tupleIndex = 0, int stride = 1) {
		int[] idxArray = namedFields[name];
		assert (idxArray.length * stride == result.length);
		int dst = 0;
		foreach (idx; idxArray) {
			assert (fields[idx].fields[tupleIndex].type == TypeToFieldType!(T));
			result[dst] = fields[idx].fields[tupleIndex].get!(T);
			dst += stride;
		}
	}
	
	
	alias simpleArrayAccess!(char[]) 	stringNoDup_;		/// ditto
	alias simpleArrayAccess!(int)			int_;						/// ditto
	alias simpleArrayAccess!(float)		float_;					/// ditto
	alias simpleArrayAccess!(double)	double_;				/// ditto
	alias simpleArrayAccess!(bool)		bool_;					/// ditto

	/// ditto
	void string_(char[] name, char[][] result, int tupleIndex = 0, int stride = 1) {
		stringNoDup_(name, result, tupleIndex, stride);
		foreach (ref r; result) r = r.dup;
	}


	void vectorArrayAccess(V)(char[] name, V[] result, int tupleIndex = 0, int stride = 1) {
		for (int i = 0; i < V.dim; ++i) {
			simpleArrayAccess!(V.flt)(name, (cast(V.flt*)result.ptr + i)[0..result.length * V.dim], tupleIndex + i, i + stride * V.dim);
		}
	}
	
	alias vectorArrayAccess!(vec2)	vec2_;		/// ditto
	alias vectorArrayAccess!(vec3)	vec3_;		/// ditto
	alias vectorArrayAccess!(vec4)	vec4_;		/// ditto
	alias vectorArrayAccess!(vec2i)	vec2i_;		/// ditto
	alias vectorArrayAccess!(vec3i)	vec3i_;		/// ditto
	alias vectorArrayAccess!(vec4i)	vec4i_;		/// ditto

	

	/**
		Get a field from the Array
		
		Params:
		index = i-th tuple
		tupleIndex = i-th item in the tuple
	*/
	T simpleIndexedFieldAccess(T)(int index = 0, int tupleIndex = 0) {
		auto item = fields[index].fields[tupleIndex];
		assert (TypeToFieldType!(T) == item.type);
		return item.get!(T);
	}
	

	alias simpleIndexedFieldAccess!(char[])		stringNoDup_;		/// ditto
	alias simpleIndexedFieldAccess!(int)			int_;						/// ditto
	alias simpleIndexedFieldAccess!(float)			float_;					/// ditto
	alias simpleIndexedFieldAccess!(double)		double_;				/// ditto
	alias simpleIndexedFieldAccess!(bool)			bool_;					/// ditto
	alias simpleIndexedFieldAccess!(Directive)	directive;				/// ditto

	/// ditto
	char[] string_(int index = 0, int tupleIndex = 0) {
		return stringNoDup_(index, tupleIndex).dup;
	}

	
	/// ditto
	vec2 vec2_(int index = 0, int tupleIndex = 0) {
		return vec2(float_(index, tupleIndex), float_(index, tupleIndex+1));
	}
	
	/// ditto
	vec3 vec3_(int index = 0, int tupleIndex = 0) {
		return vec3(float_(index, tupleIndex), float_(index, tupleIndex+1), float_(index, tupleIndex+2));
	}

	/// ditto
	vec4 vec4_(int index = 0, int tupleIndex = 0) {
		return vec4(float_(index, tupleIndex), float_(index, tupleIndex+1), float_(index, tupleIndex+2), float_(index, tupleIndex+3));
	}

	/// ditto
	vec2i vec2i_(int index = 0, int tupleIndex = 0) {
		return vec2i(int_(index, tupleIndex), int_(index, tupleIndex+1));
	}
	
	/// ditto
	vec3i vec3i_(int index = 0, int tupleIndex = 0) {
		return vec3i(int_(index, tupleIndex), int_(index, tupleIndex+1), int_(index, tupleIndex+2));
	}
	
	/// ditto
	vec4i vec4i_(int index = 0, int tupleIndex = 0) {
		return vec4i(int_(index, tupleIndex), int_(index, tupleIndex+1), int_(index, tupleIndex+2), int_(index, tupleIndex+3));
	}



	/**
		Get an array of fields from tuples
		
		Params:
		result = the place to put the result into. Must match exactly in size.
		tupleIndex = tuple offset
		stride = stride for the destination
	*/
	void simpleIndexedArrayAccess(T)(T[] result, int tupleIndex = 0, int stride = 1) {
		assert (numFields * stride == result.length, to!(char[])(numFields * stride) ~ ` == ` ~ to!(char[])(result.length)); //.toString(numFields * stride) ~ ` == ` ~ .toString(result.length));
		int dst = 0;
		foreach (idx, field; fields[0 .. numFields]) {
			assert (field.fields[tupleIndex].type == TypeToFieldType!(T));
			result[dst] = field.fields[tupleIndex].get!(T);
			dst += stride;
		}
	}


	alias simpleIndexedArrayAccess!(char[])		stringNoDup_;	/// ditto
	alias simpleIndexedArrayAccess!(int)			int_;					/// ditto
	alias simpleIndexedArrayAccess!(float)		float_;				/// ditto
	alias simpleIndexedArrayAccess!(double)		double_;			/// ditto
	alias simpleIndexedArrayAccess!(bool)		bool_;				/// ditto

	/// ditto
	void string_(char[][] result, int tupleIndex = 0, int stride = 1) {
		stringNoDup_(result, tupleIndex, stride);
		foreach (ref r; result) r = r.dup;
	}


	void vectorIndexedArrayAccess(V)(V[] result, int tupleIndex = 0, int stride = 1) {
		for (int i = 0; i < V.dim; ++i) {
			simpleIndexedArrayAccess!(V.flt)((cast(V.flt*)result.ptr + i)[0..result.length * V.dim], tupleIndex + i, stride * V.dim);
		}
	}
	
	alias vectorIndexedArrayAccess!(vec2)	vec2_;		/// ditto
	alias vectorIndexedArrayAccess!(vec3)	vec3_;		/// ditto
	alias vectorIndexedArrayAccess!(vec4)	vec4_;		/// ditto
	alias vectorIndexedArrayAccess!(vec2i)	vec2i_;		/// ditto
	alias vectorIndexedArrayAccess!(vec3i)	vec3i_;		/// ditto
	alias vectorIndexedArrayAccess!(vec4i)	vec4i_;		/// ditto
}

struct Directive {
	char[]	name;
	Array	param;
}

/// ditto
alias Array Config;

class CfgLoaderException : Exception {
    char[] file;
    int line;
    
    this(char[] file, int line, char[] msg) {
        super (msg);
        
        this.file = file;
        this.line = line;
    }
}

/**
	Parses a simple, yet powerful config file format.
	
	Examples:
	---
pixelformat = {
	bits = 32
	depth = 16
}

xres 		= 800
yres		= 600
title		= 'Deadlock'
fullscreen	= false
	---
	
	TODO: full file format specification
*/

import tango.io.Stdout;

class CfgLoader {
	bool latentDirectiveParsing = false;


	/**
		Load the specified file. Return this.
	*/
	CfgLoader load(char[] filename) {
		auto srcFile = new FileMap(filename, File.ReadExisting);
		
		scope (exit) {
			srcFile.close();
			delete srcFile;
		}
		
		getc = delegate char() {
			char x = '\r';
			
			while ('\r' == x) {
				if (srcFile.read((cast(void*)(&x))[0..1]) == srcFile.Eof) {
					_end = true;
					return char.init;
				}
			}
			
			return x;
		};
		
		this.filename = filename;
		this.currentLine = 1;
        
		parseArray(root, char.init, 0, 0, true);
		root.compress();
		
		return this;
	}
	
	
	/**
		Parse the specified data buffer. Return this.
	*/
	CfgLoader loadFromMem(char[] data) {
		char* it = &data[0];
		char* end = it + data.length;

		this.filename = "(from memory)";
		this.currentLine = 1;
		
		char eofGetc() {
			return char.init;
		}
		char normalGetc() {
			while (it < end) {
				char res = *it++;
				if (res != '\r') {
					return res;
				}
			} 

			_end = true;
			getc = &eofGetc;
			return char.init;
		}
		
		getc = &normalGetc;
		parseArray(root, char.init, 0, 0, true);
		//root.compress();
		
		return this;
	}
	
	
	alias void function(ref Array param, ref FieldTuple fieldTuple) DirectiveHandler;
	
	
	/**
		Add a handler function for a custom directive
		
		Params:
		dname = name of the directive
		handler = a function taking an Array of params and returning results into fieldTuple
	*/
	void addCustomDirective(char[] dname, DirectiveHandler handler) {
		customDirectives[dname] = handler;
	}


	/**
		The parsing results. Destroyed in CfgParser's dtor. Use dropResult() to prevent that.
	*/
	Array result() {
		return root;
	}
	
	
	/**
		Destroy all the data associated with this CfgLoader. May be useful to load another config.
	*/
	void destroy() {
		root.destroy();
		root = root.init;

		discardWindow(0);

		_end = false;
	}
	
	
	/**
		This will cause the resulting data not to be lost during a call to destroy() or during dtor execution
	*/
	void forgetResult() {
		root = root.init;
	}
	
	
	this() {
		_windowBuffer.alloc(1024);
	}


	~this() {
		destroy();
		destroyWindowBuffer();
	}


protected:


	char delegate()				getc;
	DirectiveHandler[char[]]	customDirectives;

	
	void parseArray(ref Array array, char blockEnd, int arrayNesting, int directiveNesting, bool topLevel = false) {
		while (true) {
			//writefln("white2<");
			skipWhite();
			//writefln(">white2");
			
			if (inputEnd) {
				if (!topLevel) {
					error("unexpected end of file, expected '" ~ blockEnd ~ "'");
				} else {
					return;
				}
			}
			
			char firstChar = window(0);
			
			if ('\n' == firstChar)
				++currentLine;
			
			if ('\n' == firstChar || ',' == firstChar) {
				discardWindow(1);
			}
			else if (blockEnd == firstChar) {
				discardWindow(1);
				return;
			}
			else if (validNameChar(firstChar, true) && !curIdent("true") && !curIdent("false")) {
				// a named value
				char[] name = parseName();
				skipWhite();
				
				//printf("name: %.*s\n", name);
				
				if (window(0) == '=') {
					discardWindow(1);
					skipWhite();
					
					FieldTuple f;
					parseValue(f, arrayNesting, directiveNesting);
					array.addFieldTuple(f, name);
				} else {		// no value, just a name
					FieldTuple f;
					array.addFieldTuple(f, name);
				}
			}
			else {
				//Stdout("noname field ")(firstChar).newline;
				FieldTuple f;
				parseValue(f, arrayNesting, directiveNesting);
				array.addFieldTuple(f, null);
			}
		}
		
		assert (topLevel);
	}
	
	
	bool curIdent(char[] str) {
		foreach (i, c; str) {
			if (window(i) != c || inputEnd) return false;
		}
		return !validNameChar(window(str.length), false);
	}
	
		
	void parseValue(ref FieldTuple fieldTuple, int arrayNesting, int directiveNesting) {
		while (!inputEnd) {
			skipWhite();
			
			char firstChar = window(0);
	
			if (inputEnd)
				break;
	
			if ('-' == firstChar || '.' == firstChar || (firstChar >= '0' && firstChar <= '9')) {
				//writefln("parsing a nbr");
				
				int lim = 1;
				while (!inputEnd && validNumberChar(window(lim))) {
					++lim;
				}
				
				Field field;
				field.number = Float.parse(window(0, lim));//to!(double)(window(0, lim));
				field.type = FieldType.Number;
				fieldTuple.addField(field);
				
				//writefln("got a nbr: ", field.number);
				
				discardWindow(-1);
			} else
			
			if ('(' == firstChar) {
				discardWindow(1);
				char[] directiveName = parseName();
				Array param;
				parseArray(param, ')', arrayNesting, directiveNesting + 1);
				
				processDirective(directiveName, param, fieldTuple);
				directiveName.free();
				param.destroy();
			} else
			
			if ('{' == firstChar) {
				discardWindow(1);
				
				Array param;
				parseArray(param, '}', arrayNesting + 1, directiveNesting);
				
				processDirective(`array`, param, fieldTuple);
				param.destroy();
			} else
			
			if ('\'' == firstChar) {
				// string
				Field field;
				field.type = FieldType.String;
				
				uint stringLen;
				
				discardWindow(1);
				
				void addChar(char c) {
					/+field.string.realloc(field.string.length + 1);
					field.string[$-1] = c;+/
					field.string.append(c, &stringLen);
				}
				
				bool escaped = false;
				while (true) {
					if ('\\' == window(0)) {
						discardWindow(1);

						if (escaped) {
							addChar('\\');
						}
						
						escaped = !escaped;
					}
					
					else if ('\'' == window(0)) {
						discardWindow(1);
						
						if (escaped) {
							escaped = false;
							addChar('\'');
						} else {
							break;
						}
					}
					
					else {
						if ('\n' == window(0))
							++currentLine;
						
						if (escaped) {
							addChar('\\');
							escaped = false;
						}
						addChar(window(0));
						discardWindow(1);
					}
				}
				
				field.string.realloc(stringLen);
				fieldTuple.addField(field);
				//printf("got a str: '%.*s'\n", field.string);
	
			} else
			
			if (curIdent("true")) {
				Field field;
				field.boolean = true;
				field.type = FieldType.Boolean;
				fieldTuple.addField(field);
				discardWindow(4);
			} else
			
			if (curIdent("false")) {
				Field field;
				field.boolean = false;
				field.type = FieldType.Boolean;
				fieldTuple.addField(field);
				discardWindow(5);
			} else

			if ('}' == firstChar) {
				if (arrayNesting == 0)
					error("invalid nesting: unexpected '}'");
				
				// end of data directive
				return;
			} else
			
			if (')' == firstChar) {
				if (directiveNesting == 0)
					error("invalid nesting: unexpected ')'");
				
				// end of directive != data
				return;
			} else
			
			if (',' == firstChar || '\n' == firstChar) {
				// next array item
				return;
			} else
				error("invalid value char: " ~ firstChar);
			
		}
	}
	
	
	void processDirective(char[] directiveName, ref Array param, ref FieldTuple fieldTuple) {
		switch (directiveName) {
			case `array`: {
				Field f;
				f.type	= FieldType.Array;
				f.array	= param;
				fieldTuple.addField(f);
				param = Array.init;		// do not let destroy() be called on it
			} break;
			
			case `include`: {
				scope ldr = new CfgLoader;
				ldr.latentDirectiveParsing = this.latentDirectiveParsing;
				
				if (param.numFields != 1 || param.fields[0].numFields != 1 || FieldType.String != param.fields[0].fields[0].type)
					error("include expects exactly one string parameter");
				
				//writefln("include<");
				ldr.load(param.fields[0].fields[0].string);
				//writefln(">include");
				
				Field f;
				f.type	= FieldType.Array;
				f.array	= ldr.result;
				fieldTuple.addField(f);
				
				ldr.forgetResult();
			} break;
			
			case `flatten`: {
				foreach (ref tuple; param.fields[0 .. param.numFields]) {
					foreach (ref field; tuple.fields[0 .. tuple.numFields]) {
						flattenAndErase(field, fieldTuple);
						field = Field.init;
					}
				}
			} break;
			
			default: {
				if (directiveName in customDirectives) {
					customDirectives[directiveName](param, fieldTuple);
					return;
				}
				
				if (latentDirectiveParsing) {
					Field f;
					f.type	= FieldType.Directive;
					f.directive(directiveName, param);
					fieldTuple.addField(f);
					param = Array.init;		// do not let destroy() be called on it
					return;
				}
				
				error("directive not found: " ~ directiveName);
			}
		}
	}
	
	
	static void flattenAndErase(Field f, ref FieldTuple fieldTuple) {
		if (FieldType.Array != f.type) {
			fieldTuple.addField(f);
		} else {
			foreach (ref tuple; f.array.fields[0 .. f.array.numFields]) {
				foreach (ref field; tuple.fields[0 .. tuple.numFields]) {
					flattenAndErase(field, fieldTuple);
					field = Field.init;
				}
			}
		}
	}
	
	
	void skipWhite() {
		void skipLine() {
			while (!inputEnd) {
				if (window(0) == '\n') {
					++currentLine;
					
					return;
				}
				
				discardWindow(1);
			}
		}
		
		void skipBlock() {
			int depth = 1;
			
			while (!inputEnd) {
				if (window(0) == '\n')
					++currentLine;
				
				if (window(0) == '+') {
					if (window(1) == '/') {
						discardWindow(2);
						--depth;
						if (0 == depth) return;
						else continue;
					}
				}
				
				else if (window(0) == '/') {
					if (window(1) == '+') {
						discardWindow(2);
						++depth;
						continue;
					}
				}

				discardWindow(1);
			}
			
			if (depth > 0)
				error("unterminated comment");
		}
		
		while (!inputEnd) {
			char w0 = window(0);
			
			if (w0 == ' ' || w0 == '\t') {
				discardWindow(1);
				continue;
			}
			
			if (w0 == '/') {
				char w1 = window(1);
				
				if (w1 == '/') {
					return skipLine();
				}
				
				if (w1 == '+') {
					discardWindow(2);
					skipBlock();
					continue;
				}
			}
			
			break;
		} 
	}
	
	
	bool inputEnd() {
		return _end;
	}
	
	
	char[] parseName() {
		char[] res;
		uint nameLen = 0;
		
		for (int i = 0; true; ++i) {
			char c = window(i);
			if (inputEnd) break;
			
			if (validNameChar(c, 0 == i)) {
				append(res, c, &nameLen);
			} else {
				break;
			}
		}
		
		if (nameLen == 0)
			error("expected name");
		
		assert (res !is null);
		debug Trace.formatln("realloc from %d to %d, ptr=%0.8x\n", res.length, nameLen, res.ptr);
		res.realloc(nameLen);
		
		discardWindow(-1);
		return res;
	}
	
	
	bool validNameChar(char c, bool first = false) {
		return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (!first && ((c >= '0' && c <= '9') || c == '-')) || c == '_';
	}


	bool validNumberChar(char c, bool first = false) {
		return (c >= '0' && c <= '9') || c == '.' || c == '-' || (!first && (c == '_' || c == 'e' || c == '+'));
	}
	
	
	/**
		Params:
			offset = offset for window.length
	*/
	void discardWindow(int offset) {
		assert (_windowBuffer.ptr + _windowStart == _window.ptr);
		
		if (offset > 0) {
			_windowStart += offset;
			_window = _window[offset .. $];
		} else {
			_windowStart += _window.length + offset;
			_window = _window[$+offset .. $];
		}
		
		if (0 == _window.length) {
			_window = _windowBuffer[0..0];
			_windowStart = 0;
		} else {
			assert (_windowBuffer.ptr + _windowStart == _window.ptr);
		}
	}
	
	
	void destroyWindowBuffer() {
		_windowBuffer.free();
		_window = null;
	}
	
	
	char window(uint idx) {
		auto wnd = _window;
		
		if (wnd.length <= idx) {
			// make sure we have this char
			// track the lines and character indices of beginning and end of window
			
			if (_windowBuffer.length <= idx + _windowStart) {
				_windowBuffer.realloc(idx + _windowStart + 1);
			}
		
			int fillFrom = wnd.length;
			wnd = _windowBuffer.ptr[_windowStart .. _windowStart + idx + 1];
			
			final char* end = wnd.ptr + wnd.length;
			for (char* it = &wnd.ptr[fillFrom]; it != end; ++it) {
				if (_end) {
					error("unexpected end of file");
				}
				
				*it = getc();
			}
			
			_window = wnd;
			
			/+foreach (ref c; _window[fillFrom .. $]) {
				if (inputEnd)
					error("unexpected end of file");
				
				c = getc();
			}+/
		}
		
		return wnd.ptr[idx];
	}
	
	
	char[] window(int start, int postEnd) {
		return _window[start .. postEnd];
	}

    
	void error(char[] msg) {
		throw new CfgLoaderException(filename, currentLine, msg);
	}


	char[] filename;
	int currentLine;
    
    
	Array	root;
	char[]	_window;
	int		_windowStart;
	bool		_end = false;
	
	char[]	_windowBuffer;
}




Config cachedConfigLoad(char[] filename_) {
	char[] fname = filename_.replace('\\', '/');
	
	if (fname in loadedConfigs) {
		return loadedConfigs[fname];
	}
	
	scope cfgLoader = new CfgLoader;
	cfgLoader.load(fname);
	auto res = cfgLoader.result();
	cfgLoader.forgetResult;
	
	return loadedConfigs[fname] = res;
}


private {
	Config[char[]]	loadedConfigs;
}
