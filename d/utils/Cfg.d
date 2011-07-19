module xf.utils.Cfg;


private {
	import xf.utils.Memory;
	import mintl.sortedaa;
	import mintl.mem : Mallocator = Malloc;

	import tango.io.protocol.model.IReader;
	import tango.io.device.File;
	import tango.io.FilePath;
	import tango.io.protocol.Reader;
	import tango.io.GrowBuffer;

	import tango.core.Exception;
	import tango.core.Array : contains;

	import tango.stdc.ctype;
	//import tango.stdc.stdlib : cFree = free, cMalloc = malloc;
	import xf.utils.Memory;
	
	import Integer = tango.text.convert.Integer;
	import Float = tango.text.convert.Float;
	
	//debug = UtilsCfg;
	
	//debug (UtilsCfg) {
		import tango.io.Stdout;
	//}
}



// ------------------------------------------------------------------------------------------------------------------------------------------------------------


struct Data {
	enum Type : ubyte {
		Bool,
		Int,
		Float,
		String,
		Char,		// special character
		Tuple,
		Directive,
		AssignExpr,
		None
	}
	
	
	static Data ctorHack(T)(T t) {
		Data res;
		res.type = DataType!(T);
		static if (is(T == bool)) res._bool_ = t;
		else static if (is(T == int)) res._int_ = t;
		else static if (is(T == float)) res._float_ = t;
		else static if (is(T == char)) res._char_ = t;
		else static if (is(T == char[])) {
			res._string_.alloc(t.length);
			res._string_()[] = t[];
		} 
		else static assert (false);
		return res;
	}
	
	alias ctorHack!(bool)		opCall;
	alias ctorHack!(int)			opCall;
	alias ctorHack!(float)		opCall;
	alias ctorHack!(char[])	opCall;
	alias ctorHack!(char)		opCall;
	
	
	private union {
		bool				_bool_;
		int				_int_;
		float				_float_;
		String			_string_;
		char				_char_;
		
		Tuple*			tuple_;
		Directive*		directive_;
		AssignExpr*	assignExpr_;
	}
	
	Type					type = Type.None;
	
	
	bool _bool() {
		assert (Type.Bool == type);
		return _bool_;
	}
	
	int _int() {
		assert (Type.Int == type);
		return _int_;
	}

	float _float() {
		assert (Type.Float == type);
		return _float_;
	}

	char[] _string() {
		assert (Type.String == type);
		return _string_();
	}
	char _char() {
		assert (Type.Char == type);
		return _char_;
	}

	Tuple tuple() {
		assert (Type.Tuple == type);
		return *tuple_;
	}

	Directive directive() {
		assert (Type.Directive == type);
		return *directive_;
	}

	AssignExpr assignExpr() {
		assert (Type.AssignExpr == type);
		return *assignExpr_;
	}


	T get(T)() {
		static if (is(T == bool)) {
			assert (Type.Bool == type);
			return _bool_;
		}
		else static if (is(T == int)) {
			if (Type.Int == type) return _int_;
			else if (Type.Float == type) return cast(int)_float_;
			else assert (false);
		}
		else static if (is(T == float)) {
			if (Type.Int == type) return cast(float)_int_;
			else if (Type.Float == type) return _float_;
			else assert (false);
		}
		else static if (is(T == char[])) {
			switch (type) {
				case Type.Bool: return _bool_ ? "true" : "false";
				case Type.Int: return Integer.toString(_int_);
				case Type.Float: return Float.toString(_float_);
				case Type.String: return _string_();
				default: assert (false);
			}
		}
		else static if (is(T == char)) {
			assert (Type.Char == type);
			return _char_;
		}
		else static if (is(T == Tuple)) {
			assert (Type.Tuple == type);
			return *tuple_;
		}
		else static assert (false, `not supported`);
	}

	
	char[] toString() {
		switch (type) {
			case Type.Bool: return _bool_ ? "true" : "false";
			case Type.Int: return Integer.toString(_int_);
			case Type.Float: return Float.toString(_float_) ~ "f";
			case Type.String: return "'" ~ _string_() ~ "'";
			case Type.Char: return "`" ~ _char_ ~ "`";
			case Type.Tuple: return "tuple[ " ~ tuple_.toString ~ " ]";
			case Type.Directive: return "directive("~directive_.type~")[ " ~ directive_.data.toString ~ " ]";
			case Type.AssignExpr: return "assign("~assignExpr_.lhs.toString ~ ", " ~ assignExpr_.rhs.toString ~ ")";
			case Type.None: return "(none)";
			default: assert (false);
		}
	}
	

	void dispose() {
		switch (type) {
			case Type.String:
				_string_.free();
				type = Type.None;
				break;
				
			case Type.Tuple:
				if (tuple_ !is null) {
					tuple_.dispose();
					cFree(tuple_);
					tuple_ = null;
					type = Type.None;
				}
				break;
				
			case Type.Directive:
				if (directive_ !is null) {
					directive_.dispose();
					cFree(directive_);
					directive_ = null;
					type = Type.None;
				}
				break;

			case Type.AssignExpr:
				if (assignExpr_ !is null) {
					assignExpr_.dispose();
					cFree(assignExpr_);
					assignExpr_ = null;
					type = Type.None;
				}
				break;
				
			case Type.None:
				assert (false);
				break;
				
			default: break;
		}
	}
}


template DataType(T) {
	static if (is(T == bool)) const Data.Type DataType = Data.Type.Bool;
	else static if (is(T == int)) const Data.Type DataType = Data.Type.Int;
	else static if (is(T == float)) const Data.Type DataType = Data.Type.Float;
	else static if (is(T == char[])) const Data.Type DataType = Data.Type.String;
	else static if (is(T == char)) const Data.Type DataType = Data.Type.Char;
	else static if (is(T == Tuple)) const Data.Type DataType = Data.Type.Tuple;
	else static assert (false, `not supported`);
}
	


// ------------------------------------------------------------------------------------------------------------------------------------------------------------


struct AssignExpr {
	Data	lhs;
	Data	rhs;
	
	
	void dispose() {
		lhs.dispose();
		rhs.dispose();
	}
}


// ------------------------------------------------------------------------------------------------------------------------------------------------------------


struct String {
	char[]	str() {
		return (cast(char*)(data+size_t.sizeof))[0..this.length];
	}
	alias str opCall;
	
	
	size_t	length() {
		return *cast(size_t*)data;
	}
	
	
	void alloc(size_t len) {
		free();
		data = cast(ubyte*)cMalloc(len+size_t.sizeof);
		*cast(size_t*)data = len;
	}
	
	
	void free() {
		if (data !is null) {
			cFree(data);
			data = null;
		}
	}
	
	
	private {
		ubyte*	data;
	}
}


// ------------------------------------------------------------------------------------------------------------------------------------------------------------


struct Tuple {
	Data* opIndex(int i) {
		return &data_[0..numData][i];
	}
	
	uint length() {
		return numData;
	}
	
	Data[] data() {
		return data_[0..numData];
	}
	
	void removeFirst() {
		assert (numData >= 1);
		for (int i = 1; i < numData; ++i) {
			data_[i-1] = data[i];
		}
		--numData;
	}
	
	void opCatAssign(Tuple* f) {
		Data d;
		d.tuple_ = f;
		d.type = d.type.Tuple;
		opCatAssign(d);
	}

	void opCatAssign(Directive* f) {
		Data d;
		d.directive_ = f;
		d.type = d.type.Directive;
		opCatAssign(d);
	}

	void opCatAssign(AssignExpr* f) {
		Data d;
		d.assignExpr_ = f;
		d.type = d.type.AssignExpr;
		opCatAssign(d);
	}

	void opCatAssign(Data d) {
		data_.append(d, &numData);
	}
	
	
	char[] toString() {
		char[] res;
		if (length >= 1) res ~= data_[0].toString;
		if (numData > 1) {
			foreach (d; data_[1..numData]) {
				res ~= ", " ~ d.toString;
			}
		}
		return res;
	}
	

	void dispose() {
		foreach (ref d; data_[0..numData]) {
			d.dispose();
		}
		data_.free();
		numData = 0;
	}
	
	
	private Data toData() {
		Data res;
		res.type = Data.Type.Tuple;
		res.tuple_ = cast(Tuple*)cMalloc(Tuple.sizeof);
		*res.tuple_ = *this;
		return res;
	}
	
	
	private {
		Data[]	data_;
		uint		numData;
	}
}


// ------------------------------------------------------------------------------------------------------------------------------------------------------------


struct Directive {
	Data		data;
	char[]	type;
	
	
	void dispose() {
		data.dispose();
		type.free();
	}
}


// ------------------------------------------------------------------------------------------------------------------------------------------------------------


class CfgLoader {
	char[] specialChars = `!@#$%^&:?+-*/.~`;
	void delegate(Tuple, void delegate(Data, bool commit) addData)[char[]] directiveHandlers;
	
	
	Tuple load(IReader reader) {
		this.getc = {
			char x;
			try {
				do {
					reader(x);
				} while ('\r' == x);
			} catch (IOException e) {
				return char.init;
			}
			
			return x;
		};
		
		Tuple dataTuple;
		parseData(&dataTuple, ``);
		return dataTuple;
	}
	
	
	this (bool stdDirectives = true) {
		if (stdDirectives) {
			directiveHandlers[`file`] = &fileDirectiveHandler;
			directiveHandlers[`eval`] = &evalDirectiveHandler;
			directiveHandlers[`mk-id`] = &mkIdDirectiveHandler;
			directiveHandlers[`id`] = &idDirectiveHandler;
			directiveHandlers[`inline`] = &inlineDirectiveHandler;
			directiveHandlers[`inline-id`] = &inlineIdDirectiveHandler;
			directiveHandlers[`flatten`] = &flattenDirectiveHandler;
		}
	}
	
	
	private {
		void fileDirectiveHandler(Tuple data, void delegate(Data, bool commit) addData) {
			if (data.length != 1) error(`The 'file' directive should only have one param`);
			if (data[0].type != Data.Type.String) error(`The argument to the 'file' directive should be a string`);
			char[] pathStr = data[0]._string_();
			scope path = new FilePath(pathStr);
			if (!path.exists) error(`The file path '` ~ pathStr ~ `' doesn't exist`);
			
			scope f = new File(path);
			addData(cast(Data)cast(char[])f.read(), true);
		}
		

		void evalDirectiveHandler(Tuple data, void delegate(Data, bool commit) addData) {
			if (data.length != 1) error(`The 'eval' directive should only have one param`);
			if (data[0].type != Data.Type.String) error(`The argument to the 'eval' directive should be a string`);
			
			auto c = new GrowBuffer;
			auto r = new Reader(c);
			c.write(cast(void[])data[0]._string_());
			data.dispose();
				
			scope loader = new CfgLoader;
			auto parsed = loader.load(r);
			
			foreach (ref f; parsed.data) {
				addData(f, true);
			}
		}
		
		
		void mkIdDirectiveHandler(Tuple data, void delegate(Data, bool commit) addData) {
			//Stdout.formatln(`mkIdDirectiveHandler given {}`, data.toString);
			
			if (0 == data.length) error(`The 'mk-id' directive should be given params`);
			auto first = data[0];
			
			char[]	name;
			int		from = 0;
			
			if (Data.Type.Tuple == first.type) {		// multiple values at the first line
				name = (*first.tuple_)[0].get!(char[]);
				
				if (2 == first.tuple.length) {
					// special case. remove the second item from the tuple
					data.data_[0] = *first.tuple[1];
				} else {
					data[0].tuple_.removeFirst();
				}
			} else {
				if (Data.Type.String != first.type) error(`The first param to 'mk-id' should be a string`);
				//one value on the first line
				name = first.get!(char[]);
				from = 1;
			}
			
			Data*[] res;
			for (; from < data.length; ++from) {
				res ~= data[from];
			}
			
			idsValues[name] = res;
		}

		void idDirectiveHandler(Tuple data, void delegate(Data, bool commit) addData) {
			if (data.length != 1) error(`The 'id' directive should only have one param`);
			if (data[0].type != Data.Type.String) error(`The argument to the 'id' directive should be a string`);
			
			auto val = idsValues[data[0].get!(char[])];
			
			foreach (v; val) {
				addData(*v, true);
			}
		}


		void inlineDirectiveHandler(Tuple data, void delegate(Data, bool commit) addData) {
			foreach (p; data.data) {
				addData(p, false);
			}
		}


		void inlineIdDirectiveHandler(Tuple data, void delegate(Data, bool commit) addData) {
			if (data.length != 1) error(`The 'inline-id' directive should only have one param`);
			if (data[0].type != Data.Type.String) error(`The argument to the 'inline-id' directive should be a string`);
			
			auto val = idsValues[data[0].get!(char[])];
			
			foreach (v; val) {
				addData(*v, false);
			}
		}


		void flattenDirectiveHandler(Tuple data, void delegate(Data, bool commit) addData) {
			Stdout.formatln(`flatten given: {}`, data.toString);
			//Tuple data = data_[0].tuple;
			
			foreach (p; data.data) {
				if (Data.Type.Tuple == p.type) {
					foreach (p2; p.tuple.data) {
						addData(p2, false);
					}
				} else {
					addData(p, false);
				}
			}
		}

		
		bool parseData(Tuple* result, char[] closingTokens)
		in {
			assert (result !is null);
		}
		body {
			Tuple	curTuple;
			
			void commitTuple() {
				if (curTuple.length > 1) {
					debug (UtilsCfg) Stdout.formatln(`* commiting a tuple`);
					*result ~= stackToHeap(curTuple);
					curTuple.data_ = null;
				} else if (curTuple.length == 1) {
					debug (UtilsCfg) Stdout.formatln(`* commiting a single item`);
					*result ~= *curTuple[0];
					curTuple.data_.free();
				}

				curTuple.numData = 0;
			}
			
			tokenIter: while (true) {
				if (skipComment || skipWhite) {
					continue;
				}
				
				char firstChar = window(0);
				if (char.init == firstChar) break;
				
				if ('(' == firstChar) {
					// directive
					debug (UtilsCfg) Stdout.formatln(`parsing a directive`);

					discardWindow(1);
					while (skipComment || skipWhite) {}
					
					char[] name; {
						char[] tmp;
						bool freetmp;
						if (parseString(&tmp, &freetmp)) {		// tmp is sliced from the window
							name.alloc(tmp.length);
							name[] = tmp[];
							if (freetmp) tmp.free();
						} else {
							// hmmm i might actually have directives without names, e.g. in order to nest them
							// error(`Directive name expected`);
						}
					}
					
					/+if (!skipWhite) {
						error(`Whitespace expected after directive name`);
					}+/skipWhite();
					
					Tuple directiveBody;
					if (!parseData(&directiveBody, ")")) {
						error(`Directive body expected`);
					}
					discardWindow(1);
					
					if (name in directiveHandlers) {
						auto handler = directiveHandlers[name];
						
						handler(directiveBody, (Data d, bool commit) {
							curTuple ~= d;
							if (commit) commitTuple();
						});
					} else {
						Directive directive;
						directive.type = name;
						directive.data = directiveBody.toData();
						curTuple ~= stackToHeap(directive);
					}
					
					continue;
				}
				
				if ('{' == firstChar) {
					// tuple
					debug (UtilsCfg) Stdout.formatln(`parsing a nested tuple`);

					discardWindow(1);
					
					Tuple tupleBody;
					if (!parseData(&tupleBody, "}")) {
						error(`Tuple body expected`);
					}
					discardWindow(1);

					curTuple ~= stackToHeap(tupleBody);
					continue;
				}
								
				if (closingTokens.contains(firstChar)) {
					// end of data decl
					debug (UtilsCfg) Stdout.formatln(`parsing an end of data decl`);
					break;
				}

				// must be after the closingTokens.contains stuff.
				if (";,\n".contains(firstChar)) {
					// end of tuple
					debug (UtilsCfg) Stdout.formatln(`parsing an end of tuple`);

					discardWindow(1);
					commitTuple();
					continue;
				}

				if ('=' == firstChar) {
					// assign expr
					debug (UtilsCfg) Stdout.formatln(`parsing an assign expr`);
					
					discardWindow(1);

					if (0 == curTuple.length) {
						error(`Assign expression with no left-hand-side`);
					}
					
					AssignExpr assignExpr;
					
					// take over the curTuple
					if (1 == curTuple.length) {
						assignExpr.lhs = *curTuple[0];
						curTuple.data_[0] = Data.init;
						curTuple.numData = 0;
					} else {
						assignExpr.lhs = curTuple.toData();
						curTuple = Tuple.init;
					}					
					
					while (skipComment || skipWhite) {}
					
					debug (UtilsCfg) Stdout.formatln(`assign expr {{`);
					Tuple rhsTuple;
					parseData(&rhsTuple, closingTokens.contains(')') ? ");,\n" : closingTokens.contains('}') ? "};,\n" : ";,\n");
					debug (UtilsCfg) Stdout.formatln(`} assign expr`);
					
					if (0 == rhsTuple.length) {
						error(`Assign expression with no right-hand-side`);
					}

					assert (1 == rhsTuple.length, rhsTuple.toString);
					
					assignExpr.rhs = *rhsTuple[0];
					curTuple ~= stackToHeap(assignExpr);
					
					continue;
				}
				
				foreach (sc; specialChars) {
					if (sc == firstChar) {
						discardWindow(1);
						curTuple ~= Data(sc);
						continue tokenIter;
					}
				}
				
				{
					debug (UtilsCfg) Stdout.formatln(`parsing a field `);
					Data field;
					if (parseField(&field)) {
						curTuple ~= field;
						continue;
					}
				}
				
				error(`Invalid token: '` ~ firstChar ~ `'`);
			}
			
			commitTuple();
			
			return true;
		}
		
		
		bool parseField(Data* field)
		in {
			assert (field !is null);
		}
		body {
			if (parseBool(&field._bool_)) {
				debug (UtilsCfg) Stdout.formatln(`parsed a bool: {}`, field._bool_);
				field.type = Data.Type.Bool;
				return true;
			}
			if (parseFloat(&field._float_)) {
				debug (UtilsCfg) Stdout.formatln(`parsed a float: {}`, field._float_);
				field.type = Data.Type.Float;
				return true;
			}
			if (parseInt(&field._int_)) {
				debug (UtilsCfg) Stdout.formatln(`parsed an int: {}`, field._int_);
				field.type = Data.Type.Int;
				return true;
			}
			char[] str;
			bool freetmp;
			if (parseString(&str, &freetmp)) {
				debug (UtilsCfg) Stdout.formatln(`parsed a string: '{}'`, str);
				field._string_ = String.init;
				field._string_.alloc(str.length);
				field._string_()[] = str[];
				field.type = Data.Type.String;
				if (freetmp) str.free();
				return true;
			}
			return false;
		}
		

		bool parseBool(bool* b) {
			for (int i = 0; i < 4; ++i) {
				if (window(i) != "true"[i]) goto parseFalse;
			}
			*b = true;
			discardWindow(4);
			return true;
			
		parseFalse:
			for (int i = 0; i < 5; ++i) {
				if (window(i) != "false"[i]) goto parseEnd;
			}
			*b = false;
			discardWindow(5);
			return true;

			parseEnd:
			return false;
		}
		

		bool parseInt(int* i) {
			char c0 = window(0);
			if (c0 == '-' || (c0 >= '0' && c0 <= '9')) {
				int end = 1;
				char c;
				while (c = window(end), c != char.init && !(" \t;,)}".contains(c)) && ".0123456789abcdefABCDEFxXp-+eEL".contains(c)) {
					++end;
				}

				if ('-' == c0 && 1 == end) {
					return false;
				}
				
				char[] s = window(0, end);

				uint ate;
				*i = Integer.parse(s, 0, &ate);
				
				if (ate == s.length) {
					discardWindow(ate);
					return true;
				}
				
				return false;
			}
			return false;
		}


		bool parseFloat(float* f) {
			int end = 0;
			char c;
			while (c = window(end), c != char.init && !(" \t;,)}".contains(c)) && ".0123456789abcdefABCDEFxXp-+eEL".contains(c)) {
				++end;
			}
			if (0 == end) return false;
			
			char[] s = window(0, end);
			debug (UtilsCfg) Stdout.formatln(`parseFloat.s == '{}'`, s);
			
			uint ate;
			*f = Float.parse(s, &ate);

			{	// check if it should be parsed as an int instead
				uint ateInt;
				Integer.parse(s, 0, &ateInt);
				if (ateInt >= ate) {
					return false;
				}
			}

			if (ate == s.length) {
				discardWindow(ate);
				return true;
			}
			
			return false;
		}
		
		
		// str will be a slice into the window
		bool parseString(char[]* str, bool* free) {
			char c0 = window(0);
			debug (UtilsCfg) Stdout.formatln(`parseString.c0 == '{}'`, c0);
			
			if ('\'' == c0) {	// single-quoted string
				if ('\'' == window(1) && '\'' == window(2)) {	// verbatim string
					*free = false;
					discardWindow(3);

					for (int i = 0; true; ++i) {
						if (window(i) == char.init) break;
						
						// end of the string
						if (window(i) == '\'' && window(i+1) == '\'' && window(i+2) == '\'') {
							*str = window(0, i);
							discardWindow(i + 3);
							return true;
						}
					}
					return false;
				}


				*free = true;
				discardWindow(1);
				
				uint stringLen;
				void addChar(char c) {
					(*str).append(c, &stringLen);
				}
				
				bool escaped = false;
				while (true) {
					if (char.init == window(0)) break;
					
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
						if (escaped) {
							switch (window(0)) {
								case '?': addChar('\?'); break;
								case 'a': addChar('\a'); break;
								case 'b': addChar('\b'); break;
								case 'f': addChar('\f'); break;
								case 'n': addChar('\n'); break;
								case 'r': addChar('\r'); break;
								case 't': addChar('\t'); break;
								case 'v': addChar('\v'); break;
							}
							escaped = false;
						} else {
							addChar(window(0));
						}
						
						discardWindow(1);
					}
				}
				
				(*str).realloc(stringLen);
				return true;
			} else if (isalpha(c0)) {
				*free = false;
				int end = 1;
				char c;
				while (c = window(end), c != char.init && (isalnum(c) || '-' == c)) {
					++end;
				}
				
				*str = window(0, end);
				discardWindow(str.length);
				return true;
			}
			return false;
		}
		
		
		// return true if any comment was found and skipped
		bool skipComment() {
			bool result = false;
			
			void skipLine() {
				while (window(0) != char.init) {
					if (window(0) == '\n') {
						return;
					}
					
					discardWindow(1);
				}
			}
			
			void skipBlock() {
				int depth = 1;
				
				while (window(0) != char.init) {
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
			}
			
			while (window(0) != char.init) {
				if (window(0) == '/') {
					if (window(1) == '/') {
						skipLine();
						return true;
					}
					
					if (window(1) == '+') {
						result = true;
						discardWindow(2);
						skipBlock();
						continue;
					}
				}
				
				break;
			}
			
			return result;
		}
		
		
		// return true if any whitespace was found and skipped. \n is not whitespace, it's a data declaration separator like , and ;
		bool skipWhite() {
			bool res = false;
			for (char c = window(0); " \t".contains(c); c = window(0)) {
				res = true;
				discardWindow(1);
			}
			return res;
		}
		

		// ----

		void error(char[] errStr) {
			int idx = errorWndSize + _latestWindowIdx - _window.length;

			discardWindow(0);
			
			char[] errorCtxt;
			
			foreach (i, c; errorWnd) {
				if (c == char.init) continue;
				if (i == idx) {
					errorCtxt ~= "\032";
				}
				if (i == idx+1) {
					errorCtxt ~= "\033";
				}
				errorCtxt ~= c;
			}
			
			if (idx+1 == errorWnd.length) {
				errorCtxt ~= "\033";
			}

			for (int i = 0; i < errorWndSize; ++i) {
				char c = window(i);
				if (c == char.init) continue;
				errorCtxt ~= c;
			}
			
			throw new Exception(errStr ~ \n ~ "Context:" ~ \n ~ errorCtxt ~ \n);
		}


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
		
		
		char window(int idx) {
			// make sure we have this char
			// track the lines and character indices of beginning and end of window
			
			assert (idx >= 0);
			_latestWindowIdx = idx;
			
			if (_windowBuffer.length <= idx + _windowStart) {
				_windowBuffer.realloc(idx + _windowStart + 1);
			}
			
			if (_window.length <= idx) {
				int fillFrom = _window.length;
				_window = _windowBuffer[_windowStart .. _windowStart + idx + 1];
				
				foreach (inout c; _window[fillFrom .. $]) {
					c = getc();
					
					// shift the errorWnd
					foreach (i, x; errorWnd[1..$]) {
						errorWnd[i] = x;
					}
					errorWnd[$-1] = c;
				}
			}
			
			return _window[idx];
		}
		
		
		char[] window(int start, int postEnd) {
			return _window[start .. postEnd];
		}


		char[]	_window;
		int		_windowStart;
		char[]	_windowBuffer;
		int		_latestWindowIdx;
		
		const int					errorWndSize = 20;
		char[errorWndSize]	errorWnd;
		Data*[][char[]]			idsValues;
	
	
		char delegate()	getc;
	}
}


// ------------------------------------------------------------------------------------------------------------------------------------------------------------


private T* stackToHeap(T)(T foo) {
	T* res = cast(T*)cMalloc(T.sizeof);
	*res = foo;
	return res;
}


// ------------------------------------------------------------------------------------------------------------------------------------------------------------


final class DataHash {
	this (Tuple tuple) {
		foreach (ref f; tuple.data) {
			if (f.type != Data.Type.AssignExpr) continue;
			
			auto lhs = &f.assignExpr_.lhs;
			if (lhs.type != Data.Type.String) continue;

			auto rhs = &f.assignExpr_.rhs;
			hash[lhs._string_()] = rhs;
		}
	}
	
	
	~this() {
		hash.clear();
	}
	
	
	Data* opIndex(char[] key) {
		assert (hash.contains(key), `No such field`);
		return hash[key];
	}


	Tuple tuple(char[] key) {
		auto data = opIndex(key);
		return (*data).get!(Tuple);
	}
	
	
	T accessor(T)(char[] key, int idx = 0) {
		auto data = opIndex(key);
		assert (data !is null);
		if (Data.Type.Tuple == data.type) {
			return (*data.tuple_)[idx].get!(T);
		} else {
			return (*data).get!(T);
		}
	}
	
	alias accessor!(bool)		_bool;
	alias accessor!(int)			_int;
	alias accessor!(float)		_float;
	alias accessor!(char[])	_string;
	
	
	private SortedAA!(char[], Data*, false, Mallocator) hash;
}
