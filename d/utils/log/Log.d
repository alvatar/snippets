module xf.utils.log.Log;

private {
	//import tango.text.convert.Sprint;
	import xf.utils.OldCfg;
	import xf.utils.log.Meta;
	import xf.utils.log.Appenders;
	import Path = tango.io.Path;
	debug import tango.util.log.Trace;
	import tango.io.Stdout;
}

template MLog(bool isStatic, T ...) {
	protected {
		void initLog() {
			debug {
				Trace.formatln("creating a log (%.*s)"\n, T.stringof);
			}
			//sprint = new Sprint!(char);
		}
		
		//Sprint!(char)	sprint;		
		import tango.text.convert.Format : sprint = Format;
		
	}
	
	mixin(genEnum!(T));
	mixin(genLevelsNamesArray!(T));
	
	void addAppender(Appender a, Levels l= cast(Levels)0){
		_Appender tmp;
		tmp.appender = a;
		tmp.level = l;
		appenders ~=  tmp;
		if(minLevel>l) minLevel=l;
	}
	
	void addAppender(Appender a, char[] levelname){
		_Appender tmp;
		tmp.appender = a;
		foreach(lnumb, name; levelNames){
			if(levelname == name){
				tmp.level = cast(Levels)lnumb;
				break;
			}
			if(lnumb== levelNames.length-1){
				assert(false, "no log level called " ~ levelname);
			}
		}
		appenders ~= tmp;
		if(minLevel>tmp.level) minLevel = tmp.level;
	}
	
	typeof(this) tab(){
		if(work){
			foreach(appender;appenders){
				appender.appender.tab;
			}			
		}
		return _this;
	}
	
	typeof(this) utab(){
		if(work) {
			foreach(appender;appenders){
				appender.appender.utab;
			}
		}
		return _this;
	}
	
	typeof(this) newline(Levels l){
		if(work){
			foreach(appender;appenders){
				if(appender.level <= l){
					appender.appender.newline;
				}
			}
		}	
		return _this;
	}
	
	typeof(this) newline(){
		if(work){
			foreach(appender;appenders){
				if(appender.level <= lastLevel){
					appender.appender.newline;
				}
			}
		}
		return _this;
	}
	
	mixin(genAppendCode!(T));
	//pragma(msg, genAppendCode!(T));
	
	bool load(char[] blockname){
		try {
			version (LogWalker){
				addAppender( new LWAppender(levelNames, name) );
			}
			
			if (configname is null || !Path.exists(configname)) {
				return false;
			}
			
			scope auto loader = new CfgLoader;
			loader.load(configname);
			
			scope auto map = loader.result();
			
			if(!appenderList._presetsLoaded){
				for(int i=0; i< map.count("PresetAppender"); ++i){
					try {
						auto block = map.child("PresetAppender", i);
						AppenderList._PresetAppender preset;
						preset.baseAppender = block.string_("base");
						for(int j=0; j< block.count("option"); ++j){
							preset.options ~=block.string_("option",j);
						}
						appenderList.
						presets[block.string_("name")]=preset;
					} catch (Exception e) {
						Stdout("Broken PresetAppender chunk").newline;
					}
				}
			}
			
			try {
				scope auto block = map.child(blockname); // add nonexisting block blah
				
				for(int i=0; i < block.count("Appender"); ++i){
					try {
						auto appenderCfg = block.child("Appender", i);
						
						if(appenderCfg.bool_("log")){
							auto name = appenderCfg.string_("name");
							auto appender = appenderList.getAppender(name);
							assert(appender !is null);
							
							for(int j = 0; j < appenderCfg.count("option") ; ++j){
								auto result = appender.setOption( appenderCfg.string_("option", j) );
								assert(result, appenderCfg.string_("option", j) );
							}
							appender.update;
							addAppender(appender, appenderCfg.string_("level"));
						}
					} catch (Exception e){
						Stdout("Broken Appender chunck").newline;
					}
				}
			} catch (Exception e) {
				Stdout.format("No {} chunk in debug cfg", blockname).newline;
				return false;
			}
			

			
			return true;
		} catch (Exception e) {
			version (LogDiagnostic) Stdout("No debug cfg").newline;
			return false;
		}
	}
	
	
	private final typeof(this) _this() {
		static if (isStatic) {
			return null;
		} else {
			return this;
		}
	}


	public {
		char[]	name ;
	}
	
	private Levels minLevel = Levels.None;
	private Levels lastLevel = Levels.None;
	private char[] configname = "debug.cfg";
	
	private struct _Appender{
		Appender appender;
		Levels level;
	}
	
	bool work = true;
	
	private _Appender[] appenders;
}


private class _Log(T ...) {
	mixin MLog!(false, T);
	
	this() {
		initLog();
	}
	
	~this() {
		// disconnect from appenders
	}
}

scope class Log(T ...) : _Log!(T) {
}


final class GlobalLog(char[] _name_, T ...) {
	static {
		mixin MLog!(true, T);
	}

	static this() {
		name = _name_;
		initLog();
	}
	
	
}


struct LogLevel {
	const char[] Trace = "Trace";
	const char[] Info = "Info";
	const char[] Warn = "Warn";
	const char[] Error = "Error";
	const char[] Fatal = "Fatal";
	
}

alias GlobalLog!(LogLevel.Trace, LogLevel.Info, LogLevel.Warn, LogLevel.Error, LogLevel.Fatal) GLog;

template LibraryLog (char[] name, char[] blockname, T ...) {
	alias GlobalLog!(blockname, T) myLog;
	
	//myLog bspLogger;
	mixin("myLog "~name~";");
 
	static this(){
		//bspLogger.load("BSPDebug");	
		mixin( name~`.load("`~blockname~`");`);
	}
}

version(Unittest){
	import xf.utils.log.ConsoleAppender;
	import xf.utils.log.ConsoleErrorAppender;
	import xf.utils.log.FileAppender;
	import xf.utils.log.BufferAppender;
}

unittest {
	auto scope logger = new Log!("Debug4","Debug3","Debug2","Debug1", LogLevel.Trace, LogLevel.Info);
	auto appender = new BufferAppender();
	logger.addAppender(appender, logger.Levels.Debug2);
	logger.debug4("debug 4 msg");
	logger.debug3("debug 3 msg");
	logger.debug2("debug 2 msg").tab;
	logger.debug1("debug 1 msg").utab;
	logger.info("hai!");
	assert(appender.getBuffer=="debug 2 msg\n    debug 1 msg\nhai!\n");
}