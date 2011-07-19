module xf.utils.log.Appender;

//import tango.io.Stdout;
import tango.text.Util;

AppenderList appenderList() {
	if(AppenderList.list is null) AppenderList.list = new AppenderList;
	return AppenderList.list;
}

final class AppenderList {
	private char[][char[]]  appenders;
	public static AppenderList list;
	
	package void register(char[] name){
		auto fields= split(name, ".");
		//Stdout("Adding ", name, fields[$-1]).newline;
		appenders[fields[$-1].dup] = name.dup;
	}
	
	public Appender getAppender(char[] name){
		if((name in presets)!is null) {
			auto appender = appenderList.getAppender(presets[name].baseAppender);
			foreach(option; presets[name].options){
				auto result = appender.setOption(option);
				assert(result, option);
			}
			return appender;
		}
		return cast(Appender) ClassInfo.find(appenders[name]).create;
	}
	
	_PresetAppender[char[]] presets;
	bool _presetsLoaded=false;
	
    struct _PresetAppender {
    	char[] baseAppender;
    	char[][] options;
   }
}

template RegisterToList () {
	static this() {
		appenderList.register(this.classinfo.name);
	}
}

public abstract class Appender {
	abstract void tab();
	abstract void utab();
	abstract void newline();
	
	abstract void write(char[] msg, byte max, byte level, char[] levelname, char[] loggername);
	
	final bool setOption(char[] option){
		auto fields = split(option,":");
		assert(fields.length >= 2);
		return setOption(fields[0] , join(fields[1..$],":"));
	}
	
	bool setOption(char[] option, char[] value){	return false; }
	void update() { }
	
}