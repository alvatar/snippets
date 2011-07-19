module xf.utils.log.ColorConsoleAppender;
import xf.utils.log.ConsoleAppender;
import xf.utils.log.StreamAppender;
import tango.text.Util;
version (Windows) {
	import tango.sys.win32.UserGdi;
}

final public class ColorConsoleAppender : ConsoleAppender {
	mixin RegisterToList;
	
	this() {
		super();
		version(Windows) {
			colors["Fatal"] = FOREGROUND_INTENSITY |FOREGROUND_RED;
			colors["Error"] = FOREGROUND_INTENSITY |FOREGROUND_RED;
			colors["Warn"] = FOREGROUND_INTENSITY | FOREGROUND_RED | FOREGROUND_GREEN;
			colors["Info"] =  FOREGROUND_INTENSITY |FOREGROUND_GREEN;
			colors["Default"]= FOREGROUND_INTENSITY | FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_BLUE;
		}
	}
	
	static void setColor(ushort color){
		version(Windows) {
			SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), color);
		}
	}
	
	override void write(char[] msg, byte max, byte level, char[] levelname, char[] loggername){
		
		synchronized(stream) {
			if((levelname in colors) !is null){
					setColor(colors[levelname]);
			} else {
				setColor(colors["Default"]);
			}
			super.write(msg,max,level,levelname, loggername);
			setColor(colors["Default"]);
		}
	}
	
	private int toConst(char[] c){
		version(Windows) {
			switch(c){
				case "FOREGROUND_BLUE": return FOREGROUND_BLUE;
				case "FOREGROUND_GREEN": return FOREGROUND_GREEN;
				case "FOREGROUND_RED": return FOREGROUND_RED;
				case "FOREGROUND_INTENSITY": return FOREGROUND_INTENSITY;
				case "BACKGROUND_BLUE": return BACKGROUND_BLUE;
				case "BACKGROUND_GREEN" : return BACKGROUND_GREEN;
				case "BACKGROUND_RED" : return BACKGROUND_RED;
				case "BACKGROUND_INTENSITY": return BACKGROUND_INTENSITY;
				default : assert(false); return 0;
			}
		} else {
			return 0;
		}
	}
	
	override bool setOption(char[] option, char[] value){ 
		if(super.setOption(option,value)) return true;
		switch(option) {
			case "color": auto fields = split(value,":"); colors[fields[0]] = 0;  auto fields2 = split(fields[1],"|"); foreach(field; fields2) colors[fields[0]] |=toConst(field); return true;
			default: return false;
		}
	}
	
	private ushort[char[]] colors;
}