module xf.utils.log.StreamAppender;
public import xf.utils.log.Appender;
import tango.io.model.IConduit;
import tango.core.Exception;
import tango.time.Clock;
import tango.text.locale.Locale;
import tango.text.convert.Format : sprint = Format;
//import tango.text.convert.Sprint;


public class StreamAppender: Appender{
	protected OutputStream stream;
	
	version(Win32){
		protected const char[] Eol = "\r\n";
	} else {
		protected const char[] Eol = "\n";
	}
	
	protected char[] _tab="";
	
	static this() {
		layout = new Locale;
	}
	
	public this() {
	}
	
	public this(OutputStream stream){
		setStream(stream);
	}
	
	public void setStream(OutputStream stream){
		//if(sprint is null) sprint = new Sprint!(char);
		this.stream=stream;
	}
	
	override synchronized void tab(){
		_tab~="    ";
	}
	
	override synchronized void utab(){
		if(_tab.length>0){
			_tab=_tab[0..$-4];
		}
	}
	
	override void newline(){
		synchronized (stream){
			stream.write(Eol);
		}
	}
	
	protected final char[] getTimeStamp(){
		return layout (timePattern, Clock.now);
	}
	
	protected void printTime(){
		if(printtime){
			stream.write("["~getTimeStamp ~"] ");
		}
	}
	
	protected void printLevels(char[] levelname){
		if(printlevels){
			stream.write(sprint("{0,6}: ", levelname));
		}
	}
	
	protected void printLoggerName(char[] loggername) {
		if(printloggername){
			stream.write(sprint("<{0,10}>", loggername));
		}
	}
	
	override void write(char[] msg, byte max, byte level, char[] levelname, char[] loggername){
		try {
			synchronized (stream){
				printLoggerName(loggername);
				printTime;
				printLevels(levelname);
				
				stream.write(_tab);
				stream.write(msg);
				stream.write(Eol);
				stream.flush;
			}
		} catch (IOException e) {
		}
	}
	
	override bool setOption(char[] option, char[] value){ 
		switch(option){
			case "printlevels": printlevels = (value == "true") ? true: false; return true;
			case "printtime" : printtime = (value == "true")? true: false; return true;
			case "printloggername" : printloggername = (value == "true")? true: false; return true;
			case "timepattern" : timePattern = value.dup; return true;
			default: return false;
		}
	}
	
	protected bool printlevels = false;
	protected bool printtime = false;
	protected bool printloggername = false;
	protected char[]  timePattern = "{:HH:mm:ss}";
	private static Locale layout;
	//protected Sprint!(char) sprint;		
}