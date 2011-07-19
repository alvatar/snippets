module xf.utils.log.LogWalkerAppender;

import xf.utils.log.StreamAppender;
import tango.io.device.FileConduit;
import tango.core.Atomic;
import tango.core.Thread;

final class LogWalkerAppender : StreamAppender{
	protected FileConduit fc;
	protected static Atomic!(int) counter;
	
	static this(){
		counter.store(0);
	}
	
	mixin RegisterToList;
	
	enum CreationOption {
		WriteExisting,
		WriteCreate,
		WriteAppending
	}
		
	public this(char[][] levelnames, char[] logname){
		tango.io.Stdout.Stdout(logname).newline;
		filename = logname~".lw";
		 
		fc= new FileConduit(filename, FileConduit.WriteCreate);
		
		super(fc.output);
		
		auto fc2 = new FileConduit(logname~".lwc", FileConduit.WriteCreate);
		
		void write(char[] what) {
			fc2.output.write(what);
		}
		
		write(sprint( "logname = '{}'", logname));
		write(Eol);
		foreach( index, levelname; levelnames) {
			write( "levelname = {");
			write(Eol);
			write( sprint("	name = '{}'",  levelname));
			write(Eol);
			write( sprint("	level = {}", index));
			write(Eol);
			write("}");
			write(Eol);
		}
		
		fc2.close;
	}
	
	override bool setOption(char[] option, char[] value){
		assert(false, "this appender should not be configured");
		return false;
	}
	
	override void update() { 
		assert(filename);
		if( _override ) {
			fc =new FileConduit(filename, FileConduit.WriteCreate);
		} else {
			fc = new FileConduit(filename, FileConduit.WriteAppending);
		}
		
		setStream(fc.output);
	}
	
	override void write(char[] msg, byte max, byte level, char[] levelname, char[] loggername){
		synchronized (stream){
			stream.write(sprint( "{}|{}|{}|{}|{}", loggername,  getTick, Thread.getThis, levelname, msg));
			stream.write(Eol);
			stream.flush;
		}
	}
	
	private int getTick() {
		counter.increment();
		return counter.load();
	}
	
	void close(){
		fc.close;
	}
	
	protected char[] filename;
	protected bool _override;
	
}