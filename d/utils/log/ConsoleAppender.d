module xf.utils.log.ConsoleAppender;
import xf.utils.log.StreamAppender;
import  tango.io.Console;

class ConsoleAppender : StreamAppender {
	mixin RegisterToList;
	
	this(){
		super (Cout.stream);
    }
}