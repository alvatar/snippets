module xf.utils.log.ConsoleErrorAppender;
import xf.utils.log.StreamAppender;
import  tango.io.Console;

final public class ConsoleErrorAppender : StreamAppender {
	mixin RegisterToList;
	
	this(){
		super (Cerr.stream);
    }
}