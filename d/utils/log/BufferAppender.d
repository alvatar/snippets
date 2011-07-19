module xf.utils.log.BufferAppender;
import xf.utils.log.Appender;

final public class BufferAppender : Appender{
	private int tabs;
	private char[] buffer;
	
	mixin RegisterToList;
	
	void tab(){
		tabs++;
	}
	
	void utab(){
		if(tabs>0) tabs--;
	}
	
	void newline(){
		buffer ~= '\n';
	}
	
	void write(char[] msg, byte max, byte level, char[] levelname, char[] loggername){
		for(int i=0; i< tabs; ++i){
			buffer~="    ";
		}
		buffer~=msg.dup;
		newline;
	}
	
	char[] getBuffer(){
		return buffer;
	}
}