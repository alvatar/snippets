module xf.utils.log.FileAppender;

import xf.utils.log.StreamAppender;
import tango.io.device.File;

public class FileAppender : StreamAppender{
	protected File fc;
	
	mixin RegisterToList;
	
	enum CreationOption {
		WriteExisting,
		WriteCreate,
		WriteAppending
	}
	
	public this() {
	}
	
	public this(char[] filename, CreationOption op= CreationOption.WriteCreate){
		this.filename = filename;
		_override = (op==CreationOption.WriteCreate);
		
		switch(op){
			case CreationOption.WriteExisting:  fc= new File(filename, File.WriteExisting); break;
			case CreationOption.WriteCreate:  fc= new File(filename, File.WriteCreate); break;
			case CreationOption.WriteAppending:  fc= new File(filename, File.WriteAppending); break;
		}
		
		super(fc.output);
	}
	
	override bool setOption(char[] option, char[] value){
		if ( super.setOption(option, value)) return true;
		switch(option){
			case "filename" : filename = value.dup; return true;
			case "override" :  _override = value=="true"?true:false; return true;
			default: return false;
		}
	}
	
	override void update() { 
		assert(filename);
		if( _override ) {
			fc =new File(filename, File.WriteCreate);
		} else {
			fc = new File(filename, File.WriteAppending);
		}
		
		setStream(fc.output);
	}
	
	void close(){
		fc.close;
	}
	
	protected char[] filename;
	protected bool _override;
}