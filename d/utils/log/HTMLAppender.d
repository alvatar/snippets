module xf.utils.log.HTMLAppender;

import xf.utils.log.StreamAppender;
import xf.utils.log.FileAppender;
import tango.io.device.File;
import tango.io.Stdout;
import tango.text.Util;

final class HTMLAppender : FileAppender{
	mixin RegisterToList;
	version(Win32){
		protected const char[] Eol = "<BR>\r\n";
	} else {
		protected const char[] Eol = "<BR>\n";
	}
	
	
	override synchronized void tab(){
		_tab~="&nbsp;&nbsp;&nbsp;&nbsp;";
	}
	
	override synchronized void utab(){
		if(_tab.length>0){
			_tab=_tab[0..$-24];
		}
	}
	
	
	override void write(char[] msg, byte max, byte level, char[] levelname, char[] loggername){
		msg = substitute(msg,"\n", "<BR>");
		msg = substitute(msg,"	", "&nbsp;&nbsp;&nbsp;&nbsp;");
		
		synchronized(stream) {
			char[] fontinfo = `<font color="`;
			if(levelname in colors){
				fontinfo ~= colors[levelname] ~ `">`;
			} else {
				fontinfo ~= colors["Default"] ~ `">` ;
			}
			
			char[] pattern;
			if(levelname in lines) {
				pattern = lines[levelname];
			} else {
				pattern = lines["Default"];
			}
			stream.write( Stdout.layout.convert(pattern, printtime?getTimeStamp:"",  printlevels?fontinfo~levelname~"</font>":"", fontinfo~_tab~msg~"</font>"));
			stream.flush;
		}
	}
	
	override void update() { 
		super.update();
		stream.write(`<HEAD>`);
		if(css !is null) {
			stream.write(`<link href='`~css~`' rel="stylesheet" type="text/css" >`);
		}
		stream.write(`</HEAD>`);
		if(header !is null) {
			stream.write(header);
		}
		
		if(("Fatal" in colors) is null) colors["Fatal"] = "#FF0000";
		if(("Error" in colors) is null) colors["Error"] = "#FF0000";
		if(("Warn" in colors) is null) colors["Warn"] = "#FFA500";
		if(("Info" in colors) is null) colors["Info"] =  "#0000CD";
		if(("Default" in colors) is null) colors["Default"]= "#000000";
		if(("Default" in lines) is null) lines["Default"] = " &nbsp;&nbsp;<p> [{0}] {1} : {2} </p> ";
	}
	
	override bool setOption(char[] option, char[] value){
		if ( super.setOption(option, value)) return true;
		switch(option){
			case "filename" : filename = value.dup; return true;
			case "override" :  _override = value=="true"?true:false; return true;
			case "css" : css = value.dup; return true;
			case "header": header = value.dup; return true;
			case "color": auto fields = split(value, ":"); colors[fields[0].dup] = fields[1].dup; return true;
			case "line": auto fields = split(value, ":"); lines[fields[0].dup] = join(fields[1..$],":").dup; return true;
			default: return false;
		}
	}
	
	private char[][char[]] colors;
	private char[] css;
	private char[] header;
	private char[][char[]] lines;
}
