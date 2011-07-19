module xf.utils.log.Meta;

template genList(T ...){
	static if(T.length==1){
		const char[] genList = T[0] ;
	} else {
		const char[] genList = T[0] ~ ", " ~ genList!(T[1..$]);
	}
}

template genEnum(T ...){
	const char[] genEnum = "enum Levels { " ~ genList!(T) ~ ", None }";
}

template genStringList(T ...){
	static if(T.length==1){
		const char[] genStringList = `"` ~ T[0]~ `"` ;
	} else {
		const char[] genStringList = `"` ~ T[0]~ `"` ~ ", " ~ genStringList!(T[1..$]);
	}
}

template genLevelsNamesArray(T ...){
	const char[] genLevelsNamesArray = "private static const char[][] levelNames = [" ~ genStringList!(T) ~ "];";
}

import xf.utils.Meta;

template genAppendCode(T...){
	static if(T.length==0){
		const char[] genAppendCode = "";
	} else {
		const char[] genAppendCode = "typeof(this) " ~ toLower(T[0][0]) ~ T[0][1..$] ~ " (T ...) (lazy char[] fmt, lazy T params) { if(work) { lastLevel = Levels." ~ T[0] ~ `; if ( minLevel <= lastLevel ) { char[] buf = sprint(fmt, params); foreach(appender; appenders){ if(appender.level <= Levels.`~ T[0] ~ ` ) appender.appender.write(buf, Levels.`~ T[0] ~ `, Levels.None, levelNames[Levels.`~ T[0] ~ `], name); } } } return _this; } ` ~ genAppendCode!(T[1..$]);
	}
	}