module debugging;

/+++++++++++++++++++++++++++
This has some not exactly ideal bits:

-it won't work nicely unless you link with gcc and add the -rdynamic flag.
-accessing some of the posix stuff fubared my link (unresolved ModuleInfos) so
 I had to add the prototypes directly (yuck)
-it uses the c function dprintf that on some system does what I want (print to
 a low level file stream) and on some system does something totally different.
-it has the expected executable name and the location of addr2line hard coded

+/

import core.exception;

import core.stdc.errno;
import std.cstream;
import std.stdio;
import std.string;

import core.stdc.stdio;
import core.sys.posix.sys.wait;

//import core.sys.posix.fcntl;   // O_CREAT  O_TRUNC O_WRONLY open
//import core.sys.posix.unistd;  // fork dup2
extern(C)
{
	/// these are in core.sys.posix.* but for some reason importing them breaks the build
	enum O_CREAT        = 0100;
	enum O_TRUNC        = 01000;
	enum O_WRONLY       = 01;

	int open(in char*, int, ...);
	int fork();
	int dup2(int, int);

	int backtrace (void **__array, int __size) ;
	extern char **backtrace_symbols (void **__array, int __size) ;
	void backtrace_symbols_fd (void **__array, int __size,  int __fd) ;
}

// used to print directly to a file descriptor
version(linux) extern(C) int dprintf(int fd, const char *format, ...);
else static assert(false, "system not know to support dprintf");

enum nameOfExec = "btree";
enum nameOfAddr2line = "/usr/bin/addr2line";
pragma(msg, "Getting line numbers from a file named '"~nameOfExec~"' and passing them to '"~nameOfAddr2line~"'");

void DumpMessage(string test, string message, string file = __FILE__, int line = __LINE__)
{
	derr.writef("StackTrace: %s:%d \'%s\' failed: %s\n", file, line, test, message);
	
	int dmp = open("dmplog", O_WRONLY|O_TRUNC|O_CREAT, 0666);
	if(-1 == dmp) { writef("Log file failed %d\n", errno); return; }

	dprintf(dmp,"StackTrace: %s:%d \'%s\' failed: %s\n------------------\n".ptr, file.ptr, line, test.ptr, message.ptr);
	void* buffer[200];
	int i = backtrace(buffer.ptr, buffer.length);
	
	if(0 == fork())
	{
		// child
		string[] env = [null];

		int count = 6-3+i;
		auto argv = new string[count];
		argv[0] = nameOfAddr2line;
		argv[1] = "--exe=" ~ nameOfExec;
		argv[2] = "-f";
		argv[3] = "-C";
		argv[4] = "-s";
		argv[$-1] = null;

		for(int j = 1; j < i-2; j++) argv[j + 4] = format("%x\0", cast(int)buffer[j]);

		// fork/exec to addr2line
		dup2(dmp, 1);
		std.process.execve(argv[0], argv, env);
		core.stdc.stdlib.abort();
	}
	else
	{
		wait(null);
	}

	dprintf(dmp, "-------------\n".ptr);
	backtrace_symbols_fd(&buffer[1],i-3,dmp);
}

static this()
{
	setAssertHandler(
		function void(string file, int line, string msg)
		{
			DumpMessage("Assert", msg, file, line);
			throw new AssertError( msg, file, line );
		});
}
