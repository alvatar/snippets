import cgi.cgi;
import bottledAJAX;

void main(char[][] argv)
{
	static const char[] code = ClientCode!(Foo,"/cgi-bin/"~__FILE__[0..$-2], "bar","pig", "cat");
	CGI cgi = new TextCGI(argv);

	if("" == cgi.Value("classname"))
		cgi.writef("%s", code);
	else
		Process!(Foo, "bar", "pig", "cat")(cgi,new Foo);


	pragma(msg, \n~ code ~\n);
}
class Foo{

	int bar(int i, char[] j)
	{
		debug cgi_.writef("%d, %s\n", i, j);
		return i + j.length; 
	}
	char[] pig(float f) { return "hello"; }

	char[] cat(char[] a, char[] b){return a~b;}
}