/*
This program is distributed without any warranty of any kind!

Author: Benjamin Shropshire (shro8822drop_this at vandals dot uidaho edu)

License: Freeware

Please give credit where credit is due. Please don't remove this notice.

*/
module search;

import std.stdio;
import cgi.cgi;
import std.file;
import std.utf;
import std.stream;

const char[] 
	SOURCE_PATH = `/usr/include/phobos/`,
	DOC_PATH = `/var/www/html/dmd/d/`,
	HTTP_PATH = "/var/www/html"
	;
const char[][] roots = [
	SOURCE_PATH, 
	DOC_PATH,
	`/other/path/`
	];

void main(char[][] argv)
{
	auto cgi = new HTML_CGI(argv);
	bool search_d	 = ("" != cgi.Value("D"));
	bool search_di	 = ("" != cgi.Value("DI"));
	bool search_c	 = ("" != cgi.Value("C"));
	bool search_h	 = ("" != cgi.Value("H"));
	bool search_html = ("" != cgi.Value("HTML"));
	char[] search = cgi.Value("search");

	cgi.Head(
	{
		cgi.writef("<TITLE>Search: %s \"%s\" </TITLE>",cgi.Value("path"),cgi.Value("search"));
	});
	cgi.Body(
	{
		//cgi.Spill();

		////////////////// Build "search box" header

		cgi.Form("/cgi-bin/search.cgi","post",
		{
			cgi.Heading4("Source Search:");
			cgi.Table(
			{
				cgi.TableRow(
				{
					cgi.TableData("Path:");
					cgi.TableData(
					{
						
						cgi.SelectList("path", cgi.Value("path"),roots);
					});
					cgi.writef(\n);
					cgi.TableData("Search Terms:");
					cgi.TableData({cgi.InputText("search",20, search);});
					cgi.writef(\n);
					cgi.TableData({cgi.InputSubmit("Go");});
				});
			});
			cgi.Table(
			{
				cgi.TableRow(
				{
					cgi.TableData({cgi.writef("d");cgi.InputCheckbox("D",search_d);});
					cgi.TableData({cgi.writef("di");cgi.InputCheckbox("DI",search_di);});
					cgi.TableData({cgi.writef("c");cgi.InputCheckbox("C",search_c);});
					cgi.TableData({cgi.writef("h");cgi.InputCheckbox("H",search_h);});
					cgi.TableData({cgi.writef("html");cgi.InputCheckbox("HTML",search_html);});
				});
			});
		});

		cgi.writef("<hr>");

		////////// begin scanning files

		cgi.Table(
		{
			// test if we have anything to scan

			char[] path = cgi.Value("path");
			if(path != "")
			{
				/*******
					Nested function to do recursive scanning. 
					Scan "root" and call self on all dirs
				*/
				void decend(char[] root)
				{
					// tag start of dir
					cgi.TableRow(
					{
						cgi.TableData(2,"<i>"~root~"</i>");
						cgi.TableData("dir");
					});

					// walk dirs
					foreach(DirEntry* di; root.WalkDir())
					{
						char[] type;
						char[] search = cgi.Value("search");

						try
						{
							if(di.isfile())	// test for file/dir
							{
								bool look;
								char[] str = di.name;

								// test for file type

								if(search_d && str.length > 2 && str[$-2..$] == ".d")
								{	type = "<U>D source</u>";	look = true;	}
								else if(search_di && str.length > 3 && str[$-3..$] == ".di")
								{	type = "<U>D header</u>";	look = true;	}
								else if(search_c && str.length > 2 && str[$-2..$] == ".c")
								{	type = "<U>C source</u>";	look = true;	}
								else if(search_h && str.length > 2 && str[$-2..$] == ".h")
								{	type = "<U>C header</u>";	look = true;	}
								else if(search_html && (str.length > 5 && str[$-5..$] == ".html") || (str.length > 4 && str[$-4..$] == ".htm"))
								{	type = "<U>HTML</u>";		look = true;	}
								else
								{	look = false;			type = "file";	}

								if(look)
								{
									char[][] lines = null;
									int[] lineNum = null;

									foreach(ulong i, char[] l; new File(di.name, FileMode.In))
									{
										foreach(str; l.Subs(search.length))
											if(str == search)
											{
												lines ~= l.dup;
												lineNum ~= i;
											}
									}
									if(lines.length > 0)
									{
										cgi.TableRow(
										{
											cgi.TableData(1,{cgi.writef("<u>%d</u>",di.size);});
											cgi.TableData(1,type);
											if(str[0..HTTP_PATH.length] == HTTP_PATH)
												cgi.TableData(2,{cgi.Link(str[HTTP_PATH.length..$],str);});
											else
												cgi.TableData(2,str);
										});

										foreach(i,l;lines)
											cgi.TableRow(
											{
												char[] tmp = cast(char[])read(di.name);
												if(tmp.length > 400) tmp.length = 400;
												cgi.TableData(1,{cgi.writef("%d:", lineNum[i]);});
												cgi.TableData(3,"<xmp>"~l~"</xmp>");
											});
									}
								}
							}
							else
							{
								decend(di.name);
							}
						}
						catch(Object o)
						{
							/+char[] str = di.name;
							cgi.TableRow(
							{
								cgi.TableData(2,o.toString);
								cgi.TableData(2,str);
							});+/
						}
					}// end of dir walk

				}
				decend(path);	// call function defined above
			}
			else	// nothing to scan
				cgi.writef("No path<br>");
		});



	});
}





int delegate(int delegate(inout DirEntry*)) WalkDir(char[] st)
{
	struct RetC
	{
		char[] str;
		int Ret(int delegate(inout DirEntry*) dl)
		{
			int i;
			listdir(str, (DirEntry* de){
				i = dl(de);
				return 0 == i;
				});
			return i;
		}
	}

	auto ret = new RetC;
	ret.str = st;
	return &ret.Ret;
}


int delegate(int delegate (inout char[]) dg) Subs(char[] strin,uint I)
{
	struct Ret
	{
		char[] str;
		uint I;
		int ret(int delegate (inout char[]) dg)
		{
			char[] tmp;
			for(uint i=0; i<str.length-I; i++)
			{
				tmp = str[i..i+I];
				if(int ex = dg(tmp))
					return ex;
			}
			return 0;
		}
	}
	if(strin.length < I) return (int delegate (inout char[]) dg){return 0;};

	auto ret = new Ret;
	ret.str = strin;
	ret.I = I;

	return &ret.ret;
}