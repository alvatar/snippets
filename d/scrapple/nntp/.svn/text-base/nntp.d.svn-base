/*
This program is distributed without any warranty of any kind!

Author: Benjamin Shropshire (shro8822drop_this at vandals dot uidaho edu)

Please give credit where credit is due. Please don't remove this notice.

*/

import std.conv;
import std.socket;
import std.socketstream;
import std.stream;
import std.date;
import inter;
debug import std.stdio;

const uint NNTP_SOCKET = 119;
//const uint NNTP_SOCKET = 2049;

T max(T)(T[] args)
{
	T ret = T.min;

	foreach(t;args)
		if(t>ret)ret=t;
	
	return ret;
}

bool Prefix(char[] sample, char[] test)
{
	return (sample.length >= test.length) && (sample[0..test.length] == test);
}

class WebNNTP : NNTP
{
	struct Post{char[] subj; char[] date; char[] auth; char[] bod; bool MIME;}

	Post[uint] posts;

	char[] server;
	char[] group;
	uint last;

	void Add(uint i, char[] s, char[] d, char[] a, char[] b)
	{
		debug writef("Trace@"__FILE__":",__LINE__,\n);
		if(i in posts) return;

		Post t;
		with(t)
		{
			subj = s.dup;
			date = d.dup;
			auth = a.dup;
			bod = b.dup;
		}
		if(i > last) last = i;
		posts[i] = t;
	}

	void Add(uint i)
	{
		debug writef("Trace@"__FILE__":",__LINE__,\n);

		CleanOld(100);

		if(i in posts) return;


		auto outgoing = new TcpSocket(AddressFamily.INET);
		outgoing.connect(new InternetAddress(server, NNTP_SOCKET));
		scope(exit) outgoing.close;
		auto outs = new SocketStream(outgoing);
		scope(exit) outs.close;

		char[] res;

		res = outs.readLine;
		debug writef("Trace@"__FILE__":",__LINE__,"(ret=%s)\n", res);
		if((res.length<3) || ('2'!=res[0])) throw new Error("Failed Introduction");

		outs.writeString("group "~group~\r\n);

		res = outs.readLine;
		debug writef("Trace@"__FILE__":",__LINE__,"(ret=%s)\n", res);
		if((res.length<3) || ('2'!=res[0])) throw new Error("No Group");

		Post tmp;
		PullHead(tmp, outs, i);
		PullBody(tmp, outs, i);
		posts[i] = tmp;

	}

		// discard posts more than 100 back
	void CleanOld(int c)
	{
		int[] drop;
		foreach(i;posts.keys)
			if(i<last-c)
				drop~=i;
		foreach(i;drop)
			posts.remove(i);
	}


	private void PullHead(inout Post tmp, Stream outs, int i)
	{
		debug writef("Trace@"__FILE__":",__LINE__,\n);
		if(i>getLast) throw new Error("No message");

		outs.writeString("head "~std.string.toString(i)~\r\n);

		char[] res;
		res = outs.readLine;
		debug writef("Trace@"__FILE__":",__LINE__,"(ret=%s)\n", res);
		if((res.length<3) || ('2'!=res[0])) throw new Error("No Post");

		tmp.MIME = false;

		while(!outs.eof)
		{
			debug writef("Trace@"__FILE__":",__LINE__,\n);
			res = outs.readLine;
			debug writef("Trace@"__FILE__":",__LINE__,\n);

			if(0<res.length && '.' == res[0] && (1==res.length || '.'!=res[1])) break;
				if(Prefix(res, "From:"))	tmp.auth = res["From:".length..$].dup;
			else	if(Prefix(res, "Date:"))	tmp.date = res["Date:".length..$].dup;
			else	if(Prefix(res, "Subject:"))	tmp.subj = res["Subject:".length..$].dup;
			else	if(Prefix(res, "Content-Type:"))tmp.MIME = !Prefix(res, "Content-Type: text/plain");
		}
	}

	private void PullBody(inout Post tmp, Stream outs, int i)
	{
		debug writef("Trace@"__FILE__":",__LINE__,\n);
		if(i>last) throw new Error("No message");

		char[][] bd;
		int l;

		outs.writeString("body "~std.string.toString(i)~\r\n);

		char[] res;
		res = outs.readLine;
		debug writef("Trace@"__FILE__":",__LINE__,"(ret=%s)\n", res);
		if((res.length<3) || ('2'!=res[0])) throw new Error("No Post");

		while(true)
		{
			res = outs.readLine;

			if(0<res.length && '.' == res[0]) break;
			res ~= \r;
			bd ~= res;
			l+= res.length;
		}

		tmp.bod.length = l;

		l=0;
		foreach(j,s;bd)
		{
			tmp.bod[l..l+s.length] = s;
			l+=s.length;
		}
	}

	
	this(char[] s, char[] g)
	{
		debug writef("Trace@"__FILE__":",__LINE__,"(%s)\n",s);
		server = s.dup;
		group = g.dup;
	}

	char[] getSubject(uint i){Add(i); return posts[i].subj;}
	char[] getDate(uint i)	{Add(i); return posts[i].date;}
	char[] getAuthor(uint i){Add(i); return posts[i].auth;}
	char[] getBody(uint i)	{Add(i); return posts[i].bod;}

	uint getLast()		
	{

		debug writef("Trace@"__FILE__":",__LINE__,\n);

		auto outgoing = new TcpSocket(AddressFamily.INET);
		outgoing.connect(new InternetAddress(server, NNTP_SOCKET));
		debug writef("Trace@"__FILE__":",__LINE__,": connected\n");
		auto outs = new SocketStream(outgoing);
		scope(exit) outs.close;

		char[] res;
		res = outs.readLine;
		if((0==res.length) || ('2'!=res[0])) throw new Error("Into falure");

		outs.writeString("group "~group~\r\n);

		res = outs.readLine;
		if((7>=res.length) || ('2'!=res[0])) throw new Error("No Group");

		debug writef("Trace@"__FILE__":",__LINE__,\n);


		int at = 4;
		int start;

		while(at<res.length && ' '==res[at]) at++;	// clear " *"
		while(at<res.length && ' '!=res[at]) at++;	// clear "[^ ]*"
		while(at<res.length && ' '==res[at]) at++;	// clear " *"
		while(at<res.length && ' '!=res[at]) at++;	// clear "[^ ]*"
		while(at<res.length && ' '==res[at]) at++;	// clear " *"
		start = at;
		while(at<res.length && ' '!=res[at]) at++;	// find "[^ ]*"

		last = toInt(res[start..at]);

		CleanOld(100);

		return last;
	}

	uint getPrev(uint i)
	{
		debug writef("Trace@"__FILE__":",__LINE__,\n);
		return i>0?i-1:0;
	}
}






void Populate(WebNNTP from)
{
	auto now = getUTCtime();

	for(int i=1; i<1000; i++)
	{
		switch(i%3)
		{
			case 0:	from.Add(i,"foo",std.date.toString(now-=10),"you ","Boo\r\nBang\r\nSplat");	break;
			case 1:	from.Add(i,"fub",std.date.toString(now-=10),"them","Bwah\r\nha!\r\nha!!!");	break;
			case 2:	from.Add(i,"fig",std.date.toString(now-=10),"me  ","blah\r\nblah\r\nblah");	break;
			default: assert(0);
		}
	}

}


class GroupDB : GroupGetter
{
	private ushort[char[]] groups;

	void add(ushort p, char[] n)
	{
		groups[n] = p;
	}

	void remove(char[] n)
	{
		groups.remove(n);
	}

	ushort get(char[] n)
	{
		ushort* ret = n in groups;
		if(null is ret) throw new Error("Unknown NewsGroup");

		return *ret;
	}

}
