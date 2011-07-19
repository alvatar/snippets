import std.stdio;

import iterators;

class Numbers {
	int n;
	this (int n0) {
		n = n0;
	}

	int iter() {
		for (int i = 0; i < n; i++) {
			yield(i);
		}
		return 0;
	}
	mixin mainiter!(int, iter);
}

int main(char[][] args) {
	const int TESTS = 40;

	const int n = 1_000_000;

	{
		auto t = new Timer();
		for (int j = 0; j < TESTS; j++) {
			long sum = 0;
			for (int i = 0; i < n; i++) {
				sum += i*j;
			}
			//writefln(sum);
		}
	}

	auto s = new Numbers(n);

	{
		auto t = new Timer();
		for (int j = 0; j < TESTS; j++) {
			long sum = 0;
			foreach (int i; s) {
				sum += i*j;
			}
			//writefln(sum);
		}
	}

	return 0;
}


import std.date;

class Timer {
	static d_time getCount(){return getUTCtime();}
	d_time starttime;
	this() { starttime = getCount(); }
	~this() { writefln("elapsed time = %s", (getCount() - starttime)/1000.0); }
}
