import std.stdio;

import iterators;

class Numbers {
	int n;
	this (int n0) {
		n = n0;
	}

	int iter() {
		for (int i = 1; i <= n; i++) {
			yield(i);
		}
		return 0;
	}
	mixin mainiter!(int, iter);

	alias Iterator!(double) IV;

	int iter2(IV x) {
		for (int i = 1; i <= n; i++) {
			x.yield(i*i);
		}
		return 0;
	}
	mixin inneriter!(double, iter2) squers;

	int iter3(IV x) {
		for (int i = 1; i <= n; i++) {
			x.yield(i*i*i);
		}
		return 0;
	}
	mixin inneriter!(double, iter3) qubes;
}

int main(char[][] args) {
	auto s = new Numbers(6);

	foreach (int k; s) {
		writef(k, " ");
	}
	writefln(); writefln();

	foreach (int i, double k; s.squers.ic()) {
		writef(i, ":", k, " ");
	}
	writefln();	writefln();

	foreach (int i, double k; s.qubes.ic()) {
		writef(i, ":", k, " ");
	}
	writefln();	writefln();

	foreach (int i, double k; s.squers.ic()) {
		foreach (double z; s.squers.ic()) {
			writef(z*k, "\t");
		}
		writefln();
	}
	writefln();
	writefln();

	try {
		foreach (int k; s) {
			foreach (int z; s) {
				writefln("ERROR1");
				return 1;
			}
			writefln("ERROR2");
			return 1;
		}
		writefln();
		writefln("ERROR3");
		return 1;
	} catch {
		writefln("Twice Nested iterator not allowed. OK1.");
	}

	try {
		auto x = s.squers.ic();
		foreach (double k; x) {
			foreach (double z; x) {
				writefln("ERROR4");
				return 1;
			}
			writefln("ERROR5");
			return 1;
		}
		writefln();
		writefln("ERROR6");
		return 1;
	} catch {
		writefln("Twice Nested iterator not allowed. OK2.");
	}

	return 0;
}
