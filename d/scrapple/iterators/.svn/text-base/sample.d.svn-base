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

	return 0;
}
