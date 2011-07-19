module xf.utils.Functional;


T[] map(S, T)(S[] src, T delegate(S) dg) {
	T[] res;
	foreach (s; src) {
		res ~= dg(s);
	}
	return res;
}
