module xf.utils.ScopedResource;



struct ScopedResource(Pool) {
	alias Pool.Res		Res;
	alias Pool.Params	Params;
	
	void opCall(Params p, void delegate(Res) dg) {
		Res res = pool.get(p);
		scope (exit) pool.dispose(res);
		dg(res);
	}
	
	private Pool pool;
}
