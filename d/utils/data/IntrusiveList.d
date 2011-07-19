module xf.utils.data.IntrusiveList;

private {
	import tango.io.Stdout;		// TMP
}




template IntrusiveList(char[] name, char[] junk1, int junk2) {
	private {
		alias typeof(this) ThisPtr;		
		static if (is(ThisPtr == class)) {
			alias ThisPtr ThisVal;
		} else {
			alias typeof(*ThisPtr.init) ThisVal;
		}
		
		struct IntrusiveListData {
			ThisPtr	next;
			ThisPtr	prev;
			
			IntrusiveListData* nextList() {
				return next is null ? this : getData(next);
			}
			
			IntrusiveListData* prevList() {
				return prev is null ? this : getData(prev);
			}

			void opCatAssign(ThisPtr x) {
				mergeCircular(getThis, x, &getPrev, &link);
			}
			
			void unlink() {
				if (prev !is null) {
					assert (next !is null);
					getData(prev).next = next;
					getData(next).prev = prev;
					next = prev = getThis();
				} else {
					assert (next is null);
				}
			}
			
			bool linked() {
				return next !is null && next !is getThis();
			}
			
			int opApply(int delegate(ref ThisVal) dg) {
				return iterCircular(getThis, &getNext, (ThisPtr p) {
					static if (is(ThisPtr == class)) {
						return dg(p);
					} else {
						return dg(*p);
					}
				});
			}

			private {
				ThisPtr getThis() {
					return cast(ThisPtr)(cast(void*)this - cast(size_t)mixin("&ThisPtr.init."~name));
				}
				
				static {
					void mergeCircular(I, GetPrevFn/*I delegate(I)*/, LinkFn/*void delegate(I, I)*/)(I as, I bs, GetPrevFn getPrev, LinkFn link) {
						I ae = getPrev(as);
						I be = getPrev(bs);
						
						assert (ae != bs);
						assert (be != as);
						
						link(ae, bs);
						link(be, as);
					}


					int iterCircular(I, GetNextFn/*I delegate(I)*/)(I start, GetNextFn getNext, int delegate(I) dg) {
						if (auto res = dg(start)) {
							return res;
						}
						
						for (I it = getNext(start); it != start; it = getNext(it)) {
							if (auto res = dg(it)) {
								return res;
							}
						}
						
						return 0;
					}

					IntrusiveListData* getData(ThisPtr x) {
						auto res = cast(IntrusiveListData*)(cast(void*)x + cast(size_t)mixin("&ThisPtr.init."~name));
						if (res.next is null) {
							res.next = res.prev = x;
						}
						return res;
					}

					ThisPtr getNext(ThisPtr p) {
						return getData(p).next;
					}
					
					ThisPtr getPrev(ThisPtr p) {
						return getData(p).prev;
					}

					void link(ThisPtr a, ThisPtr b) {
						getData(a).next = b;
						getData(b).prev = a;
					}
				}
			}
		}
	}
	
	mixin("IntrusiveListData "~name~";");
}


// hack for DMD > 1.032's typeof(this) bug in mixins
char[] intrusiveList(char[] name) {
	return `mixin IntrusiveList!("`~name~`", __FILE__, __LINE__);`;
}



/+class Foo {
	int foo;
	float bar;
	mixin(intrusiveList("list"));
	mixin(intrusiveList("list2"));
	char[] baz;
}


struct Bar {
	int foo;
	float bar;
	mixin(intrusiveList("list"));
	mixin(intrusiveList("list2"));
	char[] baz;
}


void testList(T)() {
	auto newT = (char[] s) {
		auto t = new T;
		t.baz = s;
		return t;
	};
	
	auto f = newT("first");
	f.list ~= newT("second");
	f.list ~= newT("third");
	f.list ~= newT("fourth");
	f.list2 ~= newT("second2");
	
	Stdout.formatln("list1:");
	foreach (o; f.list) {
		Stdout.formatln("\t{}", o.baz);
	}

	Stdout.formatln("\nlist2:");
	foreach (o; f.list2) {
		Stdout.formatln("\t{}", o.baz);
	}
}


void main() {
	Stdout.formatln("Testing a class:");
	testList!(Foo);

	Stdout.formatln("\nTesting a struct:");
	testList!(Bar);
}
+/