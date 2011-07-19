import std.stdio;

/**
	Author: Benjamin Shropshire (shro8822 AT drop_spam_this vandals dot uidaho edu)
	Copyright: You may redistribute this as you see fit. Please don't clame it 
		as your own. THIS SOFTWARE HAS NO WARRANTY OR GUARANTEE OF ANY KIND.
*/

debug
{
	void main(){}
	// Don Clugston's decimaldigit and itoa,
	// see http://trac.dsource.org/projects/ddl/browser/trunk/meta/conv.d

	template decimalDigit(int n)
	{
	  const char[] decimalDigit = "0123456789"[n..n+1];
	} 

	template itoa(long n)
	{   
	  static if (n < 0)
	     const char[] itoa = "-" ~ itoa!(-n);  
	  else static if (n < 10)
	     const char[] itoa = decimalDigit!(n); 
	  else
	     const char[] itoa = itoa!(n/10L) ~ decimalDigit!(n%10L); 
	}
}

unittest
{
	BigNum!(5) tmp,add,sum;

	tmp = uint.max;
	assert(tmp.values[0] == uint.max, "set(uint) failed");
	assert(tmp == uint.max, "opEqual(uint) false neg");
	assert(tmp != 1, "opEqual(uint) false pos");

	add = 1;
	assert(add.values[0] == 1, "set(uint) failed");
	assert(add == 1, "opEqual(uint) false neg");
	assert(add != uint.max, "opEqual(uint) false pos");

	tmp.values[1] = uint.max;

	sum = tmp+add;
	assert(sum.values[0] == 0, "low word failed");
	assert(sum.values[1] == 0, "carry word failed");
	assert(sum.values[2] == 1, "high word failed");

	sum = tmp;
	sum += add;
	assert(sum.values[0] == 0, "low word failed");
	assert(sum.values[1] == 0, "carry word failed");
	assert(sum.values[2] == 1, "high word failed");

	tmp.values[2] = tmp.values[0] = 0;
	tmp.values[1] = 1;
	sum = tmp - add;
	assert(sum.values[0] == uint.max, "low word failed");
	assert(sum.values[1] == 0, "borrow word failed");
	assert(sum.values[2] == 0, "high word failed");

	sum = tmp;
	sum -= add;
	assert(sum.values[0] == uint.max, "low word failed");
	assert(sum.values[1] == 0, "borrow word failed");
	assert(sum.values[2] == 0, "high word failed");

	tmp.values[0] = uint.max;
	tmp.values[1] = uint.max;

	sum = tmp*tmp;

	assert(sum.values[3] == uint.max);
	assert(sum.values[2] == uint.max - 1);
	assert(sum.values[1] == 0);
	assert(sum.values[0] == 1);
}

template BigNum(uint size)
{
	static if(size > 63) pragma(msg, "Ok, I'll try it. But don't say I didn't warn you!!!");

	static if(size <= 0) static assert(false, "BigNum!(0) invalid");
	else static if(size <= 2) static assert(false, "BigNum!(size<=2) unimplemented");
	else alias build!(size, size-2, size-1) BigNum;
}

template build(uint size, uint i, V...)
{
	static if(i==1)
		alias use!(size, 1, V) build;
	else
		alias build!(size, i-1, i, V) build;
}

template use(uint size, V...)
{
	struct use
	{
			//DMD0.174 seg-v for size > 73
		static if(size > 73) pragma(msg, "You might have set a new world record!!!!");

			// most likely fails of this doesn't hold.
		static assert(0 == values.offsetof);
		uint[size] values;

		void Emit()
		{
			for(int i=size-1; i>0; i--)
				writef("%x:",values[i]);
			writef("%x\n",values[0]);
		}

		void opAssign(uint val)
		{
			values[0] = val;
			static if(size>1)
				foreach(inout v; values[1..$])
					v = 0;
		}

		bool opEquals(uint i)
		{
			if (values[0] != i) return false;
			static if(size>1)
				foreach(inout v; values[1..$])
					if(v != 0) return false;
			return true;
		}

		bool opEquals(use!(size, V)  i)
		{
			return i.values[] == values[];
		}

		use!(size, V) opAdd(use!(size, V) to)
		{
			uint* ths  = this.values.ptr;
			uint* dest = to.values.ptr;
				// DON'T USE "this" after this line!!!!!!!
			asm
			{
				mov EAX, ths[0];
				mov EBX, dest[0];
				mov EDX, [EAX+0];
				add [EBX+0], EDX;
			}
			foreach(j,i; V)
			{
				const uint off = V[j]*4;		 // this is a work-around
				asm
				{
					mov EDX, [EAX+off];
					adc [EBX+off], EDX;
				}
			}
			return to;
		}

		use!(size, V) opSub(use!(size, V) to)
		{
			uint* ths  = this.values.ptr;
			uint* dest = to.values.ptr;
				// DON'T USE "this" after this line!!!!!!!
			asm
			{				// dest = ths - dest
				mov EAX, ths[0];	// dest = *EAX - dest
				mov EBX, dest[0];	// dest = *EAX - *EBX

				mov EDX, [EAX+0];	// dest = EDX - *EBX
				sub EDX, [EBX+0];	// EDX = EDX - *EBX
				mov [EBX+0], EDX;	// *EBX
							// dest
			}
			foreach(j,i; V)
			{
				const uint off = V[j]*4;	 // this is a work-around
				asm
				{
								// dest = ths - dest
								// dest = *EAX - *EBX
					mov EDX, [EAX+off];	// dest = EDX - *EBX
					sbb EDX, [EBX+off];	// EDX = EDX - ECX
					mov [EBX+off], EDX;	// *EBX
								// dest
				}
			}
			return to;
		}

		use!(size, V) opMul(use!(size, V) to)
		{
			uint* ths  = this.values.ptr;
			use!(size, V) ret;
				// DON'T USE "this" after this line!!!!!!!

			// EDX:EAX	mul in/out
			asm
			{
			// load values.ptr[0] -> ECX
			mov ECX, ths[0];
			mov ECX, [ECX];
			// load to.values.ptr[0] -> EAX
			mov EAX, to[0];
			// EDX:EAX = mul(EAX, ECX)
			imul ECX;
			// load EAX -> ret.values[0]
			mov ret[0], EAX;
			// load EDX -> ret.values[1]
			mov ret[1], EDX;
			}

			debug pragma(msg, itoa!(__LINE__)~": set ret[0] from ths[0] and to[0]");
			debug pragma(msg, itoa!(__LINE__)~": set ret[1] from ths[0] and to[0]");


			foreach(k,l; V)
			{
				const uint InI = V[k];	 // this is a work-around
				const uint DesI = InI;
				static if(DesI < size)
				{
					asm
					{
		// this seems to index by 32b (I'm not sure I trust this)
					// load to.values.ptr[InI] -> EAX
					mov EAX, to[InI];
					// EDX:EAX = mul(EAX, ECX)
					imul ECX;

					// load ret.values[DesI] -> EBX
					mov EBX, ret[DesI];
					// EBX = add(EBX, EAX)
					add EBX, EAX;
					// load EBX -> ret.values[DesI]
					mov ret[DesI], EBX;
					}
					debug pragma(msg, itoa!(__LINE__)~": set ret["~itoa!(DesI)~"] from ths[0] and to["~itoa!(InI)~"]");

					static if(DesI+1 < size)
					{
						asm
						{
						// load ret.values[DesI+1] -> EBX
						mov EBX, ret[DesI+1];
						// EBX = adc(EBX, EDX)
						adc EBX, EDX;
						// load EBX -> ret.values[DesI+1]
						mov ret[DesI+1], EBX;
						}
						debug pragma(msg, itoa!(__LINE__)~": set ret["~itoa!(DesI+1)~"] from ths[0] and to["~itoa!(InI)~"]");
					}
				}
			}

			foreach(i,j; V)
			{
					// get rank of this row
				const uint OutI = V[i];	 // this is a work-around
				asm
				{
				// load values.ptr[OutI] -> ECX
				mov ECX, ths;
				mov ECX, [ECX+OutI];
				// load to.values.ptr[0] -> EAX
				mov EAX, to[0];
				// EDX:EAX = mul(EAX, ECX)
				imul ECX;

				// load ret.values[OutI] -> EBX
				mov EBX, ret[OutI];
				// EBX = add(EBX, EAX)
				add EBX, EAX;
				// load EBX -> ret.values[OutI]
				mov ret[OutI], EBX;
				}
				debug pragma(msg, itoa!(__LINE__)~": set ret["~itoa!(OutI)~"] from ths["~itoa!(OutI)~"] and to[0]");

				static if(OutI+1 < size)
				{
					asm
					{
					// load ret.values[OutI+1] -> EBX
					mov EBX, ret[OutI+1];
					// EBX = adc(EBX, EDX)
					adc EBX, EDX;
					// load EBX -> ret.values[OutI+1]
					mov ret[OutI+1], EBX;
							nop;
					}
					debug pragma(msg, itoa!(__LINE__)~": set ret["~itoa!(OutI+1)~"] from ths["~itoa!(OutI)~"] and to[0]");
				}

				foreach(k,l; V)
				{
					const uint InI = V[k];	 // this is a work-around
					const uint DesI = OutI + InI;
					static if(DesI < size)
					{
						asm
						{
						// load to.values.ptr[InI] -> EAX
						mov EAX, to[InI];
						// EDX:EAX = mul(EAX, ECX)
						imul ECX;

						// load ret.values[DesI] -> EBX
						mov EBX, ret[DesI];
						// EBX = add(EBX, EAX)
						add EBX, EAX;
						// load EBX -> ret.values[DesI]
						mov ret[DesI], EBX;
						}
						debug pragma(msg, itoa!(__LINE__)~": set ret["~itoa!(DesI)~"] from ths["~itoa!(OutI)~"] and to["~itoa!(InI)~"]");

						static if(DesI+1 < size)
						{
							asm
							{
							// load ret.values[DesI+1] -> EBX
							mov EBX, ret[DesI+1];
							// EBX = adc(EBX, EDX)
							adc EBX, EDX;
							// load EBX -> ret.values[DesI+1]
							mov ret[DesI+1], EBX;
							}
							debug pragma(msg, itoa!(__LINE__)~": set ret["~itoa!(DesI+1)~"] from ths["~itoa!(OutI)~"] and to["~itoa!(InI)~"]");
						}
					}
				}
			}
			return ret;
		}


		use!(size, V) opDiv(uint by)
		{
			uint* ths  = this.values.ptr;
			use!(size, V) ret;
				// DON'T USE "this" after this line!!!!!!!

			asm
			{
				// move by into divisor(ECX)
				mov ECX, by[0];
				// zero dividend.high(EDX)
				xor EDX, EDX;
			}

			foreach_reverse(j,i; V)
			{
				const uint off = V[j]*4;		 // this is a work-around
				
				debug pragma(msg, itoa!(__LINE__)~": dividing place ["~itoa!(off)~"]");
				asm
				{
					// Load this.values[off] into dividend.low(EAX)
					mov EAX, ths[off];
					// divide dividend by divisor(ECX), leaving in quotient(EAX):remainder(EDX)
					div ECX;
					// move quotient(EAX) to ret.values[off]
					mov ret[off], EAX;
					// make remainder(EDX) be dividend.high(EDX)
					//     no op needed
				}
			}
			return ret;

			debug pragma(msg, itoa!(__LINE__)~": dividing place [0]");
			asm
			{
				// Load this.values[0] into dividend.low(EAX) 
				mov EAX, ths[0];
				// divide dividend by divisor(ECX), leaving in quotient(EAX):remainder(EDX)
				div ECX;
				// move quotient(EAX) to ret.values[0]
				mov ret[0], EAX;
				// discard remainder(EDX)
			}

		}

		use!(size, V) opAddAssign(use!(size, V) to)
		{
			uint* ths  = this.values.ptr;
			uint* dest = to.values.ptr;
				// DON'T USE "this" after this line!!!!!!!
			asm
			{
				mov EBX, ths[0];
				mov EAX, dest[0];
				mov EDX, [EAX+0];
				add [EBX+0], EDX;
				// no store needed because dest is memory
			}
			foreach(j,i; V)
			{
				const uint off = V[j]*4;		 // this is a work-around
				asm
				{
					mov EDX, [EAX+off];
					adc [EBX+off], EDX;
					// no store needed because dest is memory
				}
			}
			return to;
		}

		use!(size, V) opSubAssign(use!(size, V) to)
		{
			uint* ths  = this.values.ptr;
			uint* dest = to.values.ptr;
				// DON'T USE "this" after this line!!!!!!!
			asm
			{
				mov EBX, ths[0];
				mov EAX, dest[0];
				mov EDX, [EAX+0];
				sub [EBX+0], EDX;
				// no store needed because dest is memory
			}
			foreach(j,i; V)
			{
				const uint off = V[j]*4;		 // this is a work-around
				asm
				{
					mov EDX, [EAX+off];
					sbb [EBX+off], EDX;
					// no store needed because dest is memory
				}
			}
			return to;
		}

		use!(size, V) opMulAssign(use!(size, V) to)
		{
			return *this = opMul(to);
		}

		use!(size, V) opDivAssign(uint to)
		{
			return *this = opDiv(to);
		}
	}
}