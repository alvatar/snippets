/***************
	BackMath

	Author: Benjamin Shropshire

	Note:
		If this library can't handle something you feed it it will spit out a
		line with a message something like this:

	"invalid types used for Sub: (- (*> a (-> (* e a) b)) (/> c (-> (/ d c) b)))"

	If you want to help improve the library (and make it work in your case)
	then you can add a rule to meta.lisp that will handle the case you are
	running into. Here is an example of how to generate a rule for the above
	case:

	start with the given lisp s-expression

	(- (*> a (-> (* e a) b)) (/> c (-> (/ d c) b)))

	replace any known sections with new variables

	(- (*> a (-> e b)) (/> c (-> d b)))

	pull out and name any (_> ...) sections and replace them with the name

	(- W T)
	W = (*> a (-> e b))
	T = (/> c (-> d b))

	"Invert" the named expressions

	(- W T)
	(- (* W a) e) = b
	(- (/ T c) d) = b

	solve for the placeholders

	(- W T)
	W = (/ (+ b e) a)
	T = (* (+ b d) c)

	substitute back in

	(- (/ (+ b e) a) (* (+ b d) c))

	isolate the unknown (in this case "b")

	(- (/ (+ b e) a) (* (+ b d) c))
	(- (+ (/ b a) (/ e a)) (+ (* b c) (* d c)))
	(- (- (+ (/ b a) (/ e a)) (* b c)) (* d c))
	(- (+ (- (/ b a) (* b c)) (/ e a)) (* d c))
	(- (+ (* b (- (/ 1 a) (* 1 c))) (/ e a)) (* d c))
	(- (+ (* b (- (/ 1 a) c)) (/ e a)) (* d c))
	(+ (+ (* b (- (/ 1 a) c)) (/ e a)) (- (* d c)))
	(+ (* b (- (/ 1 a) c)) (/ e a) (- (* d c)))
	(+ (* b (- (/ 1 a) c)) (- (/ e a) (* d c)))

	solve for the unknown

	y = (+ (* b (- (/ 1 a) c)) (- (/ e a) (* d c)))
	(- y  (- (/ e a) (* d c))) = (* b (- (/ 1 a) c))
	(/ (- y  (- (/ e a) (* d c))) (- (/ 1 a) c)) = b

	convert to "(+,- (*,/ y a) b) = x" from

	(/ (- y  (- (/ e a) (* d c))) (- (/ 1 a) c)) = b
	(- (/ y (- (/ 1 a) c)) (/ (- (/ e a) (* d c)) (- (/ 1 a) c))) = b

	convert to "assignment form"

	(- (/ y (- (/ 1 a) c)) (/ (- (/ e a) (* d c)) (- (/ 1 a) c))) = b
	(/ y (- (/ 1 a) c)) = (-> (/ (- (/ e a) (* d c)) (- (/ 1 a) c)) b)
	y = (/> (- (/ 1 a) c) (-> (/ (- (/ e a) (* d c)) (- (/ 1 a) c)) b))

	insert the new rule

	(
		(- (*> a (-> e b)) (/> c (-> d b)))
		(/> (- (/ 1 a) c) (-> (/ (- (/ e a) (* d c)) (- (/ 1 a) c)) b))
	)

	in most cases meta.lisp should contain the original case or a generalization
	of it.


	Copywrite: You may used this code only if you accept all risk involved in
	doing so. It is highly experimental and probably contains bugs. I DON'T
	warrant it for any use.

	Version: 0.001
	Date: 12/4/2007
*/

import std.stdio;


/* *****************************************************
 * Formatting code shamelessly stolen from ddl.meta.conv
 *
 * Author: Don Clugston.
 * License: Public domain.
 */

/* *****************************************************
 *  char [] fcvt!(real x)
 *  Convert a real number x to %f format
 */
template fcvt(real x)
{
	static if (x<0) {
		const real fcvt = "-" ~ .fcvt!(-x);
	} else static if (x==cast(long)x) {
		const char [] fcvt = itoa!(cast(long)x);
	} else {
		const char [] fcvt = itoa!(cast(long)x) ~ "." ~ chomp!(afterdec!(x - cast(long)x), '0');
	}
}

template decimaldigit(int n) { const char [] decimaldigit = "0123456789"[n..n+1]; }

/* *****************************************************
 *  char [] itoa!(long n);
 */
template itoa(long n)
{
	static if (n<0)
		const char [] itoa = "-" ~ itoa!(-n); 
	else static if (n<10L)
		const char [] itoa = decimaldigit!(n);
	else
		const char [] itoa = itoa!(n/10L) ~ decimaldigit!(n%10L);
}





// symboles for is(Type == Type) tests
// used like enums.
private struct Defined{}
private struct UnDefined{}

private struct CVal{}
private struct Term{}
private struct Add{}
private struct Sub{}
private struct Mul{}
private struct Div{}

private struct AddA{}
private struct SubA{}
private struct MulA{}
private struct DivA{}

private struct SubAR{}
private struct DivAR{}

private mixin(import("generated_rules.d"));

template Operate(T)
{
	// this mixin is used so that the auto generated code need not be 
	// hard coded into the generation program
	//
	// If anyone has a better idea on how to do this, I'm open to sugestions
	// Note: the code in generated_rules.d needs access to private memebers of
	// this module.

	TypeOfAdd!(T,V) opAdd(V)(V t) { TypeOfAdd!(T,V) ret; return ret; }
	TypeOfSub!(T,V) opSub(V)(V t) { TypeOfSub!(T,V) ret; return ret; }
	TypeOfMul!(T,V) opMul(V)(V t) { TypeOfMul!(T,V) ret; return ret; }
	TypeOfDiv!(T,V) opDiv(V)(V t) { TypeOfDiv!(T,V) ret; return ret; }

	void opAssign(V)(V t)
	{
		static if(is(T.DefP == Defined))
		{
			static assert(is(V.DefP == UnDefined));
			V.set = T.get;
		}
		else
		{
			static if(is(V.DefP == Defined))
				T.set = V.get;
			else
			{
//				static assert(is(typeof(*this - t)), "Can't force equality of '"~typeof(*this).stringof~"' and '"~V.stringof~\');
				(*this - t).set = 0;
			}
		}
	}
}

template Val(real r){Value!(r) Val;}
struct Value(real r)
{
	private alias   Defined DefP; private alias CVal  Op; 
	static real get(){return r;}
	mixin Operate!(typeof(*this));
	const char[] LispOf = fcvt!(r);
}
struct DefinedT(alias _r)
{
	private alias   Defined DefP; private alias Term  Op;
	static real get(){return r;}
	alias _r r;   mixin Operate!(typeof(*this));
	const char[] LispOf = _r.stringof;
}
struct UnDefinedT(alias _r)
{
	private alias UnDefined DefP; private alias Term  Op;
	static void set(real v){r = v;}
	alias _r r;   mixin Operate!(typeof(*this));
	const char[] LispOf = _r.stringof;
}

struct OpAdd  (T, U)
{
	private alias   Defined DefP; private alias Add   Op;
	static real get(){ return T.get + U.get; }   
	mixin Operate!(typeof(*this)); alias T LHS; alias U RHS;
	const char[] LispOf = "(+ "~LHS.LispOf~" "~RHS.LispOf~")";
}
struct OpSub  (T, U)
{
	private alias   Defined DefP; private alias Sub   Op;
	static real get(){ return T.get - U.get; }   
	mixin Operate!(typeof(*this)); alias T LHS; alias U RHS;
	const char[] LispOf = "(- "~LHS.LispOf~" "~RHS.LispOf~")";
}
struct OpMul  (T, U)
{
	private alias   Defined DefP; private alias Mul   Op;
	static real get(){ return T.get * U.get; }   
	mixin Operate!(typeof(*this)); alias T LHS; alias U RHS;
	const char[] LispOf = "(* "~LHS.LispOf~" "~RHS.LispOf~")";
}
struct OpDiv  (T, U)
{
	private alias   Defined DefP; private alias Div   Op;
	static real get(){ return T.get / U.get; }   
	mixin Operate!(typeof(*this)); alias T LHS; alias U RHS;
	const char[] LispOf = "(/ "~LHS.LispOf~" "~RHS.LispOf~")";
}

struct OpAddA (T, U)
{
	private alias UnDefined DefP; private alias AddA  Op;
	static void set(real v){ U.set = v + T.get; }
	mixin Operate!(typeof(*this)); alias T LHS; alias U RHS;
	const char[] LispOf = "(+> "~LHS.LispOf~" "~RHS.LispOf~")";
}
struct OpSubA (T, U)
{
	private alias UnDefined DefP; private alias SubA  Op;
	static void set(real v){ U.set = v - T.get; }
	mixin Operate!(typeof(*this)); alias T LHS; alias U RHS;
	const char[] LispOf = "(-> "~LHS.LispOf~" "~RHS.LispOf~")";
}
struct OpSubAR(T, U)
{
	private alias UnDefined DefP; private alias SubAR Op;
	static void set(real v){ U.set = T.get - v; }
	mixin Operate!(typeof(*this)); alias T LHS; alias U RHS;
	const char[] LispOf = "(-R> "~LHS.LispOf~" "~RHS.LispOf~")";
}
struct OpMulA (T, U)
{
	private alias UnDefined DefP; private alias MulA  Op;
	static void set(real v){ U.set = v * T.get; }
	mixin Operate!(typeof(*this)); alias T LHS; alias U RHS;
	const char[] LispOf = "(*> "~LHS.LispOf~" "~RHS.LispOf~")";
}
struct OpDivA (T, U)
{
	private alias UnDefined DefP; private alias DivA  Op;
	static void set(real v){ U.set = v / T.get; }
	mixin Operate!(typeof(*this)); alias T LHS; alias U RHS;
	const char[] LispOf = "(/> "~LHS.LispOf~" "~RHS.LispOf~")";
}
struct OpDivAR(T, U)
{
	private alias UnDefined DefP; private alias DivAR Op;
	static void set(real v){ U.set = T.get / v; }
	mixin Operate!(typeof(*this)); alias T LHS; alias U RHS;
	const char[] LispOf = "(/R> "~LHS.LispOf~" "~RHS.LispOf~")";
}


real a, b, c, d, e, f, g;

void main(){Unittest();}


//unittest
void Unittest()
{
	DefinedT!(a)   A;
	UnDefinedT!(b) B;
	DefinedT!(c)   C;
	DefinedT!(d)   D;
	DefinedT!(e)   E;
	DefinedT!(f)   F;
	DefinedT!(g)   G;
	Value!(0)      Z;

	a=1;
	c=2;
	d=3;

	A + B - D = C; // 1 + b - 3 = 2; -> 4
	assert(b == 4);

	A - B - D = C; // 1 - b - 3 = 2; -> -4
	assert(b == -4);

	A - D - B = C; // 1 - 3 - b = 2; -> -4
	assert(b == -4);

	(B - A) - D = C; // b - 1 - 3 = 2; -> 6
	assert(b == 6);

	A - D - B - D = C; // 1 - 3 - b - 3 = 2; -> -7
	assert(b == -7);

	A - B - (D + D) = C; // 1 - b - (3 + 3) = 2; -> -7
	assert(b == -7);

	a=3;
	C = A + B - D; // 2 = 3 + b - 3; -> 2
	assert(b == 2);

	C = B * A; // 2 = b * 3; -> 2/3
	writef("%s == (2/3)\n", b);

	C = B * A + B * D; // 2 = b*3 + b*3; -> 1/3
	writef("%s == (1/3)\n", b);

	A / B  + D / B = C; // 2 = 3/B + 3/B; -> 6/2
	writef("%s == (6/2)\n", b);

	B / A = (B / C) + D; // B / 3 = B / 2 + 3; -> -18
	writef("%s == -18\n", b);

	(B * A) + E = (B * C) + D;
	(B / A) + E = (B * C) + D;
	(B * A) + E = (B / C) + D;
	(B / A) + E = (B / C) + D;

	(B * A) = (B * C) + D; // B * 3 = B * 2 + 3; -> 3
	writef("%s == 3\n", b);
	(B / A) = (B * C) + D;
	(B * A) = (B / C) + D;
	(B / A) = (B / C) + D;

	(B * A) + E = (B * C);
	(B / A) + E = (B * C);
	(B * A) + E = (B / C);
	(B / A) + E = (B / C);

	(B * A) - E = (B * C) - D;
	(B / A) - E = (B * C) - D;
	(B * A) - E = (B / C) - D;
	(B / A) - E = (B / C) - D;

	(B * A) = (B * C) - D;
	(B / A) = (B * C) - D;
	(B * A) = (B / C) - D;
	(B / A) = (B / C) - D;

	(B * A) - E = (B * C);
	(B / A) - E = (B * C);
	(B * A) - E = (B / C);
	(B / A) - E = (B / C);

	(B * A) = (B * C);
	(B / A) = (B * C);
	(B * A) = (B / C);
	(B / A) = (B / C);

	Z = (B * A) + E + ((B * C) + D);
	Z = (B / A) + E + ((B * C) + D);
	Z = (B * A) + E + ((B / C) + D);
	Z = (B / A) + E + ((B / C) + D);

	Z = (B * A) + ((B * C) + D);
	Z = (B / A) + ((B * C) + D);
	Z = (B * A) + ((B / C) + D);
	Z = (B / A) + ((B / C) + D);

	Z = (B * A) + E + (B * C);
	Z = (B / A) + E + (B * C);
	Z = (B * A) + E + (B / C);
	Z = (B / A) + E + (B / C);

	Z = (B * A) - E + ((B * C) - D);
	Z = (B / A) - E + ((B * C) - D);
	Z = (B * A) - E + ((B / C) - D);
	Z = (B / A) - E + ((B / C) - D);

	Z = (B * A) + ((B * C) - D);
	Z = (B / A) + ((B * C) - D);
	Z = (B * A) + ((B / C) - D);
	Z = (B / A) + ((B / C) - D);

	Z = (B * A) - E + (B * C);
	Z = (B / A) - E + (B * C);
	Z = (B * A) - E + (B / C);
	Z = (B / A) - E + (B / C);

	Z = (B * A) + (B * C);
	Z = (B / A) + (B * C);
	Z = (B * A) + (B / C);
	Z = (B / A) + (B / C);

	Z = B + (B*A + D);
	Z = B + (B/A + D);
	Z = B + (B*A - D);
	Z = B + (B/A - D);
	Z = B - (B*A + D);
	Z = B - (B/A + D);
	Z = B - (B*A - D);
	Z = B - (B/A - D);

}