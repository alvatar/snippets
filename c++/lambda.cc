// 		Lambda Abstractions and Currying in C++
// This self-contained file combines all the examples from an
// article "Lambda abstractions in C++ vs. Scheme"
// See http://pobox.com/~oleg/ftp/c++-digest/
//
// This file can be compiled (and the examples run) as
//	g++ lambda.cc -o lambda
// with gcc 2.8.1, and as
//	cl /TP lambda.cc /Folambda
// with Visual C++ 5.0.
// (note, gcc 2.7.2.1 requires one code alteration, which is illustrative
// in its own right)
//
// $Id: lambda.cc,v 1.2 2003/08/31 23:22:04 oleg Exp oleg $

#include <iostream>

using std::cout;
using std::endl;

#define XY(X,Y) X##Y
#define MakeNameXY(FX,LINE) XY(FX,LINE)
#define MakeName(FX) MakeNameXY(FX,__LINE__)

#define Lambda(args,ret_type,body) \
class MakeName(__Lambda___) { \
public: ret_type operator() args { body; } }

		// Example 3: Higher-order functions
#if defined(__GNUC__) && ((__GNUC__ == 2) && (__GNUC_MINOR__ < 8))
template<class T> void print_every_other(const int n, T& closure)
{
  cout << closure(1);
  for(register int i=3; i<=n; i+=2)
    cout << " " << closure(i);
  cout << endl;
}
#else
template<class T> void print_every_other(const int n)
{
  T closure;
  cout << closure(1);
  for(register int i=3; i<=n; i+=2)
    cout << " " << closure(i);
  cout << endl;
}
#endif

		// A closure to compute the n-th Fibonacci number,
		// with memoization
		// (caching of intermediate results)
template<class T> struct pair { T fst, snd; };

typedef Lambda((int x),
  struct xv: public pair<int> 
	       { xv(void) { fst=0; snd=0; }
		 xv(const int a, const int b) { fst = a; snd = b; }};
  pair<xv> cache; 
  int do_cache(const xv prev, const xv curr)
  {
   if( curr.fst > cache.snd.fst )
   { cache.fst = prev; cache.snd = curr; }
   return curr.snd;
  }
  int,
  if( x == 1 ) return 1;
  if( x == 2 ) return 1;
  if( x == cache.fst.fst ) return cache.fst.snd;
  if( x == cache.snd.fst ) return cache.snd.snd;
  if( x == (cache.snd.fst + 1) )
    return do_cache(cache.snd, xv(x,cache.fst.snd + cache.snd.snd));
  const int vp = (*this)(x-1);
  return do_cache(xv(x-1,vp), xv(x,vp+(*this)(x-2)))
)
fib;


		// Example 4: Currying
template<class T> class Curry_add
{
  T a;
public:
  Curry_add(const T _a) : a(_a) {}
  Curry_add<T> operator () (const T b) const { return a + b; }
  operator T (void) const { return a; }
};
template<class T> Curry_add<T> curry_add(const T a) { return a; }

static void test_currying(void)
{
  cout << "Currying 1: " << curry_add(1) << endl;
  cout << "Currying 1+2: " << curry_add(1)(2) << endl;
  cout << "Currying 1+2+4: " << curry_add(1)(2)(4) << endl;
  cout << "Currying 1.1: " << curry_add(1.1) << endl;
  cout << "Currying 1.1+0.5: " << curry_add(1.1)(0.5) << endl;
  cout << "Currying 1.1+0.5-1.0: " << curry_add(1.1)(0.5)(-1.0) << endl;
}

		// Example 5: Advanced Currying and folding.
		// Kindly contributed by Bas van der Linden from
		// Technische Universiteit Eindhoven on Aug 18, 2003

#define define(name,op) \
        template<class T> class __Curry_ ## name { T a; public: \
                __Curry_ ## name( const T& a ) : a(a) {} \
                __Curry_ ## name operator,( const T& b ) { return (op); } \
                operator T() const { return a; } \
        }; \
        struct { template<class T> __Curry_ ## name <T> \
		 operator,( const T& a ) { return a; } } name;

struct {
        template<class T>
        void operator,( const T& a ) { std::cout << a << std::endl; }
} display;


define(max, a > b ? a : b );
define(sum, a + b );


static void test_currying_folding(void)
{
 cout << "\n\nTest currying/folding" << endl;
 (display, ((((max , 2.0), 3.0), 4.0), 5.0) );
 (display, ((((max , 3.0 ), 6.2), -4.0), 5.2) );
 
 (display, ((((sum , 2.0), 3.0), 4.0), 5.0) );
 (display, ((((sum , 3.0 ), 6.2), -4.0), 5.2) );
}

int main(void)
{
		// Example 1: simple Lambda expression
  Lambda((int a, int b), int, return a+b) foo;
  cout << "foo(1,2) is " << foo(1,2) << endl;

		// Example 2: recursive Lambda expression
  Lambda((int n),int, if( n <= 0 ) return 1; else return n * (*this)(n-1))
    fact;
  cout << "fact(5) is " << fact(5) << endl;

  cout << "\nEvery other Fibonacci number: ";
#if defined(__GNUC__) && ((__GNUC__ == 2) && (__GNUC_MINOR__ < 8))
  print_every_other(11,fib());
#else
  print_every_other<fib>(11);
#endif

  test_currying();
  test_currying_folding();

  return 0;
}
