
import tango.io.Stdout;
class A {
public:
  void foo() {
    Stdout( "I am in A." ).newline;
  }
  bool isFooOverridden() {
    return isMethodOverridden!( "foo" )();
  }
private:
  bool isMethodOverridden( char[] methodName )() {
    auto method = mixin( "&" ~ methodName );
    return ( ( cast( Object )method.ptr ).classinfo != typeof( this ).classinfo );
  }
}
class B : A {
public:
  override void foo() {
    Stdout( "I am in B." ).newline;
  }
}
void main() {
  A a = new A();
  a.foo();
  Stdout.formatln( "foo overridden in a: {}", a.isFooOverridden() );
  A b = new B();
  b.foo();
  Stdout.formatln( "foo overridden in b: {}", b.isFooOverridden() );
}



import std.stdio;
// this is needed because dmd is totally fucked up
template addressOf(alias fn)
{
    enum addressOf = &fn;
}
class A
{
    int x;
    void foo()
    {
    }
    final void bar()
    {
        auto dg = &foo;
        if (addressOf!foo == dg.funcptr)
            writeln("not overriden");
        else
            writeln("overriden");
    }
}

class B : A
{
    override void foo()
    {
    }
}
void main()
{
    A a = new A;
    A b = new B;
    a.bar;
    b.bar;
}


