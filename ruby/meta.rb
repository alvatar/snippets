################################################################################
# Metaprogramming in Ruby cookbook:
# http://www.pragprog.com/titles/ppmetr/metaprogramming-ruby
################################################################################


# =====================
# Spell: Argument Array
# =====================
# Collapse a list of arguments into an array.
def my_method(*args)
  args.map {|arg| arg.reverse }
end
my_method('abc', 'xyz', '123') # => ["cba", "zyx", "321"]

# ===================
# Spell: Around Alias
# ===================
# Call the previous, aliased version of a method from a redefined method.
class String
  alias :old_reverse :reverse
  def reverse
    "x#{old_reverse}x"
  end
end
"abc".reverse # => "xcbax"

# ===================
# Spell: Blank Slate
# ===================
# Remove methods from an object to turn them into Ghost Methods (http://gist.github.com/534776).
class C
  def method_missing(name, *args)
    "a Ghost Method"
  end
end
obj = C.new obj.to_s # => "#<C:0x357258>"
class C
  instance_methods.each do |m|
    undef_method m unless m.to_s =~ /method_missing|respond_to?|^__/
  end
end
obj.to_s # => "a Ghost Method" For more information, see page 84.

# ======================
# Spell: Class Extension
# ======================
# Define class methods by mixing a module into a class’s eigenclass
# (a special case of Object Extension - http://gist.github.com/534667).
class C; end
module M
  def my_method
    'a class method'
  end
end
class << C
  include M
end
C.my_method # => "a class method"

# ============================
# Spell: Class Extension Mixin
# ============================
# Enable a module to extend its includer through a Hook Method (http://gist.github.com/534994).
module M
  def self.included(base)
    base.extend(ClassMethods)
  end
  module ClassMethods
    def my_method
      'a class method'
    end
  end
end
class C
  include M
end
C.my_method # => "a class method"

# ==============================
# Spell: Class Instance Variable
# ==============================
# Store class-level state in an instance variable of the Class object.
class C
  @my_class_instance_variable = "some value"
  def self.class_attribute
    @my_class_instance_variable
  end
end
C.class_attribute # => "some value"

# ==================
# Spell: Class Macro
# ==================
# Use a class method in a class definition.
class C; end
class << C
  def my_macro(arg)
    "my_macro(#{arg}) called"
  end
end
class C
  my_macro :x # => "my_macro(x) called"
end

# =================
# Spell: Clean Room
# =================
# Use an object as an environment in which to evaluate a block.
class CleanRoom
  def a_useful_method(x); x * 2; end
end
CleanRoom.new.instance_eval { a_useful_method(3) } # => 6

# =====================
# Spell: Code Processor
# =====================
# Process Strings of Code (http://gist.github.com/535047) from an external source.
File.readlines("file_containing_lines_of_ruby.txt").each do |line|
  puts "#{line.chomp} ==> #{eval(line)}"
end
# >> 1 + 1 ==> 2
# >> 3 * 2 ==> 6
# >> Math.log10(100) ==> 2.0

# ====================
# Spell: Context Probe
# ====================
# Execute a block to access information in an object’s context.
class C
  def initialize
    @x = "a private instance variable"
  end
end
obj = C.new
obj.instance_eval { @x } # => "a private instance variable"

# ==========================
# Spell: Deferred Evaluation
# ==========================
# Store a piece of code and its context in a proc or lambda for evaluation later.
class C
  def store(&block)
    @my_code_capsule = block
  end
  def execute
    @my_code_capsule.call
  end
end
obj = C.new
obj.store { $X = 1 }
$X = 0
obj.execute
$X # => 1

# =======================
# Spell: Dynamic Dispatch
# =======================
# Decide which method to call at runtime.
method_to_call = :reverse
obj = "abc"
obj.send(method_to_call) # => "cba"

# =====================
# Spell: Dynamic Method
# =====================
# Decide how to define a method at runtime.
class C
end
C.class_eval do
  define_method :my_method do
    "a dynamic method"
  end
end
obj = C.new
obj.my_method # => "a dynamic method"

# ====================
# Spell: Dynamic Proxy
# ====================
# Forward to another object any messages that don’t match a method.
class MyDynamicProxy
  def initialize(target)
    @target = target
  end
  def method_missing(name, *args, &block)
    "result: #{@target.send(name, *args, &block)}"
  end
end
obj = MyDynamicProxy.new("a string")
obj.reverse # => "result: gnirts a"

# =================
# Spell: Flat Scope
# =================
# Use a closure to share variables between two scopes.
class C
  def an_attribute
    @attr
  end
end
obj = C.new
a_variable = 100
# flat scope:
obj.instance_eval do
  @attr = a_variable
end
obj.an_attribute # => 100

# ===================
# Spell: Ghost Method
# ===================
# Respond to a message that doesn’t have an associated method.
class C
  def method_missing(name, *args)
    name.to_s.reverse
  end
end
obj = C.new
obj.my_ghost_method # => "dohtem_tsohg_ym"

# ==================
# Spell: Hook Method
# ==================
# Override a method to intercept object model events.
$INHERITORS = []
class C
  def self.inherited(subclass)
    $INHERITORS << subclass
  end
end
class D < C
end
class E < C
end
class F < E
end
$INHERITORS # => [D, E, F]

# ====================
# Spell: Kernel Method
# ====================
# Define a method in module Kernel to make the method available to all objects.
module Kernel
  def a_method
    "a kernel method"
  end
end
a_method # => "a kernel method"

# =============================
# Spell: Lazy Instance Variable
# =============================
# Wait until the first access to initialize an instance variable.
class C
  def attribute
    @attribute = @attribute || "some value"
  end
end
obj = C.new
obj.attribute # => "some value"

# ===================
# Spell: Mimic Method
# ===================
# Disguise a method as another language construct.
def BaseClass(name)
  name == "string" ? String : Object
end
class C < BaseClass "string" # a method that looks like a class
  attr_accessor :an_attribute # a method that looks like a keyword
end
obj = C.new
obj.an_attribute = 1 # a method that looks like an attribute

# ==================
# Spell: Monkeypatch
# ==================
# Change the features of an existing class.
"abc".reverse # => "cba"
class String
  def reverse
    "override"
  end
end
"abc".reverse # => "override"

# ======================
# Spell: Named Arguments
# ======================
# Collect method arguments into a hash to identify them by name.
def my_method(args)
  args[:arg2]
end
my_method(:arg1 => "A", :arg2 => "B", :arg3 => "C") # => "B"

# ================
# Spell: Namespace
# ================
# Define constants within a module to avoid name clashes.
module MyNamespace
  class Array
    def to_s
      "my class"
    end
  end
end
Array.new # => []
MyNamespace::Array.new # => my class

# ================
# Spell: Nil Guard
# ================
# Override a reference to nil with an “or.”
x = nil
y=x || "avalue" # =>"avalue"

# =======================
# Spell: Object Extension
# =======================
# Define Singleton Methods by mixing a module into an object’s eigenclass.
obj = Object.new
module M
  def my_method
    'a singleton method'
  end
end
class << obj
  include M
end
obj.my_method # => "a singleton method"

# =================
# Spell: Open Class
# =================
# Modify an existing class.
class String
  def my_string_method
    "my method"
  end
end
"abc".my_string_method # => "my method"

# =======================
# Spell: Pattern Dispatch
# =======================
# Select which methods to call based on their names.
$x = 0
class C
  def my_first_method
    $x += 1
  end
  def my_second_method
    $x += 2
  end
end
obj = C.new
obj.methods.each do |m|
  obj.send(m) if m.to_s =~ /^my_/
end
$x # => 3

# ==============
# Spell: Sandbox
# ==============
# Execute untrusted code in a safe environment.
def sandbox(&code)
  proc {
    $SAFE = 2
    yield
  }.call
end
begin
  sandbox { File.delete 'a_file' }
rescue Exception => ex
  ex # => #<SecurityError: Insecure operation `delete' at level 2>
end

# =================
# Spell: Scope Gate
# =================
# Isolate a scope with the class, module, or def keyword.
a = 1
defined? a # => "local-variable"
module MyModule
  b = 1
  defined? a # => nil
  defined? b # => "local-variable"
end
defined? a # => "local-variable"
defined? b # => nil

# =================
# Spell: Self Yield
# =================
# Pass self to the current block.
class Person
  attr_accessor :name, :surname
  def initialize
    yield self
  end
end
joe = Person.new do |p|
  p.name = 'Joe'
  p.surname = 'Smith'
end

# ===================
# Spell: Shared Scope
# ===================
# Share variables among multiple contexts in the same Flat Scope (103).
lambda {
  shared = 10
  
  self.class.class_eval do
    define_method :counter do
      shared
    end
  
    define_method :down do
      shared -= 1
    end
  end
}.call
counter # => 10
3.times { down }
counter # => 7

# =======================
# Spell: Singleton Method
# =======================
# Define a method on a single object.
obj = "abc"
class << obj
  def my_singleton_method
    "x"
  end
end
obj.my_singleton_method # => "x"

# =====================
# Spell: String of Code
# =====================
# Evaluate a string of Ruby code.
my_string_of_code = "1 + 1"
eval(my_string_of_code) # => 2

# =====================
# Spell: Symbol to Proc
# =====================
# Convert a symbol to a block that calls a single method.
[1, 2, 3, 4].map(&:even?) # => [false, true, false, true]

