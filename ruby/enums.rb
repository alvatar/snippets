################################################################################
# Methods for emulating enums in Ruby
# source: http://www.lesismore.co.za/rubyenums.html
################################################################################

# Represents a C# style enumeration of known values.
#
# Usage:
# Color = Enum.new(:Red, :Green, :Blue)
# Color.is_a?(Enum) # => true
# Color::Red.inspect # => "Color::Red"
# Color::Green.is_a?(Color) # => true
# Color::Green.is_a?(Enum::Member) # => true
# Color::Green.index # => 1
# Color::Blue.enum # => Color
# values = [[255, 0, 0], [0, 255, 0], [0, 0, 255]]
# values[Color::Green] # => [0, 255, 0]
# Color[0] # => Color::Red
# Color.size # => 3
#
# Enums are enumerable. Enum::Members are comparable.

class Enum < Module 
  class Member < Module 
    attr_reader :enum, :index 

    def initialize(enum, index) 
      @enum, @index = enum, index 
      # Allow Color::Red.is_a?(Color) 
      extend enum 
    end 
    
    # Allow use of enum members as array indices 
    alias :to_int :index 
    alias :to_i :index 
    
    # Allow comparison by index 
    def <=>(other)
      @index <=> other.index
    end

    include Comparable
  end

  def initialize(*symbols, &block)
    @members = []
    symbols.each_with_index do |symbol, index|
      # Allow Enum.new(:foo)
      symbol = symbol.to_s.sub(/^[a-z]/){|letter| letter.upcase}.to_sym
      member = Enum::Member.new(self, index)
      const_set(symbol, member)
      @members << member 
    end 
    super(&block) 
  end 
    
  def [](index) 
    @members[index] 
  end 
  
  def size() 
    @members.size 
  end 
  
  alias :length :size 
  
  def first(*args) 
    @members.first(*args) 
  end 
  
  def last(*args) 
    @members.last(*args) 
  end 
  
  def each(&block) 
    @members.each(&block) 
  end 
  
  include Enumerable 
end


################################################################################
################################################################################

# Symbol enums
# symbol_enums.rb

module Kernel
  # simple (sequential) enumerated values
  def enum(*syms)
    syms.each { |s| const_set(s, s.to_s) }
    const_set(:DEFAULT, syms.first) unless syms.nil?
  end
end

# Declare your enums:
# require 'symbol_enums'

module Constants
  module Gradient
    enum :DOWNSLOPE, :LEVEL, :UPSLOPE
  end

  module TreeCover
    enum :GOOD, :BAD, :OK
  end

  module TrafficDensity
    enum :LOW, :MEDIUM, :HIGH
  end
end

# Use them:
tree_cover = Constants::TreeCover::OK

################################################################################
################################################################################

# Enums with values
# value_enums.rb

class Object
  def self.enums(*args)
    args.flatten.each_with_index do | const, i |
      class_eval %(#{const} = #{i})
    end
  end

  def self.bitwise_enums(*args)
    args.flatten.each_with_index do | const, i |
      class_eval %(#{const} = #{2**i})
    end
  end
end

# Use it:
# require 'value_enums'

class Foo
  enums %w(FOO BAR BAZ)
  bitwise_enums %w(ONE TWO FOUR EIGHT)
end

p [Foo::FOO, Foo::BAR, Foo::BAZ]
p [Foo::ONE, Foo::TWO, Foo::FOUR, Foo::EIGHT]

# Output:
# [0, 1, 2]
# [1, 2, 4, 8]
