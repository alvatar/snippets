# Assuming you mean nested classes in the following sense:

class A
    class B; end
    class C; end
end

# Where B and C are 'nested' within A then the following should work:

class Class
    def nested_classes
        constants.collect { |c| const_get(c) }.
            select { |m| m.instance_of?(Class) }
    end
end

# A.nested_classes =>  [A::B, A::C]
