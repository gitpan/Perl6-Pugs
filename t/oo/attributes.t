#!/usr/bin/pugs

use v6;
require Test;

plan 48;

=pod

Class attributes tests from L<S12/"Attributes">

=cut

todo_eval_ok '!try { has $.x }', "has only works inside of class|role definitions";

# L<S12/"Attributes" /the automatic generation of an accessor method of the same name\./>

eval 'class Foo1 { has $.bar; }';

{
    my $foo = eval 'Foo1.new()';
    todo_eval_ok('$foo ~~ Foo1', '... our Foo instance was created');
    todo_eval_ok('$foo.can("bar")', '.. checking autogenerated accessor existence');
    todo_eval_ok('!defined($foo.bar())', '.. autogenerated accessor works');
    todo_eval_ok('!defined($foo.bar)', '.. autogenerated accessor works w/out parens');    
    # what exactly will happen if we try to set bar()
}

# L<S12/"Attributes" /Pseudo-assignment to an attribute declaration specifies the default/>

eval 'class Foo2 { has $.bar = "baz"; }';

{
    my $foo = eval 'Foo2.new()';
    todo_eval_ok('$foo ~~ Foo2', '... our Foo instance was created');
    todo_eval_ok('$foo.can("bar")', '.. checking autogenerated accessor existence');
    todo_eval_is('$foo.bar()', "baz", '.. autogenerated accessor works');
    todo_eval_is('$foo.bar', "baz", '.. autogenerated accessor works w/out parens');    
    # what exactly will happen if we try to set bar()
}

# L<S12/"Attributes" /trait causes the generated accessor to be declared C\<rw\>\, making it/>

eval 'class Foo3 { has $.bar is rw; }';

{
    my $foo = eval 'Foo3.new()';
    todo_eval_ok('$foo ~~ Foo3', '... our Foo instance was created');
    todo_eval_ok('$foo.can("bar")', '.. checking autogenerated accessor existence');
    todo_eval_ok('!defined($foo.bar())', '.. autogenerated accessor works');
    todo_eval_ok('$foo.bar = "baz"', '.. autogenerated mutator works as lvalue');    
    todo_eval_is('$foo.bar', "baz", '.. autogenerated mutator as lvalue set the value correctly');    
    todo_eval_ok('$foo.bar("baz2")', '.. autogenerated mutator works as method');    
    todo_eval_is('$foo.bar', "baz2", '.. autogenerated mutator as method set the value correctly');        
}

# L<S12/"Attributes" /Private attributes use a colon to indicate that no public accessor is/>

eval 'class Foo4 { has $:bar = "baz"; }';

{
    my $foo = eval 'Foo4.new()';
    todo_eval_ok('$foo ~~ Foo4', '... our Foo instance was created');
    todo_eval_ok('!$foo.can("bar")', '.. checking autogenerated accessor existence');
}

# L<S12/"Attributes">

eval 'class Foo5 {
  has $.tail is rw;
  has @.legs;
  has $:brain;

  method set_legs  (@legs) { @.legs = @legs }
  method inc_brain ()      { $:brain++ }
  method get_brain ()      { $:brain }
}';

{
    my $foo = eval 'Foo5.new()';
    todo_eval_ok('$foo ~~ Foo5', '... our Foo instance was created');
        
    todo_eval_is '$foo.tail = "a"', "a", "setting a public rw attribute";
    todo_eval_is '$foo.tail',       "a", "getting a public rw attribute";
    
    todo_eval_ok '$foo.set_legs(1,2,3)',       "setting a public ro attribute (1)";
    todo_eval_is '$foo.legs.[1]', 2,           "getting a public ro attribute (1)";
    # ok instead of todo_ok to suppress "unexpected succeeded"-messages
    ok           !eval('$foo.legs = (4,5,6)'), "setting a public ro attribute (2)";
    todo_eval_is '$foo.legs.[1]', 2,           "getting a public ro attribute (2)";
    
    todo_eval_ok '$foo.inc_brain()',  "modifiying a private attribute (1)";
    todo_eval_is '$foo.get_brain', 1, "getting a private attribute (1)";
    todo_eval_ok '$foo.inc_brain()',  "modifiying a private attribute (2)";
    todo_eval_is '$foo.get_brain', 2, "getting a private attribute (2)";
}

# L<S12/"Construction and Initialization" /If you name an attribute as a parameter, that attribute is initialized directly, so/>

eval 'class Foo6 {
  has $.bar is rw;
  has $.baz;
  has $:hidden;

  submethod BUILD($.bar, $.baz, $:hidden) {}
  method get_hidden() { $:hidden }
}';

{
    my $foo = eval 'Foo6.new(bar => 1, baz => 2, hidden => 3)';
    todo_eval_ok '$foo ~~ Foo6', '... our Foo instance was created';
        
    todo_eval_is '$foo.bar',        1, "getting a public rw attribute (1)";
    todo_eval_is '$foo.baz',        2, "getting a public rw attribute (2)";
    todo_eval_is '$foo.get_hidden', 3, "getting a private ro attribute (3)";
}


# L<A12/"Default Values">
todo_eval_ok 'class Foo7 { has $.attr = 42 }', "class definition worked";
todo_eval_is 'Foo7.new.attr', 42,              "default attribute value (1)";

# L<A12/"Default Values" /is equivalent to this:/>
todo_eval_ok 'class Foo8 { has $.attr is build(42) }',
  "class definition using 'is build' worked";
todo_eval_is 'Foo8.new.attr', 42, "default attribute value (2)";

# L<A12/"Default Values" /is equivalent to this:/>
todo_eval_ok 'class Foo9 { has $.attr will build(42) }',
  "class definition using 'will build' worked";
todo_eval_is 'Foo9.new.attr', 42, "default attribute value (3)";

my $was_in_supplier = 0;
sub fourty_two_supplier() { $was_in_supplier++; 42 }
# XXX: Currently hard parsefail!
#todo_eval_ok 'class Foo10 { has $.attr = { fourty_two_supplier() } }',
#  "class definition using '= {...}' worked";
todo_fail "hard parsefail";
todo_eval_is 'Foo10.new.attr', 42, "default attribute value (4)";
todo_is      $was_in_supplier, 1,  "fourty_two_supplier() was actually executed (1)";

# The same, but using 'is build {...}'
# XXX: Currently hard parsefail!
#todo_eval_ok 'class Foo11 { has $.attr is build { fourty_two_supplier() } }',
#  "class definition using 'is build {...}' worked";
todo_fail "hard parsefail";
todo_eval_is 'Foo11.new.attr', 42, "default attribute value (5)";
todo_is      $was_in_supplier, 2,  "fourty_two_supplier() was actually executed (2)";

# The same, but using 'will build {...}'
# XXX: Currently hard parsefail!
#todo_eval_ok 'class Foo12 { has $.attr will build { fourty_two_supplier() } }',
#  "class definition using 'will build {...}' worked";
todo_fail "hard parsefail";
todo_eval_is 'Foo11.new.attr', 42, "default attribute value (6)";
todo_is      $was_in_supplier, 3,  "fourty_two_supplier() was actually executed (3)";