#!/usr/bin/pugs

use v6;
require Test;

plan 9;

=pod

Very basic meta-class tests from L<S12/"Introspection">

=cut

eval 'class Foo-0.0.1 { method bar ($param) returns Str { return "baz" ~ $param } }';

# L<S12/"Introspection" /should be called through the meta object\:/>

todo_eval_ok("Foo.meta.can('bar')", '... Foo can bar');
todo_eval_ok("Foo.meta.isa(Foo)", '... Foo is-a Foo (of course)');

# L<S12/"Introspection" /Class traits may include\:/>

todo_eval_ok("Foo.meta.name() eq 'Foo'", '... the name() property is Foo');
todo_eval_ok("Foo.meta.version() == 0.0.1", '... the version() property is 0.0.1');
todo_eval_ok("(Foo.meta.isa())[0] ~~ Foo", '... the isa() property returns Foo as the first parent class');

# L<S12/"Introspection" /The C\<\.meta\.getmethods\> method returns method\-descriptors containing\:/>

# NOTE: I am guessing on some of this here, but it's a start for now

my @methods = eval 'Foo.meta.getmethods()';
todo_eval_is("@methods[0].name eq 'bar'", '... our first method is foo()');
todo_eval_is("@methods[0].signature eq '\$param'", '... our first methods signature is $param');
todo_eval_is("@methods[0].returns ~~ Str", '... our first method returns a Str');
todo_eval_ok("!@methods[0].multi", '... our first method is not a multimethod');
