#!/usr/bin/pugs

use v6;
require Test;

=kwid 

Shift tests

=cut

plan 20;

my @s1 = (1, 2, 3, 4);

is(+@s1, 4, 'we have 4 elements in our list');
my $a = shift(@s1);
is($a, 1, 'shift(@s1) works');

is(+@s1, 3, 'we have 3 elements in our list');
$a = shift @s1;
is($a, 2, 'shift @s1 works');

is(+@s1, 2, 'we have 2 elements in our list');
$a = @s1.shift();
is($a, 3, '@s1.shift() works');

is(+@s1, 1, 'we have 1 element in our list');
$a = @s1.shift;
is($a, 4, '@s1.shift() works');

is(+@s1, 0, 'we have no elements in our list');
ok(!defined(shift(@s1)), 'after the list is exhausted it give undef');

my @s2 = (1, 2, 3, 4);

is(+@s2, 4, 'we have 4 elements in our list');
is(shift(@s2), 1, 'inline shift(@s2) works');

is(+@s2, 3, 'we have 3 elements in our list');
is(shift @s2, 2, 'inline shift @s2 works');

is(+@s2, 2, 'we have 2 elements in our list');
is(@s2.shift(), 3, 'inline @s2.shift() works');

is(+@s2, 1, 'we have 1 elements in our list');
is(@s2.shift, 4, 'inline @s2.shift works');

is(+@s2, 0, 'we have no elements in our list');
ok(!defined(shift(@s2)), 'again, the list is exhausted and we get undef');
