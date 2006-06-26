#!/usr/bin/pugs

use v6;
use Test;

=kwid

=head1 String interpolation

These tests derived from comments in L<"http://use.perl.org/~autrijus/journal/23398">

=cut

plan 30;

my $world = "World";
my @list  = (1,2);
my %hash  = (1=>2);
sub func { return "func-y town" }
sub func_w_args($x,$y) { return "[$x][$y]" }

# Double quotes
is("Hello $world", 'Hello World', 'double quoted string interpolation works');
is("@list[]\ 3 4", '1 2 3 4', 'double quoted list interpolation works');
is("@list 3 4", '@list 3 4', 'array without empty square brackets does not interpolate');
is("%hash{}", "1\t2\n", 'hash interpolation works');
is("%hash", '%hash', 'hash interpolation does not work if not followed by {}');
is("Wont you take me to &func()", 'Wont you take me to func-y town', 'closure interpolation');
is("2 + 2 = { 2+2 }", '2 + 2 = 4', 'double quoted closure interpolation works');
is("&func() is where I live", 'func-y town is where I live', "make sure function interpolation doesn't eat all trailing whitespace");

# L<S02/Names and Variables /except when interpolating/>
is("&func. () is where I live", '&func. () is where I live', '"&func. ()" should not interpolate');
is("&func_w_args("foo","bar"))", '[foo][bar])', '"&func_w_args(...)" should interpolate');
# L<S02/"Literals" /"In order to interpolate the result of a method call">
is("$world.chars()", '5', 'method calls with parens should interpolate');
is("$world.chars", 'World.chars', 'method calls without parens should not interpolate');
is("$world.substr(0,1)", 'W', 'method calls with parens and args should interpolate');

# Single quotes
# XXX the next tests will always succeed even if '' interpolation is buggy
is('Hello $world', 'Hello $world', 'single quoted string interpolation does not work (which is correct)');
is('2 + 2 = { 2+2 }', '2 + 2 = { 2+2 }', 'single quoted closure interpolation does not work (which is correct)');
is('$world @list[] %hash{} &func()', '$world @list[] %hash{} &func()', 'single quoted string interpolation does not work (which is correct)');

# Corner-cases
is("Hello $world!", "Hello World!", "! is not a part of var names");
sub list_count (*@args) { +@args }
is(list_count("@list[]"), 1, 'quoted interpolation gets string context');
is(qq{a{chr 98}c}, 'abc', "curly brace delimiters don't interfere with closure interpolation");

# Quoting constructs
# The next test will always succeed, but if there's a bug it probably
# won't compile.
is(q0"abc\\d\\'\/", q0"abc\\d\\'\/", "raw quotation works");
is(q1"abc\\d\"\'\/", q0|abc\d"\'\/|, "single quotation works"); #"
is(q2"abc\\d\"\'\/", q0|abc\d"'/|, "double quotation works"); #"
is(qa"$world @list[] %hash{}", q0"$world 1 2 %hash{}", "only interpolate array");
is(qb"$world \\\"\n\t", "\$world \\\"\n\t", "only interpolate backslash");
is('$world \qq[@list[]] %hash{}', '$world 1 2 %hash{}', "interpolate quoting constructs in ''");

is(" \d[111] \d[107] ", ' o k ', "\\d[] respects whitespaces around it")

# L<S02/"Literals" /any of these by separating the numbers/>
is("x  \x[41,42,43]  x",     "x  ABC  x",  "\\x[] allows multiple chars (1)");
is("x  \x[41,42,00043]  x",  "x  ABC  x",  "\\x[] allows multiple chars (2)");
is("x  \d[65,66,67]  x",     "x  ABC  x",  "\\d[] allows multiple chars (1)");
is("x  \d[65,66,000067]  x", "x  ABC  x",  "\\d[] allows multiple chars (2)");

is("x  \x[41,42,43]]  x",    "x  ABC]  x", "\\x[] should not eat following ]s");
is("x  \d[65,66,67]]  x",    "x  ABC]  x", "\\d[] should not eat following ]s");