#!/usr/bin/pugs

use v6;
require Test;

=kwid

=head1 String interpolation

These tests derived from comments in http://use.perl.org/~autrijus/journal/23398

=cut

plan 5;

my $world = "World";

is("Hello $world", 'Hello World', 'double quoted string interpolation works');
is('Hello $world', 'Hello $world', 'single quoted string interpolation does not work (which is correct)');

is("2 + 2 = { 2+2 }", '2 + 2 = 4', 'double quoted closure interpolation works');
is('2 + 2 = { 2+2 }', '2 + 2 = { 2+2 }', 'single quoted closure interpolation does not work (which is correct)');

sub list_count (*@args) { +@args }
my @a = (1,2,3);
ok(list_count("@a") == 1, 'quoted interpolation gets string context');