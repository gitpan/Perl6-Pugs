#!/usr/bin/pugs

use Test;
use v6;

plan 4;

=head1 DESCRIPTION

This test tests the C<pick> builtin.

=cut

my @array = <a b c d>;
eval_ok '@array.pick == any <a b c d>', "pick works on arrays", :todo;

my %hash = (a => 1);
# eval is superfluous?
eval_is '%hash.pick.key',   "a", "pick works on hashes (1)", :todo;
eval_is '%hash.pick.value', "1", "pick works on hashes (2)", :todo;

my $junc = (1|2|3);
ok 1|2|3 == $junc.pick, "pick works on junctions";