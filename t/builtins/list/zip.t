#!/usr/bin/pugs

use v6;
require Test;

=pod

The zip() builtin and operator tests

L<S03/"C<zip>">

=cut

plan 12;

{
	my @a = (0, 2, 4);
	my @b = (1, 3, 5);

	my @e = (0 .. 5);

	my @z; eval '@z = zip(@a; @b)';
	my @y; eval '@y = (@a ¥ @b)';
	my @x; eval '@x = (@a Y @b)';

	todo_is(~@z, ~@e, "simple zip");
	is(~@y, ~@e, "also with yen char");
	is(~@x, ~@e, "also with Y char");
};

{
	my @a = (0, 3);
	my @b = (1, 4);
	my @c = (2, 5);

	my @e = (0 .. 5);

	my @z; eval '@z = zip(@a; @b; @c)';
	my @y; eval '@y = (@a ¥ @b ¥ @c)';
	my @x; eval '@x = (@a Y @b Y @c)';

	todo_is(~@z, ~@e, "zip of 3 arrays");
	is(~@y, ~@e, "also with yen char");
	is(~@x, ~@e, "also with Y char");
};

{
	my @a = (0, 4);
	my @b = (2, 6);	
	my @c = (1, 3, 5, 7);

	my @e = (0 .. 7);

	my @z; eval '@z = zip(zip(@a; @b); @c)';
	my @y; eval '@y = ((@a ¥ @b) ¥ @c)';
	my @x; eval '@x = ((@a Y @b) Y @c)';

	todo_is(~@z, ~@e, "zip of zipped arrays with other array");
	todo_is(~@y, ~@e, "also as ¥");
	todo_is(~@x, ~@e, "also as Y");
};

{
	my @a = (0, 2);
	my @b = (1, 3, 5);
	my @e = (0, 1, 2, 3, undef, 5);
	
	my @z; eval '@z = (@a ¥ @b)';
	is(@z, @e, "bug in zipping - should use length of longest");
}

{
	my @a;
	my @b;
	
	eval '(@a ¥ @b) = (1, 2, 3, 4)';
	todo_is(@a, (1, 3), "first half of two zipped arrays as lvalues");
	todo_is(@b, (2, 4), "second half of the lvalue zip");
}