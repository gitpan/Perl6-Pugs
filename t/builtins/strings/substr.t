#!/usr/bin/pugs

use v6;
require Test;

plan 23;

{ # read only
	my $str = "foobar";

	is(substr($str, 0, 1), "f", "first char");
	is(substr($str, -1), "r", "last char");
	is(substr($str, 1, 2), "oo", "arbitrary middle");
	is(substr($str, 3), "bar", "length omitted");
	is(substr($str, 3, 10), "bar", "length goes past end");
	is(substr($str, 20, 5), undef, "substr outside of string");
	is(substr($str, -100, 10), undef, "... on the negative side");

	is(substr($str, 0, -2), "foob", "from beginning, with negative length");
	is(substr($str, 2, -2), "ob", "in middle, with negative length");
	is(substr($str, 3, -3), "", "negative length - gives empty string");

	is($str, "foobar", "original string still not changed");
};

{ # replacement
	my $str = "foobar";

	substr($str, 2, 1, "i");
	is($str, "foibar", "fourth arg to substr replaced part");

	substr($str, -1, 1, "blah");
	is($str, "foibablah", "longer replacement expands string");

	substr($str, 1, 3, "");
	is($str, "fablah", "shorter replacement shrunk it");

	substr($str, 1, -1, "aye");
	is($str, "fayeh", "replacement with negative length");
};

{ # as lvalue
	my $str = "gorch ding";

	eval 'substr($str, 0, 5) = "gloop"';
	todo_is($str, "gloop ding", "lvalue assignment modified original string");
	
	my $r;
	eval '$r = \substr($str, 0, 5)';
	ok(ref($r), '$r is a reference');
	todo_eval_is('$$r', "gloop", '$r referent is eq to the substring');

	eval '$$r = "boing"';
	todo_is($str, "boing ding", "assignment to reference modifies original");
	todo_eval_is('$$r', "boing", '$r is consistent');

	my $o;
	eval '$o = \substr($str, 3, 2)';
	todo_eval_is('$$o', "ng", "other ref to other lvalue");
	eval '$$r = "foo"';
	todo_is($str, "foo ding", "lvalue ref size varies but still works");
	todo_eval_is('$$o', " d", "other lvalue wiggled around");
};

{ # from synopsis 9
# This test is not working as-is
#	todo_eval_is('substr("camel", 0|1, 2&3)', (("ca"|"am") & ("cam"|"ame")), "junctive substr");
}