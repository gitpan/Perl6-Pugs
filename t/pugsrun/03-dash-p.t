#!/usr/bin/pugs

use v6;
require Test;

=pod

Test C<-p> implementation

The C<-p> command line switch mimics the Perl5 C<-p> command line
switch, and wraps the whole script in

  while (<>) {
    ...         # your script
    print;
  };

=cut

my @examples;

push @examples, '-p';
push @examples, '-p "-e1;"';
push @examples, '-pe ";"';
push @examples, '-pe ""';

plan +@examples;

diag "Running under $?OS";

my ($pugs,$redir_in,$redir_out) = ("./pugs", "<", ">");

if ($?OS ~~ rx:perl5{MSWin32|msys|mingw}) {
  $pugs = 'pugs.exe';
  $redir_out = '>';
  # $redir_in = '<';
};

my $str = "
foo
bar
";

my $h = open '>temp-ex-input';
$h.print($str);
$h.close();

for @examples -> $ex {
  my $command = "$pugs $ex $redir_in temp-ex-input $redir_out temp-ex-output";
  diag $command;
  system $command;

  my $expected = $str;
  my $got      = slurp "temp-ex-output";
  unlink "temp-ex-output";

  todo_is $got, $expected, "$ex works like cat";
}

unlink "temp-ex-input";