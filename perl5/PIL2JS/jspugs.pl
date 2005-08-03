#!/usr/bin/perl

use warnings;
use strict;

use FindBin;
use File::Spec;
use lib File::Spec->catfile($FindBin::Bin, "lib");
use PIL2JS;
use Term::ReadLine;

my $term = Term::ReadLine->new("jspugs");

my $prompt = "jspugs> ";
my $OUT = $term->OUT || \*STDOUT;

print $OUT <<EOF;
Welcome to JSPugs -- Perl6 User's Golfing System
Type :h for help.

EOF

our %cfg;
*cfg = \%PIL2JS::cfg;
$cfg{output} = "output.html";
$cfg{verbose}++;
command_conf(pugs      => $cfg{pugs});
command_conf(pil2js    => $cfg{pil2js});
command_conf(preludepc => $cfg{preludepc});
command_conf(prelude   => $cfg{prelude});
command_conf(output    => $cfg{output});

while(defined($_ = $term->readline($prompt))) {
  next unless /\S/;
  $term->addhistory($_);
  s/\s*$//;

  if(my ($cmd, $arg) = /^:([hq]|pil(?:\.yaml)?|conf|precomp|js|l)\s*(.*)$/) {
    no strict "refs";
    $cmd =~ s/\./_/g;
    &{"command_$cmd"}($arg);
  } else {
    command_compile($_);
  }
}

sub command_q { exit }

sub command_h { print $OUT <<USAGE }
Commands available from the prompt:
:h                      = show this help message
:q                      = quit
:conf thing [new_value] = set the path to thing
                          (pugs|pil2js|preludepc|lib6|output)
:precomp                = precompile the Prelude
:pil <exp>              = show PIL of expression
:pil.yaml <exp>         = show PIL of expression dumped as YAML
<exp>                   = compile expression and save as HTML
:js <exp>               = compile expression and show it
:l filename.p6          = compile file and save as HTML
USAGE

sub command_conf {
  my ($what, $new) = split /\s+/, shift;

  my %human = (
    pugs      => "pugs",
    pil2js    => "pil2js",
    preludepc => "the precompiled Prelude",
    prelude   => "the Prelude sourcecode",
    output    => "the JavaScript output",
  );

  unless($what and $what =~ /^(?:pugs|pil2js|prelude(?:pc)?|output)$/) {
    print $OUT "Usage: :conf pugs|pil2js|prelude|preludepc|lib6|output [new_value]\n";
    return;
  }

  $cfg{$what} = $new if $new;

  my $descr =  "$human{$what}:" . " " x (30 - length $human{$what});
  my $path  =  $cfg{$what};
  $path     =~ s/^@{[ PIL2JS::pwd() ]}.//;
  $path    .=  " " x (20 - length $path);
  print $OUT
    "* Path to $descr $path [" .
    (-d $cfg{$what} || (-f $cfg{$what} && -s $cfg{$what}) ? "OK" : "NOT FOUND") .
    "]\n";
}

sub command_precomp {
  my $js = eval { precomp_module_to_mini_js "-I", PIL2JS::pwd("lib6"), "-MPrelude::JS" };
  die $@ if $@;

  open my $fh, ">", $cfg{preludepc} or die "Couldn't open \"$cfg{preludepc}\" for writing: $!\n";
  print $fh $js                     or die "Couldn't write to \"$cfg{preludepc}\": $!\n";
  close $fh                         or die "Couldn't close \"$cfg{preludepc}\": $!\n";

  command_conf("preludepc");
}

sub command_pil {
  my $pil = eval { compile_perl6_to_pil "-e", $_[0] };
  print $OUT $@ and return if $@;
  print $OUT $pil;
}

sub command_pil_yaml {
  my $pil  = eval { compile_perl6_to_pil "-e", $_[0] };
  print $OUT $@ and return if $@;
  my $yaml = eval { run_pil2js("--yaml-dump", \$pil) };
  print $OUT $@ and return if $@;
  print $OUT $yaml;
}

sub command_compile { compile("-e", $_[0]) }
sub command_l       { compile($_[0]) }

sub compile {
  my $html = eval { compile_perl6_to_htmljs_with_links @_ };
  print $OUT $@ and return if $@;

  open my $fh, ">", $cfg{output} or die "Couldn't open \"$cfg{output}\" for writing: $!\n";
  print $fh $html                or die "Couldn't write to \"$cfg{output}\": $!\n";
  close $fh                      or die "Couldn't close \"$cfg{output}\": $!\n";

  command_conf("output");
}

sub command_js {
  my $pil = eval { compile_perl6_to_mini_js "-e", $_[0] };
  print $OUT $@ and return if $@;
  print $OUT "$pil\n";
}