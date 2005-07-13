#!/usr/bin/pugs

use v6;
use Test;

plan 1;

sub nonce () { return (".$*PID." ~ int rand 1000) }

my $tmpfile = "temp-test" ~ nonce();
{
  my $fh = open($tmpfile, :w) err die "Couldn't open \"$tmpfile\" for writing: $!\n";
  print $fh: "TestÄÖÜ\n\n0";
  close $fh err die "Couldn't close \"$tmpfile\": $!\n";
}

{
  my $fh = open $tmpfile err die "Couldn't open \"$tmpfile\" for reading: $!\n";
  my @chars;
  push @chars, $_ while defined($_ = getc $fh);
  close $fh err die "Couldn't close \"$tmpfile\": $!\n";

  is ~@chars, "T e s t Ä Ö Ü \n \n 0", "getc() works even for utf-8 input";
}

END { unlink $tmpfile }