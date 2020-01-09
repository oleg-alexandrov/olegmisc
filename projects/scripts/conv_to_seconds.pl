#!/usr/bin/perl
use strict;	   # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
undef $/;          # read one whole file in one scalar

sub conv_to_seconds {

  # Convert 23:34 and 1:23:54 to seconds

  my $time = shift;
  $time =~ s/[^\d\:\.\+\-]//g;
  
  my @items = split(":", $time);

  my $num_items = scalar(@items);
  
  my $seconds = 0;

  my $count;
  my $factor = 1;
  for ($count = 0; $count <= $num_items - 1; $count++){

    $seconds = $seconds + $factor*$items[$num_items - 1 - $count];
    $factor = $factor*60;
  }

  return $seconds;
}

1;
