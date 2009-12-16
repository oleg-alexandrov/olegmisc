#!/usr/bin/perl
use strict;	   # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
undef $/;          # read one whole file in one scalar

MAIN:{

  # Resample a contour
  
  my ($text, $preamble);
  open(FILE, "<$ARGV[0]");  
  $text = <FILE>;
  close(FILE);

  if ($text !~ /(^.*?\n)(\d.*?)$/s){
    print "Could not match preamble\n";
    exit(0);
  }

  $preamble = $1;
  $text = $2;

  my @contours = split("NEXT\n", $text);

  my $sample_rate = 50;
  my $contour;

  foreach $contour (@contours){

    my @points = split("\n", $contour);
    my @resampled_contours = ();

    my $point;
    my $count = 0;
    foreach $point (@points){

      if ($count % $sample_rate == 0){
        push(@resampled_contours, $point);
      }

      $count++;

    }

    # Form closed resampled contour
    $contour = join ("\n", @resampled_contours) . "\n" . $resampled_contours[0] . "\n";
  }
  
  $text = $preamble . join ("NEXT\n", @contours);
  $text =~ s/\s*\n$//g;
  
  print "$text\n";
}
