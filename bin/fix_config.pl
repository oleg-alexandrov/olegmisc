#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{

  # Move some lines according to deired logic
  
  if (scalar(@ARGV) < 1){
    print "Usage: $0 \n";
    exit(0);
  }

  my $file = $ARGV[0];
  
  print "file is $file\n";

  open(FILE, "<$file");
  my @lines = <FILE>;
  close(FILE);

  my $haz_line = "";
  my $sci_line = "";
  my $brace_count = 0;
  my $last_brace_index = 0;
  my $start = 0;
  my $end = 0;
  
  for (my $count = 0; $count < scalar(@lines); $count++){
    
    my $line = $lines[$count];

    if ($line =~ /robot_camera_calibrations/){
      $start = 1;
    }
    
    if ($line =~ /nav_cam_to_haz_cam_timestamp_offset/){
      $haz_line = $line;
      $lines[$count] = "";
    }
    if ($line =~ /nav_cam_to_sci_cam_timestamp_offset/){
      $sci_line = $line;
      $lines[$count] = "";
    }

    if (!$start){
      next;
    }

    if ($end){
      next;
    }
    
    if ($line =~ /\{/){
      $brace_count++;
    }
    
    if ($line =~ /\}/){
      $brace_count--;
    }

    if ($brace_count == 0){
      # arrived at the matching brace, insert the text we removed earlier
      $lines[$count - 1] =~ s/\s*\n/,\n/g;
      $lines[$count] = $haz_line . $sci_line . $lines[$count];
      $lines[$count] =~ s/,(\s*\}\s*)$/$1/sg;
      $end = 1;
    }
  }

  print "Writing: $file\n";
  open(FILE, ">$file");
  foreach my $line (@lines){
    print FILE "$line";
  }
  close(FILE);
}

