#!/usr/bin/perl
use strict;		      # 'strict' insists that all variables be declared
use diagnostics;	      # 'diagnostics' expands the cryptic warnings
use Cwd;
undef $/; # undefines the separator. Can read one whole file in one scalar.

# use main to avoid the curse of global variables
MAIN: {

  # Clean up cvs status

  my $dir = getcwd;
  $dir =~ s/^.*?(dev\/)/$1/g;
  $dir = $dir . "/" unless ($dir =~ /\/$/);
  
  my @lines = split("===", <>);
  
  my ($line, $file, $status, @files, @stats, $file_len);

  @files = ();
  @stats = ();
  $file_len = 0;
  
  foreach $line (@lines){

    next if ($line =~ /Status:\s+Up-to-date/i);

    next unless ($line =~ /Status:\s*(.*?)\n/i);
    $status = $1;
    
    next unless ($line =~ /Repository\s+revision:.*?(\/.*),/i);
    $file = $1;

    $file =~ s/^.*?(dev\/)/$1/g;
    $file =~ s/Attic\///g;

    $file =~ s/$dir//g;
    
    push(@files, $file);
    push(@stats, $status);

    if ($file_len < length($file)){
      $file_len = length($file);
    }
    
  }

  my $counter = 0;

  foreach $file (@files){

    $status = $stats[$counter]; $counter++;

    # pad with spaces
    $file = $file . " " x ($file_len - length($file));
    
    print "File: $file $status\n";
    
  }

}
