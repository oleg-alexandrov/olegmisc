#!/usr/bin/perl
use strict;	   # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
undef $/;          # read one whole file in one scalar

MAIN:{
  
  my $text;
  my @files = <*>;
  my %missing;
  
  foreach my $file (@files){
    print "$file\n";

    next if (-d $file);
    
    open(FILE, "<$file");
    my $text = <FILE>;
    close(FILE);


    my @vals = ($text =~ /href\s*=\s*\"\s*([^\/]*?)\s*\"/g);

    foreach my $val (@vals){
      next if ($val =~ /mailto/);
      next if (exists $missing{$val});
      $missing{$val} = 1;
      print "wget www.math.umn.edu/~aoleg/emacs/$val; sleep 5;\n";
    }
  }

}
