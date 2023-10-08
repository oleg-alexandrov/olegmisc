#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

# given images in small_dir and a larger set in big_dir, and a time delta_t,
# find images in dir2 closer than delta_t in time from images in dir1.
# all image files must be timestamps, a floating point number in seconds.
MAIN:{
  
  if (scalar(@ARGV) < 3){
    print "Usage: $0 small_dir big_dir delta_t\n";
    exit(0);
  }

  my $small_dir = $ARGV[0];
  my $big_dir   = $ARGV[1];
  my $delta_t   = $ARGV[2];

  print "small big delta_t is $small_dir $big_dir $delta_t\n";
  $delta_t  = int($delta_t);
  
  my %small_hash;
  my @small_files = glob("$small_dir/*jpg");
  foreach my $file (@small_files){
    next unless($file =~ /\/([\d\.]*?)\.jpg/);
    my $num = $1;
    $num = int($num);
    #print "num is $num\n";
    $small_hash{$num} = 1;
  }

  my %big_hash;
  my @big_files = glob("$big_dir/*jpg");
  foreach my $file (@big_files){
    next unless ($file =~ /\/([\d\.]*?)\.jpg/);
    my $num = int($1);
    for (my $ind = $num - $delta_t; $ind <= $num + $delta_t; $ind++){
      next unless (exists $small_hash{$num});
      $big_hash{$file} = 1;
    }
  }

  foreach my $file (sort {$a cmp $b} keys %big_hash){
    print "$file\n";
  }
  
}
