#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{

  # Given two very similar files, but some
  # having more entries than others, insert empty lines so that
  # for each n, line number n in both files if non-empty
  # starts with the same word. In short, make two files
  # easier to compare. Default diff and vimdiff gets confused.
  
  if (scalar(@ARGV) < 2){
    print "Usage: $0 \n";
    exit(0);
  }

  open(FILE, "<$ARGV[0]"); my @lines0 = <FILE>; close(FILE); 
  open(FILE, "<$ARGV[1]"); my @lines1 = <FILE>; close(FILE); 

  my (%keys0, %keys1, %keys_all);

  foreach my $line (@lines0){
    $line =~ s/^\s*//g; $line =~ s/\s*$//g;
    my $key = $line;
    if ($line =~ /^(.*?[^\s]+)\s/){
      $key = $1;
      #print "$key\n";
    }
    $keys0{$key} = $line;
    $keys_all{$key} = $line;
  }

  foreach my $line (@lines1){
    $line =~ s/^\s*//g; $line =~ s/\s*$//g;
    my $key = $line;
    if ($line =~ /^(.*?[^\s]+)\s/){
      $key = $1;
      #print "$key\n";
    }
    $keys1{$key} = $line;
    $keys_all{$key} = $line;
  }

  my $file0_out = $ARGV[0] . ".out.txt";
  my $file1_out = $ARGV[1] . ".out.txt";
  print "Writng: $file0_out $file1_out\n";
  open(FILE0, ">$file0_out");
  open(FILE1, ">$file1_out");

  foreach my $key (sort { $keys_all{$a} cmp $keys_all{$b} } keys %keys_all){
    if (exists $keys0{$key}){
      print FILE0 "$keys0{$key}\n";
    }else{
      print FILE0 "\n"; 
    }
    
    if (exists $keys1{$key}){
      print FILE1 "$keys1{$key}\n";
    }else{
      print FILE1 "\n"; 
    }
    
  }

  close(FILE0);
  close(FILE1);
}
