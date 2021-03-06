#!/usr/bin/perl
use strict;                            # 'strict' insists that all variables be declared
use diagnostics;                       # 'diagnostics' expands the cryptic warnings
undef $/;

# make double f(int a, int b) into f(a, b)

MAIN:{
  my $junk;
  my @anarray;

  $_ = <>;

  #s/\/\/.*?\n/\n/g; # strip comments
  #s/\n/ /g;          # strip newlines
  s/\*//g;

  if (! /(^.*?)\((.*?)\)/s ){
    print "Failed to match function prototype\n";
    exit(0);
  }
  
  my $a=$1; my $b=$2; # $b is the list of args
  
  ($junk, $a)=split(' ', $a) if (/ /);
  
  my @lines = split("\n", $b);
  foreach my $line (@lines){

    my $comment = "";
    if ($line =~ /^(.*?)(\/\/.*?)$/){
      $line = $1; $comment = $2;
    }
    my $spaces = "";
    if ($line =~ /^(\s+)(.*?)$/){
      $spaces = $1;
      $line   = $2;
    }
    
    $line =~ s/\s*,\s*/, /g;
    my @args = split(',', $line);
    #print "$line--num args is " . scalar(@args) . "\n";
    foreach my $arg (@args){
      if ($arg =~ /^.* ([^\s]+?)\s*$/){
        $arg = $1;
      }
    }
    
    $line = join(', ', @args);

    $line = $spaces . $line;
    $line .= $comment;
  }

  $b = join("\n", @lines);
  
  print "$a\($b\);";
}
