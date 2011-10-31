#!/usr/local/bin/perl
use strict;		      # 'strict' insists that all variables be declared
use diagnostics;	      # 'diagnostics' expands the cryptic warnings

my @lines=<>;

my $maxcol=0;
my ($a, $b);

foreach (@lines){
  chomp;
  if ( /(.*?)\%(.*?)$/ ){
    $a=$1; $b=$2;
  }else{
   $a=$_; $b=""; 
  }

  $maxcol=length($a) if ( length($a) > $maxcol );
}

my $pad=2;


foreach (@lines){

  if ( /(.*?)\%(.*?)$/ ){
    $a=$1; $b=$2;
  }else{
   $a=$_; $b=""; 
  }


  $a=$a . " " x ($maxcol - length($a) + $pad);

  if (! $b =~ /^\s*$/){
    print "$a%$b\n";
  }else{
   print "$a\n"; 
  }
  
}

