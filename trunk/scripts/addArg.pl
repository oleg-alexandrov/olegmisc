#!/usr/bin/perl
use strict;	   # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
require 'splitByMatchingParen.pl';

undef $/;          # read one whole file in one scalar

MAIN:{

  if (scalar(@ARGV) < 3){
    print "Usage: $0 functionName argToAdd argPos\n";
    #exit(0);
  }

  my $fun    = 'void myfun(int a, // first arg, note the comma in comments
                 double b, // second arg,  axUp6Ze 
                 double c, double d // third and fourth, 
                 );';

  my $argPos = shift @ARGV;
  
  my $argToAdd = "int q // an integer";

  print "Function is\n$fun\n\n";
  print "Insert at pos $argPos\n";

  $fun = addArg($fun, $argToAdd, $argPos);
  
  print "\nFunction is\n$fun\n";
}

sub addArg{
    
  # Insert at prescribed position another argument to a function construct like
  # void myfun(int a, // first arg, note the comma in comments
  #               double b, // second arg,  axUp6Ze 
  #               double c, double d // third and fourth, 
  #               );';

  # Position starts from 1, not from zero.
  

  my $fun      = shift;
  my $argToAdd = shift;
  my $argPos   = shift;

  if ($argPos <= 0){
    print "Expecting argument position to be >= 1\n";
    exit(0);
  }
  
  my $comma = ' axUp6Ze '; # something unlikely to encode the comma to
  while ($fun =~ /$comma/){
    $comma .= 'a';         # make sure the funny text above does not indeed occur 
  }

  my ($before, $argList, $after) = splitByMatchingParen($fun);

  
  # New arg will be on its on line
  $argToAdd .= "\n";
  $argToAdd =~ s/^\s*//g;
  $argToAdd = "\n" . $argToAdd;
  
  # Encode any comma in comments as that can be confusing
  $argList  =~ s/(\/\/.*?[\n]+)/encodeComma($1, $comma)/eg;
  $argToAdd =~ s/(\/\/.*?[\n]+)/encodeComma($1, $comma)/eg;

  # Move comma to the end of line (we want to
  # group an argument with its comment if any)
  $argList =~ s/(,)(\s*\/\/.*?[\n]+)/$2$1/g;

  my @args = split(",", $argList);
  my $len  = scalar (@args);

  # Do the insertion
  my @outargs = ();
  for (my $i = 0; $i < $len ; $i++){
    
    if ($i+1 == $argPos){
      push(@outargs, $argToAdd);
    }
    
    push(@outargs, $args[$i]);
    
  }

  # If to insert after all other arguments
  if ( $argPos > $len ){
    push(@outargs, $argToAdd);
  }

  $argList = join(",", @outargs);

  # Move comma back and decode the comma in comments
  $argList =~ s/([ \t\r]*[\n]+[ \t\r]*)(,)/$2$1/g;
  $argList =~ s/(\s*\/\/.*?)(,)/$2$1/g;
  $argList =~ s/$comma/,/g;

  
  # Indent all but the first argument
  my $indentLevel = " " x length($before);
  @outargs = split("\n", $argList);

  for (my $i = 0; $i < scalar(@outargs); $i++){

    if ($i > 0) { $outargs[$i] =~ s/^\s*/$indentLevel/g; }
    else        { $outargs[$i] =~ s/^\s*//g;             }
 
  }

  $argList = join("\n", @outargs);
  
  # Rm excessive spaces, put space at the end
  $argList  =~ s/^\s*//g;
  $argList  =~ s/\n[ \t\r]+\n/\n/g;
  $argList  =~ s/\s*$//g;
  $argList .=  "\n";

  $after = $indentLevel . $after;
  $fun = $before . $argList . $after;

  return $fun;

}

sub encodeComma{

  my $text  = shift;
  my $comma = shift;
  
  $text =~ s/,/$comma/g;

  return $text;
  
}

