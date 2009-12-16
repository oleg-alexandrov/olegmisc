#!/usr/bin/perl
use strict;	   # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
use lib $ENV{HOME} . '/bin';

require 'splitByMatchingParen.pl';

undef $/;          # read one whole file in one scalar

MAIN:{

  # Insert at prescribed position another argument to a function construct like
  # void myfun(int a,             // first arg, note the comma in comments
  #            double b,          // second arg,  axUp6Ze 
  #            double c, double d // third and fourth, 
  #            );

  # Position starts from 1, not from 0.

  # Test with addArg.pl myfun "int i // some arg" 5 testAddArgs.cpp
  
  if (scalar(@ARGV) < 4){
    print "Usage: $0 functionName argToAdd argPos files\n";
  }

  my $fun      = shift @ARGV;
  my $argToAdd = shift @ARGV;
  my $argPos   = shift @ARGV;
  my @files    = @ARGV;

  my $tag = 'xu6Ao u9 '; # something unlikely

  # Visit each file, get the chunk of text starting with $fun,
  # and insert the new argument at the prescribed location.
  foreach my $file (@files){

    open(FILE, "<$file"); my $text = <FILE>; close(FILE);

    $text =~ s/(^|\n)([^\n]*?\Q$fun\E\s*\()/$1$tag$2/g;  
    my @blocks = split(/$tag/, $text);

    foreach my $block (@blocks){

      next unless ($block =~ /^.*?\Q$fun\E\s*\(/);
      
      #print "------------------------\n";
      #print "block is\n'$block'\n\n";
      #print "Insert at pos $argPos\n";
      
      $block = addArg($block, $argToAdd, $argPos);
      
      #print "\nblock is\n'$block'\n";
      #print "------------------------\n";
      
    }

    $text = join('', @blocks);
    open(FILE, ">", $file . "_out"); print FILE $text; close(FILE);
    
  }
  
}

sub addArg{

  # Add $argToAdd at postion $argPos for function $fun.
  
  my $fun      = shift;
  my $argToAdd = shift;
  my $argPos   = shift;

  if ($argPos <= 0){
    $argPos = 1; # We start from 1
  }
  
  # Split void 'myfun(int a);' into 'void myfun(', 'int a', ');'
  my ($before, $argList, $after) = splitByMatchingParen($fun);

  if ($argList eq "" and $after eq ""){
    # Could not split properly
    return $before;
  }

  # Strip leading space from each line (will put it back later when indenting)
  $argList =~ s/(^|\n)[ ]*([^\s])/$1$2/g;
  
  # New arg will be on its on line
  $argToAdd .= "\n";
  $argToAdd =~ s/^\s*//g;
  $argToAdd = "\n" . $argToAdd;
  
  # Encode any comma in comments to not confuse such comma with the comma
  # separating function arguments
  my $comma = ' axUp6Ze '; # something unlikely to encode the comma to
  while ($fun =~ /$comma/){
    $comma .= 'a';         # make sure the funny text above does not indeed occur 
  }
  $argList  =~ s/(\/\/.*?\n)/encodeComma($1, $comma)/eg;
  $argToAdd =~ s/(\/\/.*?\n)/encodeComma($1, $comma)/eg;

  # Strip comma in $argToAdd if any
  $argToAdd =~ s/,//g;
  
  # We want to group an argument with its trailing comment and newlines if any
  $argList =~ s/(,)(\s*\/\/.*?\n)/$2$1/g; # move comma beyond the comment
  $argList =~ s/(,)(\s*)/$2$1/g;          # move comma beyond all newlines
  
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

  # Put back together the arguments
  $argList = join(",", @outargs);

  # Move the comma back and decode any comma within comments
  $argList =~ s/(\s*)(,)/$2$1/g;
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
  
  # Put just one newline at the end
  $argList =~ s/^\s*//g;
  $argList  =~ s/\s*$//g;
  $argList .=  "\n";

  # The case of just one argument
  $argList =~ s/^\s*,\s*//g;

  # Strip extra spaces
  $argList =~ s/[ \t]*\n/\n/g;
  
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

