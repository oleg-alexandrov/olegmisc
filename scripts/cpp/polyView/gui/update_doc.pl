#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{

  # Take the documentation from a text file and insert it into a C++
  # file to be compiled and embedded with the executable.
  
  my $txtDocFile = "documentation.html";
  my $cppDocFile = "documentation.cpp";

  open(FILE, "<$txtDocFile");
  my $preProcDocText = join("", <FILE>);
  close(FILE);
  
  my $docText = "";
  foreach my $line ( split("\n", $preProcDocText)  ){
    $line =~ s/\"/\\\"/g; # Escape any quotes
    $docText .= '"' . $line . '\\n"' . "\n";
  }
  $docText =~ s/\s*$//g;
  
  open(FILE, "<$cppDocFile") || die "Cannot open file: $cppDocFile\n";
  my $text = join("", <FILE>);
  close(FILE);

  my $tag = 'char docText[] =' . $docText . ";\n";
  
  $text =~ s!(^.*?[ ]*//[ ]* Begin.*?\n).*?([ ]*//[ ]*End.*?)$! $1 . $tag . $2 !egs;

  open(FILE, ">$cppDocFile");
  print FILE "$text\n";
  close(FILE);
  
}

