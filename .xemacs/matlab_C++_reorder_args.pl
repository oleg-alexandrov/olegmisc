#!/usr/bin/perl
use strict;		      # 'strict' insists that all variables be declared
use diagnostics;	      # 'diagnostics' expands the cryptic warnings
undef $/; # undefines the separator. Can read one whole file in one scalar.

# The matlab external interfaces sucks. If I change in the main matlab program the order of arguments for 
# a C++ function, it is a pain to reorder the arguments in the function itself.

my (@lines, $fun, $dup, %order, $count, @args, %lines_hash);

@lines=split("\n", <>);

$fun=shift @lines;
$dup=$fun;
$dup =~ s/^.*?\((.*?)\).*?$/$1/g;
@args = split(",", $dup); 

$count=0;
foreach ( @args ){
  s/\s//g;
  $order{$_}=$count; # will order as they show up in @args
  $count++;
}

# extract the variable names
foreach (@lines){
  $dup=$_;
  next if (/^\s*$/);
  $dup =~ s/^\s*\w.*?\s+(\w+)\s.*?$/$1/g;
  $lines_hash{$dup}=$_;
}

# order
$count=0;
print "$fun\n\n";
foreach ( sort { $order{$a} <=> $order{$b} } keys %lines_hash ){
  $lines_hash{$_} =~ s/\[\d+\]/\[$count\]/g; # change the corresponding index
  print "$lines_hash{$_}\n";
  $count++;
}
