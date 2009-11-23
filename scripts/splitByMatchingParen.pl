#!/usr/bin/perl
use strict;                # 'strict' insists that all variables be declared
use diagnostics;           # 'diagnostics' expands the cryptic warnings

# MAIN:{
#   my $text = "aa(b(c)d)ef";
#   my ($before, $mid, $after) = splitByMatchingParen($text);
#   print "before is $before\n";
#   print "mid    is $mid\n";
#   print "after  is $after\n";
# }

sub splitByMatchingParen{

  # Split 'aa(b(c)d)ef' into 'aa(', 'b(c)d', ')ef'.
  
  my $text = shift;
  my $lpar = '(';
  my $rpar = ')';
  
  my @chars = split("", $text);
  my $len   = scalar( @chars );
  
  my $beg = findFirstCharPos(\@chars, $lpar);
  if ($beg < 0){
    return ($text, "", "");
  }

  my $end = matchClosingParen($beg, \@chars, $lpar, $rpar);
  if ($end < 0){
    return ($text, "", "");
  }

  my $before = ""; my $mid = ""; my $after = "";

  for (my $i = 0; $i < $len; $i++){

    my $char = $chars[$i];
    
    if    ( $i <= $beg ){  $before .= $char; }
    elsif ( $i <  $end ){  $mid    .= $char; }
    else                {  $after  .= $char; }
    
  }
  
  return ($before, $mid, $after);
  
}

sub findFirstCharPos{

  my $chars = shift;
  my $char  = shift;

  my $success = 0;
  my $pos     = 0;
  my $len     = scalar (@$chars);
  
  while ($pos < $len){
    if ($chars->[$pos] eq $char){
      return $pos;
    }
    $pos++;
  }
  print "Failed to match \'$char\' in the text " . join("", @$chars) . "\n";
  return -1;
}

sub matchClosingParen{

  my $beg   = shift;
  my $chars = shift;
  my $lpar  = shift;
  my $rpar  = shift;

  my $len = scalar ( @$chars );

  if ($beg < 0 or $beg >= $len or $chars->[$beg] ne $lpar){
    print "Invalid starting position\n";
    exit(0);
  }

  
  my $count = 1;
  
  for (my $i = $beg + 1 ; $i < $len ; $i++) {

    my $ch = $chars->[$i];
    
    if ($ch eq $rpar) {
      $count--;
    }elsif ($ch eq $lpar){
      $count++;
    }

    if ($count == 0){
      return $i; # position of the closing paren
    }
  }

  return -1; # could not find the closing paren
}

1;
