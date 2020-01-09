#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{

  # Print the specified columns in a file. This works
  # like awk '{print $1}' but is less to type.
  # The column count starts from 1. Column 0 is the
  # last column.

  if (scalar(@ARGV) < 1){
    print "Usage: $0 colNumbers\n";
    exit(0);
  }

  # See if instead of printing a set of columns we should instead exclude
  # a set of columns.
  my $do_exclude = 0;
  if ($ARGV[0] eq '-v'){
    $do_exclude = 1;
  }

  my @c;
  my %cols;
  my $count = 0;
  foreach my $arg (@ARGV){
    next unless ($arg =~ /^\-*[\/\d]+$/);
    $cols{$arg} = $count++;
    push(@c, $arg);
  }

  foreach my $line (<STDIN>){
    $line =~ s/^\s*//g;
    $line =~ s/\s*$//g;
    next if ($line =~ /^\s*$/);
    
    my @vals = split(/\s+/, $line);
    my $nVals = scalar(@vals);
    #for (my $col = 1; $col <= $nVals; $col++){
    foreach my $col (@c){
      if ($col =~ /^(\d+)\/(\d+)/){
	# ratio
	my $a = $1; my $b = $2;
        if ($a > scalar(@vals) || $b > scalar(@vals)){
          print "Out of range for line $line\n";
        }else{
          print ( ($vals[$a-1]/$vals[$b-1]) . "\t" );
        }
      } else {
	my $col_exists = (exists $cols{$col} || exists $cols{$col-$nVals});
	if (  ( !$do_exclude &&  $col_exists ) ||
	      (  $do_exclude && !$col_exists )
	   ){
	  # Recall that our convention is that 1 is first column,
	  # 0 is last column, -1 is the one before last, etc.
          if ($col-1 < scalar(@vals)){
            print $vals[$col-1] . "\t";
          }else{
            print "none\t";
          }
	}
      }
    }
    
    print "\n";
  }

}
