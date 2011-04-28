#!/usr/bin/perl
use strict;	   # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
use Scalar::Util qw(looks_like_number);
use List::Util   qw(max min);

undef $/;          # read one whole file in one scalar

MAIN:{

  # Look at the maximum discrepancy between corresp. numbers in file1 and file2
  
  if (scalar(@ARGV) < 2){
    print "Usage: $0 file1 file2\n";
    exit(0);
  }
  
  my $file1 = shift @ARGV;
  my $file2 = shift @ARGV;
  open(FILE, "<$file1");  my @val1 = split("\n", <FILE>);  close(FILE);
  open(FILE, "<$file2");  my @val2 = split("\n", <FILE>);  close(FILE);
  if ( scalar(@val1) != scalar(@val2) ){
    print "Warning: Files $file1 and $file2 do not have the same number of rows.\n";
  }
  my $numRows = min( scalar(@val1), scalar(@val2) );
  
  my ($max_err, $max_row, $max_col) = (0, 0, 0);

  for (my $row = 0; $row < $numRows; $row++){
    
    my @l1 = split(/(,|\s+)/, $val1[$row]);
    my @l2 = split(/(,|\s+)/, $val2[$row]);

    if ( scalar(@l1) != scalar(@l2) ){
      print "Warning: Row " . ($row + 1) . " does not have the same number "
         . "of elements in $file1 and $file2.\n";
    }
    my $numlen = min( scalar(@l1), scalar(@l2) );
    
    my $col_num = -1;
    for (my $col = 0; $col < $numlen; $col++){
      
      if ( looks_like_number( $l1[$col] ) && looks_like_number( $l2[$col] ) ){

        $col_num++;
        
        my $err = abs( $l1[$col] - $l2[$col] );
        if ($err > $max_err){
          $max_err = $err;
          $max_col = $col_num;
          $max_row = $row;
        }
        
        #print "$l1[$col] $l2[$col] $err\n";
        #print "Number $l1[$col]\n";
        
      }else{
        #print "Not number $l1[$col]\n";
      }
      
    }
    
  }

  print "Max err is $max_err at line " . ($max_row + 1)
     .                  " and column " . ($max_col + 1) 
        #.               " in files $file1 $file2"
        . "\n";
  
  #print "Lines:\n";
  #print $val1[$max_row] . "\n";
  #print $val2[$max_row] . "\n";
  
}
