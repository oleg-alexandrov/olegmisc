#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

sub read_matrix {
  my $file = $_[0];
  print "Will read $file\n";

  my @mat = ();
  open(FILE, "<$file");
  foreach my $line (<FILE>){
    $line =~ s/^\s*//g;
    $line =~ s/\s*$//g;
    my @vals = split(/\s+/, $line);
    push (@mat, \@vals);
  }

  close(FILE);
  
  return \@mat;
}

sub matrix_count_rows_cols { 
  my $r_mat = $_[0];
  my $num_rows = scalar(@$r_mat);
  my $num_cols = scalar(@{$r_mat->[0]});
  ($num_rows,$num_cols);
}

sub matrix_multiply {
    my ($r_mat1,$r_mat2)=@_;
    my ($r_product);
    my ($r1,$c1)=matrix_count_rows_cols($r_mat1);
    my ($r2,$c2)=matrix_count_rows_cols($r_mat2);

    #print $c1,$c2,"\n";
    #print $r1,$r2,"\n";
    
    die "matrix 1 has $c1 columns and matrix 2 has $r2 rows>" 
        . " Cannot multiply\n" unless ($c1==$r2);
    for (my $i=0;$i<$r1;$i++) {
        for (my $j=0;$j<$c2;$j++) {
            my $sum=0;
            for (my $k=0;$k<$c1;$k++) {
                $sum+=$r_mat1->[$i][$k]*$r_mat2->[$k][$j];
            }
            $r_product->[$i][$j]=$sum;
        }
    }
    return $r_product;
}

sub print_matrix { 
  my $mat = $_[0];
  my ($rows, $cols) = matrix_count_rows_cols($mat);
  print "Rows $rows and columns $cols\n";
  for (my $row = 0; $row < $rows; $row++){
    for (my $col = 0; $col < $cols; $col++){
      print $mat->[$row]->[$col] . " ";
    }
    print "\n";
  }
}

sub vec_to_mat {
  my $x = $_[0];
  my $y = $_[1];
  my $z = $_[2];
  my @a = ($x);
  my @b = ($y);
  my @c = ($z);
  my @d = (1);
  my @mat = (\@a, \@b, \@c, \@d);
  return \@mat;
}

sub mat_to_vec {
  my $mat = $_[0];
  my ($rows, $cols) = matrix_count_rows_cols($mat);
  if ($cols != 1 and $rows != 4){
    die "Must have one column and 4 rows.";
  }
  my @ans = ();
  for (my $row = 0; $row < $rows - 1; $row++){
    push (@ans, $mat->[$row]->[0]);
  }
  return @ans;
}

my $height = 1737400.0;
MAIN:{
  
  if (scalar(@ARGV) < 2){
    print "Usage: $0 file.ply trans.txt\n";
    exit(0);
  }

  my $ply_file = $ARGV[0];
  my $trans_file = $ARGV[1];
  my $csv_file = $ply_file;
  $csv_file =~ s/\.\w+$/_trans.csv/g;

  print "Inputs are $ply_file $trans_file\n";
  print "Will write: $csv_file\n";

  my $trans = read_matrix($trans_file);
  print_matrix($trans);
  
  #exit(1);
  
  if ($ply_file eq $csv_file){
    print "Input is already csv.\n";
    exit(1);
  }
  open(FILE, "<$ply_file");
  open(FILE2, ">$csv_file");
  print "Writing: $csv_file\n";
  foreach my $line (<FILE>){
    if ($line =~ /^[a-z]/i || $line =~ /^\s*$/){
      next;
    }
    
    $line =~ s/^\s*//g;
    my @vals = split(/\s+/, $line);
    
    # Go from haz cam frame to nav_cam frame by flipping 180 degreees
    $vals[0] *= -1; 
    $vals[1] *= -1; 

    # Per kalib's report
    $vals[0] += 0.07930373;
    $vals[1] += 0.00029806;
    $vals[2] += 0.00176547;

    my $point = vec_to_mat($vals[0], $vals[1], $vals[2]);
    #print "line is $line\n";
    #print "Point is: \n";
    #print_matrix($point);

    my $trans_point = matrix_multiply($trans, $point);

    #print "Trans point is: \n";
    #print_matrix($trans_point);

    @vals = mat_to_vec($trans_point);

    #print "trans vals are $vals[0] $vals[1] $vals[2]\n";
           
    printf(FILE2 "%0.17g %0.17g %0.17g\n",
           $vals[0], $vals[1], $vals[2] + $height);
  }
  close(FILE);
  close(FILE2);
}
