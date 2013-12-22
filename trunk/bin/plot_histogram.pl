#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{

  if (scalar(@ARGV) < 1){
    print "Usage: $0 file.tif\n";
    exit(1);
  }

  my $file = shift;
  my $prefix = $file; $prefix =~ s/\/.*?$//g; $prefix =~ s/\.\w+$//g;
  $prefix =~ s/^.*?_//g;
  $prefix =~ s/_/\\_/g;

  my $text = qx(gdalinfo -hist $file);
  my @bands = ("North error: $prefix", "East error: $prefix", "Down error: $prefix");

  my $code = "figure(1); clf; hold on;\n";
  for (my $band = 1; $band <= 3 ; $band++){
    $code .= plot_band($band, $bands[$band-1], $text);
  }

  my $mFile = $file;
  $mFile =~ s/\.\w+$//g;
  $mFile =~ s/\./_/g;
  $mFile = $mFile . ".m";

  $prefix = $mFile;
  $prefix =~ s/\.m//g;
  print "---$prefix\n";
  $code .= "print ('-dpng', '$prefix.png', '-r800');\n";
  $code .= "disp('Saving $prefix.png');\n";
  #$code .= "exit\n";


  print "Writing: $mFile\n";
  open(FILE, ">$mFile");
  print FILE $code;
  close(FILE);
  print qx(module load matlab; matlab -nodisplay < $mFile) . "\n";
}


sub plot_band{

  my $band  = shift;
  my $title = shift;
  my $text  = shift;

  my ($min, $max, $vals);
  if ($text =~ /Band\s+$band.*?Minimum=(.*?),\s+Maximum=(.*?),.*?buckets.*?\n\s*(.*?)\s*\n/s){
    $min = $1;
    $max = $2;
    $vals = $3;
    print "--$min $max $vals\n";
  }else{
    return "";
  }

  my @vals_y = split(/\s+/, $vals);
  my $size = scalar(@vals_y);

  my $X = "X = linspace($min, $max, $size)";
  my $Y = "Y = [$vals]";

  $text = "subplot(3, 2, 2*$band-1);\n$X;\n$Y;\nplot(X, Y); title('         $title'); axis on;\n\n";
  #if (-e "find_max.m"){ $text .= "hold on; find_max(X, Y);\n\n"; }

  $text .= "I=find(X < 0.1*$max);\n";
  $text .= "subplot(3, 2, 2*$band);\n\nplot(X(I), Y(I));title('            Detail of left plot')\n\n";
  #if (-e "find_max.m"){ $text .= "hold on; find_max(X(I), Y(I));\n\n"; }

  return $text;
}
