#!/usr/bin/perl
use strict;		      # 'strict' insists that all variables be declared
use diagnostics;	      # 'diagnostics' expands the cryptic warnings
undef $/; # undefines the separator. Can read one whole file in one scalar.

# Convert a C++ function to a Mex-like interface. Format nicely.

my ($line, @args, $count, $maxlen, $lhs, $rhs, $maxlen2);

$line=<>;
$line =~ s/\s*\n\s*/ /g;

return 0 unless ($line =~ /\((.*?)\)/);
@args=split (",", $1);

# align nicely
$count=0;
$maxlen=0;
foreach (@args){
  s/^\s*//g;
  s/\s*$//g; 

  if (/^int\s+(\w+)$/){
    $lhs="  int $1"; $rhs="(int) *";  $_ = "$lhs = $rhs" . "mxGetPr(prhs[$count]);";
    $maxlen = length($lhs)  if ( length($lhs) > $maxlen );
    
  }elsif (/^double\s+(\w+)$/){
    $lhs="  double $1"; $rhs = "*";  $_ = "$lhs = $rhs" . "mxGetPr(prhs[$count]);";
    $maxlen = length($lhs)  if ( length($lhs) > $maxlen );

  }elsif (/^\w+\s*\*\s*(\w+)$/){
    $lhs="  double* $1"; $rhs = "  ";  $_ = "$lhs = $rhs" . "mxGetPr(prhs[$count]);";
    $maxlen = length($lhs)  if ( length($lhs) > $maxlen );

  }else{
    $lhs="  ?"; $rhs = "  ";  $_ = "$lhs = $rhs" . "mxGetPr(prhs[$count]);";
    $maxlen = length($lhs)  if ( length($lhs) > $maxlen );
  }
  $count++;
}


$maxlen2=0;
foreach (@args){
  next unless (/^(.*?)( =.*?)$/);
  $_ = "$1" . " " x ($maxlen - length ($1)) . "$2";

  next unless (/^(.*?)(mxGetPr.*?)$/);
  $maxlen2 = length($1)  if ( length($1) > $maxlen2 );
}

foreach (@args){
  next unless (/^(.*?)(.)(mxGetPr.*?)$/);
    $_ = "$1" . " " x ($maxlen2 - length ($1)) . "$2" . "$3";
}

print 
   "#include \"mex.h\"\n\n"
   . "// $line\n"
   . "void mexFunction( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[] ){\n\n";

foreach (@args){
  print "$_\n";
}

