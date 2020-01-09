#!/usr/bin/perl
use strict;	   # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
undef $/;          # read one whole file in one scalar

sub read_xg{

  my $file = shift;
  open(FILE, "<$file");
  my $text = <FILE>;
  close(FILE);
  
  my @polys;
  my @polyStr = split("NEXT", $text);
  foreach my $line (@polyStr){

    my @poly = ();
    my @verts = split("\n", $line);
    my $firstVert = 0;

    foreach my $vertStr (@verts){

      next unless ($vertStr =~ /^\s*([e\+\-\.\d]+)\s+([e\+\-\.\d]+)/);
      my @vert = ($1, $2);
      if ($firstVert == 0){
        $firstVert = \@vert;
      }
      push(@poly, \@vert);
      
    }

    # if the first vertex is repeated at the end, drop it
    my $numVerts = scalar(@poly);
    if ($numVerts >= 2){
      my $lastVert = $poly[$numVerts-1];
      if ($firstVert->[0] == $lastVert->[0] && $firstVert->[1] == $lastVert->[1]){
        pop(@poly);
      }
    }
    
    push(@polys, \@poly) unless ( scalar (@poly) == 0 );
  }
  
  return @polys;
}

sub save_xg{

  my $polys = shift;
  my $file  = shift || "";
  my $color = shift || "red";

  my $H;
  if ($file eq ""){
    $H = *STDOUT;
  }else{
    open(FILE, ">$file");
    $H = *FILE;
  }

  foreach my $poly (@$polys){
    
    foreach my $vert (@$poly){
      print $H $vert->[0] . " " . $vert->[1] . "\n";
    }
    
    # Make the polygon closed
    if ( scalar(@$poly) >= 1 ){
      my $vert = $poly->[0];
      print $H $vert->[0] . " " . $vert->[1] . "\n";
    }
    
    print $H "NEXT\n";
  }

  if ($file ne ""){
    close($H);
  }
  
}

sub print_xg{

  my $polys = shift;
  my $file  = "";
  my $color = shift || "red";

  save_xg($polys, $file, $color);
}

1;
