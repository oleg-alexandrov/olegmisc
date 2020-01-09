sub find_bdbox{

  my $poly = shift;

  my $count = 0;

  my ($xll, $yll, $xur, $yur, $point);
  
  foreach $point (split("\n", $poly)){

    next unless ($point =~ /^\s*([e\-\+\.\d]+)\s+([e\-\+\.\d]+)/);

    my $x = $1; my $y = $2;
    $count++;

    if ($count == 1){
      
      $xll = $x; $xur = $x;
      $yll = $y; $yur = $y;
      
    }else{

      if ($x < $xll) { $xll = $x; }; if ($x > $xur) { $xur = $x; };
      if ($y < $yll) { $yll = $y; }; if ($y > $yur) { $yur = $y; };
      
    }
  }

  if ($count == 0){
    print "Error: empty polygon\n";
    exit(0);
  }

  return ($xll, $yll, $xur, $yur);
}

1;
