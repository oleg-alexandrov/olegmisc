#!/usr/bin/perl -w

sub rotx {
  my $a = shift;
  my $x = shift;
  my $y = shift;
  my $z = shift;
  
  my $c = cos($a);
  my $s = sin($a);

  my $x1 = $x;
  my $y1 = 0 + $c*$y - $s*$z;
  my $z1 = 0 + $s*$y + $c*$z;
  return ($x1, $y1, $z1);
}

sub roty {
  my $a = shift;
  my $x = shift;
  my $y = shift;
  my $z = shift;
  
  my $c = cos($a);
  my $s = sin($a);

  my $x1 =  $c * $x  + 0 * $y + $s * $z;
  my $y1 =  0  * $x  + 1 * $y + 0  * $z;
  my $z1 = -$s * $x  + 0 * $y + $c * $z;
  return ($x1, $y1, $z1);
}

sub rotz {
  my $a = shift;
  my $x = shift;
  my $y = shift;
  my $z = shift;
  
  my $c = cos($a);
  my $s = sin($a);

  my $x1 =  $c * $x  -$s * $y + 0  * $z;
  my $y1 =  $s * $x  +$c * $y + 0  * $z;
  my $z1 =  0  * $x  + 0 * $y + 1  * $z;
  return ($x1, $y1, $z1);
}

sub rot {
  my $a = shift;
  my $b = shift;
  my $c = shift;
  my $x = shift;
  my $y = shift;
  my $z = shift;

  ($x, $y, $z) = rotx($a, $x, $y, $z);
  ($x, $y, $z) = roty($b, $x, $y, $z);
  ($x, $y, $z) = rotz($c, $x, $y, $z);
  
  return ($x, $y, $z);
}


# print three values between -$val/2 and $val/2
my $val = 800000;
#my @a =(7800+rand($val) - $val/2, 10000+rand($val)-$val/2, -8000+rand($val)-$val/2);
my @a =(7800+rand($val) - $val/2, 10000+rand($val)-$val/2, 0);

# Make the radius equal to earth radius
my $earthRad = 6355267;
my $len = sqrt($a[0]*$a[0] + $a[1]*$a[1] + $a[2]*$a[2]);
$len = $earthRad / $len;
#$a[0] *= $len;
#$a[1] *= $len;
#$a[2] *= $len;
$a[0] = -3791951.47505586;
$a[1] = 5100051.2401456;
$a[2] = 0;

my $scale = 0.25;
my $a = 1.67 + rand($scale) - $scale/2.0;
my $b = 0.068 + rand($scale) - $scale/2.0;
my $c = 0.68 + rand($scale) - $scale/2.0;

my ($x, $y, $z);
$x = 1; $y = 0; $z = 0;

my ($x1, $y1, $z1) = rot($a, $b, $c, 1, 0, 0);
my ($x2, $y2, $z2) = rot($a, $b, $c, 0, 1, 0);
my ($x3, $y3, $z3) = rot($a, $b, $c, 0, 0, 1);

print "$x1 $x2 $x3 $a[0]\n";
print "$y1 $y2 $y3 $a[1]\n";
print "$z1 $z2 $z3 $a[2]\n";
print "0 0 0 1\n";

print "# angles are $a $b $c\n";


#print join(" ", @a) . "\n";




