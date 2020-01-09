#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
use Date::Parse;
use POSIX qw{strftime};
  MAIN:{

  # Usage: diff_time.pl output.txt or cat output.txt | diff_time.pl
  my @lines;
  if (scalar(@ARGV) >= 1){
    open(FILE, "<$ARGV[0]");
    @lines = <FILE>;
    close(FILE);
  }else{
    @lines = <STDIN>;
  }
  
  my $count = -1;
  my @dates = ();
  foreach my $line (@lines){

    next unless ($line =~ /^.*?\s*\[\s*\d\d\d\d-/);
    $line =~ s/^.*?\[/\[/g;

    $count++;

    print $line;
    my $date = get_date_as_seconds($line);

    push (@dates, $date);
    if ($count%2){
      my $diff = $dates[$count] - $dates[$count-1]; # difference in times in seconds
      #print $dates[$count-1] . " " . $dates[$count] . " " . ($dates[$count] - $dates[$count-1]) . "\n";
      print_diff("Diff is", $diff);
    }
  }

  my $len = scalar(@dates);
  my $diff = $dates[$len - 1] - $dates[0]; # difference in times in seconds
  print_diff("Total time is", $diff);
}

sub print_diff{
  my $text = shift;
  my $diff = shift;
  print "$text " . print_hours($diff) . " hours (" . print_mins($diff) . " minutes) ($diff seconds)\n\n";
}

sub get_date_as_seconds{

  my $line = shift;

  my $date = $line;
  if ($date =~ /^.*?\[\s*(.*?)\s*\]/s) {
    $date = $1;
  } else {
    print "ERROR: Could not match date in: $line\n";
  }

  $date =~ s/-/ /g;
  if ($date =~ /^(.*?)\s+(.*?)\s+(.*?)\s+(.*?)$/) {
    $date = "$3 $2 $1 $4";
  }

  $date = str2time($date);
  if ($date !~ /\d/) {
    print "ERROR: Could not parse date from: $line\n";
  }

  return $date;
}

sub print_hours{
  my $val = shift;
  return  int(100*$val/3600 + 0.5)/100;
}

sub print_mins{
  my $val = shift;
  return  int(100*$val/60 + 0.5)/100;
}
