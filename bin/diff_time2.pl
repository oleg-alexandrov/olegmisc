#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings
use Date::Parse;
use POSIX qw{strftime};
  MAIN:{

  # Usage: cat output.txt | diff_time2.pl

  my @lines = <STDIN>;
  my $count = -1;
  my (@tlines, @tags, @dates);
  foreach my $line (@lines){
    next unless ($line =~ /^.*?\s+\d\d\d\d-/);
    $line =~ s/\s*$//g;
    #print $line . "\n";
    my ($tag, $date) = get_date_as_seconds($line);
    push(@tlines, $line);
    push (@tags, $tag);
    push (@dates, $date);
  }

  for (my $i = 0; $i < scalar(@tags); $i++){
    my $diff = 0;
    if ($i > 0){ $diff = $dates[$i] - $dates[$i-1]; }
    print $tlines[$i] . " -- " . $diff . "\n";
  }

}

sub get_date_as_seconds{

  my $line = shift;
  my ($date, $tag);

  if ($line =~ /^(.*?\s+)(\d\d\d\d-.*?)\s*$/){
    $tag = $1;
    $date = $2;
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

  return ($tag, $date);
}

sub print_diff{
  my $text = shift;
  my $diff = shift;
  print "$text " . print_hours($diff) . " hours (" . print_mins($diff) . " minutes) ($diff seconds)\n\n";
}

sub print_hours{
  my $val = shift;
  return  int(100*$val/3600 + 0.5)/100;
}

sub print_mins{
  my $val = shift;
  return  int(100*$val/60 + 0.5)/100;
}
