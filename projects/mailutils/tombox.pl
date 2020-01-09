#!/usr/bin/perl
use strict;        # insist that all variables be declared
use diagnostics;   # expand the cryptic warnings

MAIN:{

  # Convert from some format exported from Outlook to mbox format.
  
  if (scalar(@ARGV) < 1){
    print "Usage: $0 <fileName>\n";
    exit(0);
  }

  open(FILE, "<$ARGV[0]"); my $text = join("", <FILE>);  close(FILE);

  $text =~ s/\r//g; # strip Windows carriage return
  $text =~ s/^.*?\n//g; # strip header

  my $sep = ' xxA5oaD dd 5o5o ';
  $text =~ s/(=\s*mailbox\s+servers.*?\n)/$1$sep/ig;
  my @mails = split($sep, $text);

  my $out = "outFile.txt";
  print "Writing to $out\n";
  open(FILE, ">$out");
  
  my $count = 0;
  foreach my $mail (@mails){

    $count++;
    my ($subject, $body, $footer) = ($mail, $mail, $mail);
    if ($mail =~ /^(.*?)\"(.*)\"(.*?=\s*mailbox\s+servers.*?)$/is){
      $subject = $1;
      $body    = $2;
      $footer  = $3;
    }else{
      $subject =~ s/\/.*?$//sg;
    }
    
    my $from = $footer;
    $from =~ s/\/.*?$//sg;
    $from =~ s/\s*SMTP.*?$//g;
    $from =~ s/^\s*//g;
    $from =~ s/\s*$//g;

    $subject =~ s/^\s*//g;
    $subject =~ s/\s*$//g;

    my $time = $count;
    my $sec  = $time % 60; $time = ($time - $sec)/60;
    my $min  = $time % 60; $time = ($time - $min)/60;
    my $hour = $time;
    if ($sec < 10)  { $sec = "0" + $sec; }
    if ($min < 10)  { $min = "0" + $min; }
    if ($hour < 10) { $hour = "0" + $hour; }


    my $year = $count;
    $year += 1000;
    
    #my $date = "Mon Jan 1 $hour:$min:$sec $year";
    my $date = "Mon Jan 1 22:44:44 2011";
    my $id = $mail; $id =~ s/[^\w]//g;
    $id = substr $id, 0, 50;
    $id .= $count;
    
    #print "$id\n";

    $date    =~ s/\n/ /g;
    $id      =~ s/\n/ /g;
    $from    =~ s/\n/ /g;
    $subject =~ s/\n/ /g;
    
    my $msg = "From - $date\n"  . 
       "Message-ID: <$id>\n"    .
          "From: $from\n"       .
             "Date: $date\n"    .
                "Subject: $subject\n" .
                   "\n"               .
                      $body           .
                         "\n\n";

    print FILE $msg;

    #print "$subject\n";
    #print "-----\n$msg\n----\n\n";
    #print "--$from--\n\n\n";
    #print "---$subject----$from---\n";
    #print "---$subject---body:$body----$footer\n\n\n";
    #print "$footer\n";
  }
  
  close(FILE);

}
