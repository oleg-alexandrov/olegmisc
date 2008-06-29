#!/usr/bin/perl
use strict;		      # 'strict' insists that all variables be declared
use diagnostics;	      # 'diagnostics' expands the cryptic warnings

undef $/; # undefines the separator. Can read one whole file in one scalar.
$| = 1; # flush the buffer each line


sub read_done_ids {
  
  my ($done_file, $Done_hash, $line, $id, $success, $text);
  
  $done_file = shift; $Done_hash = shift;
 
  open(FILE, "<$done_file") || die "Can't open file $done_file";
  $text = <FILE>; 
  close (FILE);
 
  foreach $line (split ("\n", $text)) {
    next if ($line =~ /^\s*$/);
    
    if ($line =~ /^(.*?)\s+(\d+)\s*$/){
      $id = $1; $success = $2;
      $Done_hash->{$id} = $success;
    }else{
      print "Error! Malformatted file $done_file!\n";
      print "Bad line: $line\n";
      exit(0);
    }
  }
}


sub parse_mailbox {

  # Split mailbox into messages. The big idea is that messages
  # are separated by a blank line (\n\n), and the first line
  # is a "From " line with a date. Also the header is separated
  # from the message body by a blank line too.
  
  my ($folder, $text, @mails, $mail, $count, $header);
  
  $folder = shift; 
  print "Doing $folder\n";

  open (FILE, "<$folder");
  $text = <FILE>;
  close (FILE);

  # Below is the trickiest part in the entire script,
  # splitting an email into messsages
  # Note that this removes whitespace between messages.
  @mails = split ("(?=\n\nFrom .*?\\d:\\d\\d:\\d\\d)", $text);

  # Lots of ugly heuristic below. Getting obsessed about the above
  # line doing a good job at splitting messages
  
  $count = 0;
  foreach $mail (@mails){
    $mail =~ s/^\s*//g; # rm whitespace at the beginning

    $count++;
    #print "$count$mail\n\n*********************************\n\n";
    
    # Check if the message has a header
    if ($mail =~ /^(.*?)(\n\n|$)/s){
      $header = $1;
    }else{
      print "Can't locate header!\n";
      print "$mail\n";
      exit(0);
    }

    # Check if the message id is missing
    if ($header !~ /\nMessage-ID:\s+\<.*?\>/i){
      print "Error, no message id!\n";
      print "Message is: $mail\n";
      exit(0);
    }

    # check if there is more than one message id in the header    
    if ( $header =~ /\nMessage-ID:\s+\<[^\n]*?\>.*?\nMessage-ID:\s+\<[^\n]*?\>/si ) {
      print "Error, more than one message id!\n";
      print "$mail\n";
      exit(0);
    }
 
    #    print "$mail\n";
    #    print "z" x 1000 . "\n"; 
  } 

  return @mails;
}

sub write_mailbox{   # Checked!

  my ($folder, $mails, $message);
  
  $folder = shift;
  $mails  = shift;

  if (-e $folder){
    print "Error! $folder exists!\n";
    exit(0);
  }

  print "Writing to $folder\n";
  open(FILE, ">$folder");
  foreach $message (@$mails){
    print FILE $message;
  }
  close(FILE);
  
}

sub split_in_header_body{ # checked!
  
  # Split message into header and body. Flag if there are problems.
  # Use the fact that the header is separated from the message body by a blank line.

  # To do: Add more checks for problems below.
  
  my ($message, $header, $body, $error);

  $message = shift;

  if ($message =~ /^(.*?\n\n)(.*?\s*)$/s){
    $header = $1;
    $body = $2;
  }else{
    #empty message body
    $header = $message;
    $body = "";
  }
  
  $error = 0;

  # The "From " line is the most important. 
  if ($header !~ /^From .*?\d:\d\d:\d\d/){
    print "Error! Malformed From line!\n";
    $error = 1;
  }
  
  # Check if the message id is missing. Some of my utils rely on it.
  if ($header !~ /\nMessage-ID:\s+\<.*?\>/i){
    print "Error, no message id!\n";
    $error = 1;
  }

  # check if there is more than one message id in the header    
  if ( $header =~ /\nMessage-ID:\s+\<[^\n]*?\>.*?\nMessage-ID:\s+\<[^\n]*?\>/si ) {
    print "Error, more than one message id!\n";
    $error = 1;
  }

  return ($header, $body, $error);
}

sub merge_header_body { # not checked!

  my ($header, $body);

  $header = shift; $body = shift;
  return $header . $body;
}

1;
