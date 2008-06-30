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


sub read_mailbox {

  # Split mailbox into messages. The big idea is that a message
  # starts with "\nFrom " line with a date on that line. 
  
  my ($folder, $text, @mails, $mail, $count, $header);
  
  $folder = shift; 
  print "Doing $folder\n";

  open (FILE, "<$folder");
  $text = <FILE>;
  close (FILE);

  # If the line below is uncommented, it is hard to see the result of processing
  # the mails before forwarding to gmail.
  # $text =~ s/\r\n/\n/g; # strip windows newline
  
#   if ($text =~ /[^\n]\n(From .*?\d:\d\d:\d\d.*?)\n/i){
#     print "Error: looks like malformatted mailbox. See the text:\n$1\n";
#     exit(0);
#   }
  
  my $tag = ' sld839X929Ax xAio97UaIaE '; # something unlikely
  $text =~ s/(\n)(From .*?\d:\d\d:\d\d)/$1$tag$2/ig;
  @mails = split ($tag, $text);

  return @mails;
}

sub write_mailbox{

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

sub process_message{
  
  # Split message into header and body. Flag if there are problems.
  # Use the fact that the header is separated from the message body by a blank line.

  my ($message, $header, $body, $error);

  $message = shift;
  $message =~ s/^\s*//g; # rm whitespace at the beginning

  # Check if the message has a header
  if ($message =~ /^(.*?)(\n\n.*?)$/s){
    $header = $1;
    $body = $2;
  }else{
    # message with no body, just a header
    $header = $message;
    $body = "";
  }
  
  # Check if the message id is missing
  if ($header !~ /\nMessage-ID:\s+\<.*?\>/i){
    print "Error, no message id!\n";
    print "Message is: $message\n";
    exit(0);
  }
  
#   # check if there is more than one message id in the header
#   my ($id1, $id2);
#   if ( $header =~ /\nMessage-ID:\s+\<([^\n]*?)\>.*?\nMessage-ID:\s+\<([^\n]*?)\>/si ) {

#     $id1 = $1; $id2 = $2;
#     if ($id1 ne $id2){
#       print "Error, more than one message id!\n";
#       print "$message\n";
#       exit(0);
#     }
#   }

  $header =~ s/\nSubject:/\nSubject: \[cdn\]/i;
  
  ($header, $body) = &fix_content_type($header, $body);
     
  $message = $header . $body;

  return $message;
}


sub fix_content_type {

  # Some attachments/message parts are in HTML, but Outlook (or
  # Thuderbird) is stupid enough to claim it is text. Replace
  # text/plain with text/html.

  my ($header, $body) = @_;

  my ($sep, @attachments, $attachment);

  # Sometimes things are wrong in the header.
  if ($body =~ /\<html/i && $header =~ /Content-Type:\s+text\/plain/i){
    $header =~ s/Content-Type:\s+text\/plain/Content-Type: text\/html/ig;  
  }

  # Same thing with attachments. What is below is a silly way to parse
  # attachments, I don't want to bother to learn MIME::PARSE or something
  if ($header =~ /\nContent-Type:.*?\n\s+boundary=\"(.*?)\"/i){
    $sep = $1;
    #print "Separator is $sep\n";
  }else{
    return ($header, $body); 
  }

  # The attachment parser below does not do a perfect job, but is good enough.
  @attachments = split ($sep, $body);

  foreach $attachment (@attachments){

    # It is very dumb to claim html is plain text. Same for ms-tnef, whatever that is.
    if ($attachment =~ /\nContent-Type:\s+(text\/plain|application\/ms-tnef)/i && $attachment =~ /\n\<html/i){
      $attachment =~ s/\nContent-Type:\s+(text\/plain|application\/ms-tnef)/\nContent-Type: text\/html/;
    }
    
  }
  
  $body = join ($sep, @attachments);
  return ($header, $body);

}

sub add_message_id_if_needed {

  # Don't modify the way the ID is manufactured here,
  # as this will yield to messages being duplicated on gmail
  # if having different ids.
  my ($header, $id);
  $header = shift;

  if ($header =~ /Message-ID:\s+\<.*?\>/i){
    # nothing to do, id exists
    return $header;
  }
  
  # Manufacture an id from the "from" and "to" lines.
  # Must be deterministic.
  if ($header =~ /^From (.*?)\n/){
    $id = $1;
  }else{
    print "Error! Missing \"From\" line in header!\n";
    exit(0);
  }

  if ($header =~ /To:(.*?)\n/i){
    $id = $id . $1;
  }else{
    print "Error! Missing \"To\" line in header!\n";
    print "$header\n";
    exit(0);
  }

  $id =~ s/[^a-zA-Z0-9]/-/g;

  $header =~ s/^(.*?\n)(Date:.*?)$/$1Message-ID: \<$id\>\n$2/sg;

  return $header;
}

1;
