#!/usr/bin/perl
use strict;		      # 'strict' insists that all variables be declared
use diagnostics;	      # 'diagnostics' expands the cryptic warnings

undef $/; # undefines the separator. Can read one whole file in one scalar.
$| = 1; # flush the buffer each line


sub read_done_ids {
  
  my ($done_file, $Done_hash, $line, $id, $success, $text);
  
  $done_file = shift; $Done_hash = shift;

  %$Done_hash = ();

  if (!-e $done_file){
    print "Missing $done_file\n";
    return;
  }
  
  open(FILE, "<$done_file") || die "Can't open file $done_file";
  $text = <FILE>; 
  close (FILE);
 
  foreach $line (split ("\n", $text)) {
    next if ($line =~ /^\s*$/);
    
    if ($line =~ /^(.*?)\s+(\d+)\s*$/){
      $id = $1; $success = $2;
      $Done_hash->{$id} = $success;
    }else{
      print "\nError! Malformatted file $done_file!\n";
      print "Bad line: $line\n";
      exit(0);
    }
  }
}


sub read_mailbox {

  # Split mailbox into messages. The big idea is that a message
  # starts with "\nFrom " line with a date on that line. 

  my ($folder, $mails, $text);
  
  $folder = shift; 
  $mails  = shift;
  
  print "Reading $folder\n";

  open (FILE, "<$folder");
  $text = <FILE>;
  close (FILE);

  # The case of empty folders: return zero messages
  if ($text =~ /^\s*$/){
    @$mails = ();
    return;
  }

  # Put a tag between messages. Care must be taken due to
  # the fact that there may be carriage returns (\r) in places.
  my $tag = ' sld839X929Ax xAio97UaIaE '; # something unlikely
  $text =~ s/(\r?\n\r?)(From .*?\d:\d\d:\d\d)/$1$tag$2/ig;

  # pass the mailbox by reference
  @$mails = split ($tag, $text);

  # Some processing
  my $mail;
  foreach $mail (@$mails){

    next if ($mail =~ /^\s*$/);

    $mail =~ s/^\s*//g;
    
    if ($mail !~ /^From .*?\d:\d\d:\d\d/){
      print "Invalid mail\n$mail\n";
      exit(0);
    }

    # Wipe the old message id and create a new one.
    my ($header, $body) = &extract_header_body ($mail);
    #$header =~ s/\nMessage-ID:\s+.*?($|\n)/$1/ig;
    $header = &add_message_id_if_needed($header);
    $mail   = &combine_header_body($header, $body);

  }

}

sub write_mailbox{

  my ($folder, $mails, $message);
  
  $folder = shift;
  $mails  = shift;

  if (-e $folder){
    print "\nError! $folder exists!\n";
    exit(0);
  }

  print "Writing to $folder\n";
  open(FILE, ">$folder");
  foreach $message (@$mails){

    # make sure there is a newline bewteen messages
    $message =~ s/^\s*//g;
    $message =~ s/\s*$//g;
    $message =~ s/\s*$/\n\n/g;
    
    print FILE $message;
  }
  close(FILE);
  
}

sub extract_header_body {

  my $message = shift;
  
  $message =~ s/^\s*//g; # rm whitespace at the beginning

  if ($message !~ /^From .*?\d:\d\d:\d\d/){
    print "Invalid message.\n" . $message . "\n";
    exit(0);
  }

  my ($header, $body);
  
  # Check if the message has a header
  if ($message =~ /^(.*?)(\r?\n\r?\n\r?.*?)$/s){
    $header = $1;
    $body = $2;
  }else{
    # message with no body, just a header
    $header = $message;
    $body = "";
  }

  return ($header, $body);
  
}

sub combine_header_body{

  my $header = shift;
  my $body   = shift;

  # make sure there is exactly one newline between header and body
  $header =~ s/\s*$/\n\n/g;
  $body =~ s/^\s*//g;
  
  # put an empty line after body
  $body =~ s/\s*$/\n\n/g;
  
  my $message = $header . $body;

  return $message;
}

sub process_message{
  
  # Split message into header and body. Flag if there are problems.
  # Use the fact that the header is separated from the message body by a blank line.

  my ($message, $header, $body, $error);

  $message = shift;

  ($header, $body) = &extract_header_body ($message);
  
  # Check if the message id is missing
  if ($header !~ /\nMessage-ID:\s+\<.*?\>/i){
    print "\nError: no message id!\n";
    print "Message is:\n$message\n";
    exit(0);
  }
  
#   # check if there is more than one message id in the header
#   my ($id1, $id2);
#   if ( $header =~ /\nMessage-ID:\s+\<([^\n]*?)\>.*?\nMessage-ID:\s+\<([^\n]*?)\>/si ) {

#     $id1 = $1; $id2 = $2;
#     if ($id1 ne $id2){
#       print "\nError, more than one message id!\n";
#       print "$message\n";
#       exit(0);
#     }
#   }

  $header =~ s/\nSubject:/\nSubject: \[cdn\]/i;
  
  ($header, $body) = &fix_content_type($header, $body);

  $message = &combine_header_body($header, $body);

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

sub extract_ids {
  
  my $folder = shift;
  my @ids = ();
  
  my @mails;
  &read_mailbox($folder, \@mails);

  my $message;
  foreach $message (@mails){

    if ($message !~ /^From .*?\d:\d\d:\d\d/){
      print "Invalid message\n$message\n";
      exit(0);
    }
    
    my ($header, $body) = &extract_header_body ($message);
    my $id = extract_message_id($header);

    push (@ids, $id);
  }

  return @ids;
}

sub extract_message_id {

  my $header = shift;

  my $message_id;
  
  $header = &add_message_id_if_needed($header);

  if ($header =~ /Message-ID:\s+\<(.*?)\>/i){
    $message_id = $1;
  }else{
    print "No message id in header:\n$header";
  }

  return $message_id;
  
}

sub add_message_id_if_needed {

  # Manufacture an id from the "Date" and "To" fields.
  # Must be deterministic.

  my ($header, $id);

  $header = shift;
  $id     = "";
  
  if ($header !~ /^From .*?\d:\d\d:\d\d/){
    print "\nError! Missing \"From\" line in header!\n$header\n";
    exit(0);
  }

  if ($header =~ /(^|\n)Message-ID:\s+\<.*?\>/i){
    # nothing to do, id exists
    return $header;
  }

  if ($header =~ /(^|\n)Date:(.*?)($|\n)/i){
    $id = $2;
  }else{
    #print "\nError: Cannot match date in: \n$header\n";
    #exit(0);
  }

  my $to = "";
  if ($header =~ /(^|\n)To:(.*?)($|\n)/i){
    # Must have the cleanups below (added the hard way)
    $to = $2;
    $to =~ s/\@.*?$//g; # the part after @ may be different on gmail and local
    $to =~ s/[\"\'\`; ]//g;
    $to =~ s/\<.*?$//g; 
    $to =~ s/\s*,.*?$//g;
    $to =~ s/\s*undisclosed.*?$//ig;
  }else{
    #print "\nError! Missing \"To\" line in header!\n";
    #print "$header\n";
    #exit(0);
  }
  $id = $id . $to;

  $id =~ s/\s//g;
  $id =~ s/[^a-zA-Z0-9]//g;

  $header =~ s/^(From.*?\n)(.*?)$/$1Message-ID: \<$id\>\n$2/sg;
  
  #print "Adding message id to:\n$header\n\n";

  return $header;
}

1;
