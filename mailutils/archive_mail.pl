#!/usr/bin/perl
use strict;		      # 'strict' insists that all variables be declared
use diagnostics;	      # 'diagnostics' expands the cryptic warnings
use Encode;
require 'mail_utils.pl';

undef $/; # undefines the separator. Can read one whole file in one scalar.
$| = 1; # flush the buffer each line

# Split mailbox in individual messages, and forward to gmail using procmail. 
# Sample procmail file (sans "# ") is below.
# :0
# ! me@mydomain.com
# 

MAIN: {

  my ($id, $message_file, $pause, $message);
  my ($output, $success, $folder, @folders, $done_file, %Done_hash, $line, $mailbox);
  my (@mails, $debugMode);

  if ( scalar(@ARGV) < 2 ){
    print "Usage: $0 MailFolder debugMode\n";
    exit(0);
  }
  $folder    = $ARGV[0];
  $debugMode = $ARGV[1]; # 1 for debugging, 0 for actual work
  
  $message_file = 'Message_file';   # save here a message before calling the fwding prog
  $done_file    = 'Sent_to_gmail.txt'; # Store message ids of mail forwarded earlier
  $pause = 30; # pause this many seconds between sending messages to gmail

  &read_done_ids ($done_file, \%Done_hash);

  &read_mailbox($folder, \@mails);
  print "Number of messages is " . scalar (@mails) . "\n";
  
  foreach $message (@mails) {          

    $message = &add_message_id_if_needed($message);

    if ($message !~ /Message-ID:\s+\<(.*?)\>/i){ 
      print "Missing message id\n" . $message . "\n";
      exit(0);
    }  
    $id = $1;
    $id =~ s/\s+/_/g; # make sure there are no spaces in the ID
    next if ( ( exists $Done_hash{$id} ) && ( $Done_hash{$id} == 1 ) );
    
    print "Will be doing message $id\n";
    
    $message = &process_message($message);
    
    if ( !$debugMode ){
      # Forward with procmail
      &send_message_to_gmail_via_procmail($message);
      $success = 1;
      $Done_hash{$id} = $success;
      open (DONE_FH, ">>$done_file") || die "Can't append to $done_file!\n";
      print DONE_FH "$id $success\n";
      close (DONE_FH);
      
      print "Sleep for $pause seconds\n\n\n"; 
      sleep $pause;
    }

  } # end loop

  # If in debug mode just write the processed messages to disk
  if ($debugMode){ # debug

    foreach $message (@mails) {
      $message =~ s/[\n]*$//g;
    }

    # this will show what processing the messages underwent before being sent
     my $outfile = "ProcessedMessages";
     print "Writing processed messages to $outfile\n";
     open(FILE, ">$outfile");
     print FILE join ("\n\n", @mails);
     print FILE "\n";
     close(FILE);
  }

}

sub send_message_to_gmail_via_procmail {

  my ($message, $tmp_file, $procmailrc_file);

  $message = shift;

  $tmp_file = "Tmp_file.txt";
  open(FILE, ">$tmp_file");
  print FILE "$message\n";
  close(FILE);

  $procmailrc_file = $ENV{HOME} . '/gmail_procmailrc';
  if (!-e $procmailrc_file){
    print "Error! $procmailrc_file does not exist. Exiting.\n";
    exit(0);
  }

  # To do: open $procmailrc_file and see if there is a valid address there.
  # Otherwise procmail might forward to the original recepient maybe, not good.
  
  print "cat $tmp_file \| /usr/bin/procmail $procmailrc_file\n";
  print `cat $tmp_file \| /usr/bin/procmail $procmailrc_file` . "\n";

}
