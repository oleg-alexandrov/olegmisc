#!/usr/bin/perl
use strict;		      # 'strict' insists that all variables be declared
use diagnostics;	      # 'diagnostics' expands the cryptic warnings
use Encode;
require 'mail_utils.pl';

undef $/; # undefines the separator. Can read one whole file in one scalar.
$| = 1; # flush the buffer each line

# Split mailbox in individual messages, and forward to gmail using procmail. 

MAIN: {

  my ($id, $message_file, $pause, $message);
  my ($output, $success, $folder, @folders, $done_file, %Done_hash, $line, $mailbox);
  my (@mails);

  if ( $#ARGV < 0 ){
    print "Usage: $0 MailFolder\n";
    exit(0);
  }
  $folder = $ARGV[0];

  my $do_debug = 0; # 1 for debugging, 0 for actual work
  
  $message_file = 'Message_file';  # save here a message before calling the forwarding program
  $done_file = 'Sent_to_gmail.txt'; # Store message ids of mail forwarded earlier
  $pause = 30; # pause this many seconds between sending messages to gmail

  &read_done_ids ($done_file, \%Done_hash);

  @mails = &read_mailbox($folder);
  print "Number of messages is " . scalar (@mails) . "\n";
  
  foreach $message (@mails) {          
    next unless ($message =~ /Message-ID:\s+\<(.*?)\>/i); # checked for message id earlier
    
    $id = $1;
    $id =~ s/\s+/_/g; # make sure there are no spaces in the ID
    next if ( ( exists $Done_hash{$id} ) && ( $Done_hash{$id} == 1 ) );
    
    print "Will be doing message $id\n";
    
    $message = &process_message($message);
    
    if ( !$do_debug ){
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
    
  }

  if ($do_debug ){ # debug
    
    # this will show what processing the messages underwent before being sent
    my $outfile = "ProcessedMessages";
    print "Writing processed messages to $outfile\n";
    open(FILE, ">$outfile");
    print FILE join ("\n", @mails); # I don't know why "\n" is necessary here
    print FILE "\n";
    close(FILE);
  }
}


sub send_message_to_gmail_via_procmail {

  my ($message, $tmp_file, $procmailrc_file);

  $message = shift;

  $tmp_file = "Tmp_file.txt";
  open(FILE, ">$tmp_file");
  print FILE "$message";
  close(FILE);

  $procmailrc_file = $ENV{HOME} . '/gmail_procmailrc';
  if (!-e $procmailrc_file){
    print "Error! $procmailrc_file does not exist!!! Exiting.\n";
    exit(0);
  }
  
  print "cat $tmp_file \| /usr/bin/procmail $procmailrc_file\n";
  print `cat $tmp_file \| /usr/bin/procmail $procmailrc_file` . "\n";

}
