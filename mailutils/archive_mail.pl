#!/usr/bin/perl
use strict;		      # 'strict' insists that all variables be declared
use diagnostics;	      # 'diagnostics' expands the cryptic warnings
use Encode;
require 'mail_utils.pl';

undef $/; # undefines the separator. Can read one whole file in one scalar.
$| = 1; # flush the buffer each line

# Split mailbox in individual messages, and forward to gmail using procmail. 

MAIN: {

  print "Warning: this code was not tested after recent changes!  Use with care!\n";
  exit(0); 
  
  my ($mail_dir, $id, $message_file, $pause, $message);
  my ($output, $success, $folder, @folders, $done_file, %Done_hash, $line, $mailbox);
  my (@mails);

  # define some settings we will need later
  $mail_dir = $ENV{HOME} . '/mail';
  $message_file = 'Message_file';  # save here a message before calling the forwarding program
  $done_file = 'Sent_to_gmail.txt'; # Store message ids of mail forwarded earlier
  $pause = 30; # pause this many seconds between sending messages to gmail

  &read_done_ids ($done_file, \%Done_hash);

  # start the fun, go through the folders, forward to gmail stuff which has not been forwarded yet
  @folders = <$mail_dir/*>;
  foreach $folder (@folders){

    @mails = &parse_mailbox($folder);
    foreach $message (@mails) {          
      next unless ($message =~ /Message-ID:\s+\<(.*?)\>/i); # checked for message id earlier
        
      $id = $1; $id =~ s/\s+/_/g; # this last subst should not be necessary, is here just in case
      next if ( ( exists $Done_hash{$id} ) && ( $Done_hash{$id} == 1 ) );
      print "Will be doing message $id\n";
      
     my $action = 1;   # use $action == 0 for debugging
     if ($action){
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
