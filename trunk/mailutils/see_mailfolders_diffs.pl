#!/usr/bin/perl
use strict;		      # 'strict' insists that all variables be declared
use diagnostics;	      # 'diagnostics' expands the cryptic warnings
use Encode;
use File::stat;
use lib $ENV{HOME} . '/bin/mailutils';
require 'mail_utils.pl';
undef $/;

# Get the ids from a mail directory, put all of them in a hash.
# Then use those ids to find which mails in one directory
# are missing from the second one, and the other way around.

# Do not read mailbox files bigger than this (it is assumed such files
# have been split into smaller mailboxes).
# This number must be bigger than the number used to split mailboxes.
my $big_file_size = 40e6; # 40MB

MAIN: {

  # Get ids of the files on gmail, write them to disk
  my $gmail_dir      = "gmail";
  my $gmail_ids_file = $gmail_dir . "_ids.txt";
  &write_ids_to_disk($gmail_dir, $gmail_ids_file);
  
  # Get ids of the files in old_mail (local mail), write them to disk
  my $old_mail_dir      = "old_mail";
  my $old_mail_ids_file = $old_mail_dir . "_ids.txt";
  &write_ids_to_disk($old_mail_dir, $old_mail_ids_file);

  # Pause before reading the ids from disk
  print "Pausing ...\n";
  sleep(5);

  # Read the ids we just wrote to disk
  my %gmail_ids    = &get_ids_from_disk($gmail_ids_file);
  my %old_mail_ids = &get_ids_from_disk($old_mail_ids_file);

  # Mailboxes (we skip the ones of size $big_file_size) 
  my @gmail_files    = &get_files_in_maildir($gmail_dir);
  my @old_mail_files = &get_files_in_maildir($old_mail_dir);
  
  my $missing_gmail    = "Missing_" . $gmail_dir;
  my $missing_old_mail = "Missing_" . $old_mail_dir;
  
  my ($id, $folder);

  # Empty before appending to folder
  print "Will write to $missing_old_mail\n";
  open(FILE, ">$missing_old_mail"); print FILE ""; close(FILE);
  
  # See mails that are on gmail, but which are not on old mail
  foreach $folder (@gmail_files){
    print "Doing $folder\n";
    &write_missing($folder, \%old_mail_ids, $missing_old_mail);
  }

  # Empty before appending to folder
  open(FILE, ">$missing_gmail"); print FILE ""; close(FILE);
  print "Will write to $missing_gmail\n";
  
  # The other way around, files which are in old mail, but not on gmail
  foreach $folder (@old_mail_files){
    print "Doing $folder\n";
    &write_missing($folder, \%gmail_ids, $missing_gmail);
  }
  
}

sub get_ids_from_disk {

  my $file = shift;

  open(FILE, "<$file");
  my @ids = split("\n", <FILE>);
  close(FILE);
  
  my %hash;
  my $id;
  foreach $id (@ids){
     next if ($id =~ /^\s*$/);
    $hash{$id} = 1;
  }

  return %hash;
}

sub get_files_in_maildir {

  my $mail_dir = shift;

  print "\nGetting files in $mail_dir\n\n";
  
  my $file_line = `find $mail_dir`;
  my @files     = split("\n", $file_line);
  @files        = sort {$a cmp $b} @files;
  
  my @files_out = ();
  
  my $file;
  foreach $file (@files){

    if ( -d $file){
      #print "$file is a directory, skipping\n";
      next;
    }

    if ($file =~ /\.(msf|dat)$/){
      #print "Skipping $file\n";
      next;
    }
    
    my $filesize = stat($file)->size;
    #print "$filesize Size of $file is $filesize\n";
    
    if ($filesize >= $big_file_size){ 
      print "-----Skip $file as too big\n";
      next;
    }

    push(@files_out, $file);

  }

  return @files_out;
}

sub write_ids_to_disk {

  my $mailbox = shift;
  my $id_file = shift;
  
  # make the id file blank before we start
  open(FILE, ">$id_file");
  print FILE "";
  close(FILE);
  
  my @files = &get_files_in_maildir($mailbox);
  my $file;

  foreach $file (@files){
    
    print "Will do $file\n";

    my @ids;
    @ids = &extract_ids($file);

    open(FILE, ">>$id_file");
    print "Appending ids to $id_file\n";
    foreach my $id (@ids){
      print FILE "$id\n";
    }
    close(FILE);

    print "\n";
  }

}


sub write_missing {

  # Append the emails in $folder whose id is not in the $existing hash,
  # to a new folder called $missing_folder.
  my $folder         = shift;
  my $existing_ids   = shift;
  my $missing_folder = shift;
  
  my @mails;
  &read_mailbox($folder, \@mails);

  my $message;
  foreach $message (@mails){

    if ($message !~ /^From .*?\d:\d\d:\d\d/){
      print "Error, invalid message!\n$message\n";
      exit(0);
    }
    
    my ($header, $body) = &extract_header_body ($message);

    $header = &add_message_id_if_needed($header);
       
    my $id = extract_message_id($header);

    next if ( exists $existing_ids->{$id} );  

    $message = &combine_header_body($header, $body);

    open(FILE, ">>$missing_folder");
    print FILE $message . "\n"; # Newline just in case
    close(FILE);
    
  }

}
