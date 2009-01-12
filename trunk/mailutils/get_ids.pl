#!/usr/bin/perl
use strict;		      # 'strict' insists that all variables be declared
use diagnostics;	      # 'diagnostics' expands the cryptic warnings
use Encode;
use File::stat;

require 'mail_utils.pl';

undef $/; # undefines the separator. Can read one whole file in one scalar.
$| = 1; # flush the buffer each line

# Get the ids from a mailbox, put all of them in a file

MAIN: {

  # Get ids of the files on gmail, write them to disk
  my $gmail_dir          = "gmail";
  my $gmail_ids_file = $gmail_dir . "_ids.txt";
  #&get_ids($gmail_dir, $gmail_ids_file);
  
  # Get ids of the files in old_mail (local mail), write them to disk
  my $old_mail_dir          = "old_mail";
  my $old_mail_ids_file = $old_mail_dir . "_ids.txt";
  #&get_ids($old_mail_dir, $old_mail_ids_file);

  my $id;

  my %gmail_ids    = &get_hash_from_disk($gmail_ids_file);
  my %old_mail_ids = &get_hash_from_disk($old_mail_ids_file);

  my @gmail_files    = &get_files_in_maildir($gmail_dir);
  my @old_mail_files = &get_files_in_maildir($old_mail_dir);

  
  my $missing_gmail    = "Missing_" . $gmail_dir;
  my $missing_old_mail = "Missing_" . $old_mail_dir;
  
  my $folder;

  # See mails that are on gmail, but which are not on old mail
  open(FILE, ">$missing_old_mail"); print FILE ""; close(FILE); # empty b/f app
  foreach $folder (@gmail_files){
    print "Doing $folder\n";
    &write_missing($folder, \%old_mail_ids, $missing_old_mail);
  }

  # The other way around, files which are in old mail, but not on gmail
  open(FILE, ">$missing_gmail"); print FILE ""; close(FILE); # emtpy b/f app
  foreach $folder (@old_mail_files){
    print "Doing $folder\n";
    &write_missing($folder, \%gmail_ids, $missing_gmail);
  }
  
}

sub get_hash_from_disk {

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

  $mail_dir = "/home/aoleg/" . $mail_dir;

  my $file_line = `find $mail_dir`;
  my @files = split("\n", $file_line);

  my @files_out = ();
  
  my $file;
  foreach $file (@files){

    if ( -d $file){
      #print "$file is a directory, skipping\n";
      next;
    }

    if ($file =~ /\.msf$/){
      #print "Skipping $file\n";
      next;
    }
    
    my $filesize = stat($file)->size;
    #print "$filesize Size of $file is $filesize\n";
    
    if ($filesize >= 15000000){
      print "Skip $file as too big\n";
      next;
    }

    push(@files_out, $file);

  }

  return @files_out;
}

sub get_ids {

  my $mailbox = shift;
  my $id_file = shift;
  
  # make file blank before we start
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

sub extract_ids {
  
  my $folder = shift;
  my @ids = ();
  
  my @mails;
  &read_mailbox($folder, \@mails);

  my $message;
  foreach $message (@mails){

    next unless ($message =~ /^From /);
    
    my ($header, $body) = &extract_header_body ($message);

    $header = &add_message_id_if_needed($header);
       
    my $id = extract_message_id($header);

    push (@ids, $id);
  }

  return @ids;
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

    next unless ($message =~ /^From /);
    
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
