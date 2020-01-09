#!/usr/bin/perl
use strict;	       # 'strict' insists that all variables be declared
use diagnostics;       # 'diagnostics' expands the cryptic warnings
use lib $ENV{HOME} . '/bin/mailutils';
#undef $/;             # Keep this commented, want to read a line at a time

my $max_size = 5e+6; # Split a huge mailbox in chunks of size <= $max_size

MAIN: {

  # Split a big mailbox into small mailboxes. Read the big mailbox
  # one line at a time to conserve memory.
  
  my ($line, $big_mailbox_file, $count);

  if (! @ARGV){
    print "Usage $0 big_mailbox_file\n";
    exit(0);
  }

  $big_mailbox_file = $ARGV[0];
  if (!-e $big_mailbox_file){
    print "File $big_mailbox_file does not exist\n";
    exit(0);
  }

  my $small_mailbox = "";

  # Use a big number for $count so that files are sorted well later
  $count = 1000;

  # Will read one line at a time from the big mailbox, rather than the
  # entire mailbox at once, since the mailbox can be huge
  
  open(FILE_BIG, "<$big_mailbox_file");
  
  for ( ; ; ){

    $line = <FILE_BIG>;
    last unless (defined ($line)); # reached the last line in the file

    if ($line !~ /^From.*?\d:\d\d:\d\d/){
      # Not the start of a new message, so just append the current
      # line and move to the next line
      $small_mailbox .= $line;
      next;
    }

    # Start of a new message. If the small mailbox is bigger than
    # prescribed size, write it to disk, and reset the small
    # mailbox. Else, just append the current line. 

    if (length ($small_mailbox) > $max_size){

      my $small_mailbox_file = $big_mailbox_file . "__" . $count;
      print "Writing to $small_mailbox_file\n";
      open(FILE_SMALL, ">$small_mailbox_file");
      print FILE_SMALL $small_mailbox;
      close(FILE_SMALL);

      # reset small mailbox. It will start with the current line
      $small_mailbox = $line;
      $count++;

    }else{
      $small_mailbox .= $line;
    }
    
  } # end looping over the lines of the current file

  # Write the last chunk
  my $small_mailbox_file = $big_mailbox_file . "__" . $count;
  print "Writing last chunk to $small_mailbox_file\n";
  open(FILE_SMALL, ">$small_mailbox_file");
  print FILE_SMALL $small_mailbox;
  close(FILE_SMALL);
  
  close(FILE_BIG);
}
