#!/usr/bin/perl
use strict;		      # 'strict' insists that all variables be declared
use diagnostics;	      # 'diagnostics' expands the cryptic warnings
undef $/; # undefines the separator. Can read one whole file in one scalar.

MAIN: {

  open(FILE, "<reminders.txt");
  my @entries = split("\n", <FILE>);
  close(FILE);

  my ($entry, $date, $subject);

  my $gcal = "BEGIN:VCALENDAR\n";
  foreach $entry (@entries){
    
    next unless ($entry =~ /^(\d+\/\d+)\s+(.*?)$/);

    $date    = $1;
    $subject = $2;

    next if ($subject =~ /tomorrow/i);

    $date =~ s/(\d+)/&add_zero($1)/eg;
    $date =~ s/[^\d]*//g;
    $date = "2009" . $date;

    $gcal
       .=
          "BEGIN:VEVENT\n"
             . &form_event($date, $subject)
                . &form_alarms('oleg.alexandrov@gmail.com')
                   . &form_alarms('oleg.alexandrov@cadence.com')
                   . "END:VEVENT\n";
    
  }
  $gcal .= "END:VCALENDAR\n";
  
  open(FILE, ">google_reminders.ics");
  print FILE $gcal;
  close(FILE);
  
}

sub add_zero{

  my $d = shift;

  if ($d =~ /^\d$/){
    return "0" . $d;
  }else{
    return $d;
  }

}

sub form_event {

  my $date    = shift;
  my $subject = shift;
  
  return 'DTSTART;TZID=America/Los_Angeles:' . $date . 'T110000
DTEND;TZID=America/Los_Angeles:' . $date . 'T120000
RRULE:FREQ=YEARLY;WKST=SU
CLASS:PRIVATE
SEQUENCE:1
STATUS:CONFIRMED
SUMMARY:' . $subject . '
TRANSP:OPAQUE
';
     
  
}
sub form_alarms{

  my $email = shift;
  
  return 'BEGIN:VALARM
ACTION:EMAIL
DESCRIPTION:This is an event reminder
SUMMARY:Alarm notification
ATTENDEE:mailto:' . $email . '
TRIGGER:-P2D
END:VALARM
BEGIN:VALARM
ACTION:EMAIL
DESCRIPTION:This is an event reminder
SUMMARY:Alarm notification
ATTENDEE:mailto:' . $email . '
TRIGGER:-P1D
END:VALARM
BEGIN:VALARM
ACTION:EMAIL
DESCRIPTION:This is an event reminder
SUMMARY:Alarm notification
ATTENDEE:mailto:' . $email . '
TRIGGER:-P0DT0H10M0S
END:VALARM
';

}
