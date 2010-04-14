#!/usr/bin/perl

# FvwmCommand script

# screen width
if ( `xwininfo -root` =~ /Width: (\d+)\s*Height: (\d+)/ ) {
  $SW = $1;
  $SH = $2;
} else {
  # some resonable number if xwininfo doesn't work
  $SW = 1024;
  $SH = 780;
}

print "Screen dims are $SW $SH\n";

# start a dedicated server
$fifo = "$ENV{'HOME'}/.FCMfocus";
system( "$ENV{'HOME'}/bin/FvwmCommand 'FvwmCommandS $fifo'");
#for slow machine
select(undef,undef,undef,1);

# we need this to run this script in background job
$SIG{'TTIN'} = "IGNORE";

# start monitoring (-m option ) all fvwm transaction (-i3 option )
open( FCM, "FvwmCommand -f $fifo -m -i3 |" ) || die "FCM $fifo";

# send command through the new fifo which is "$fifo" + "C"
open( FCC, ">${fifo}C" ) || die "FCC $fifo" ;

# appearantly, it has be unbuffered
select( FCC ); $| = 1;
select( STDOUT ); $| = 1;

my (%Windows, $curr_focus, $prev_focus, $prev_focus2, $win_count);
$win_count = 0;
while ( <FCM> ) {
  
  if ( /^(0x[\da-f]+) frame\s+x ([\-\+\d]+), y ([\-\+\d]+), width ([\-\+\d]+), height ([\-\+\d]+)/ ) {
    
    $id     = $1;
    $x      = $2;
    $y      = $3;
    $width  = $4;
    $height = $5;
    
    if (!exists $Windows{$id}){
      
      $Windows{$id} = $win_count; $win_count++;
      print "$id $win_count added\n";
    
      print "---$id $x $y $width $height\n";
      print "$_\n";
      
      # fvwm doesn't like commands from modules too fast
      #select(undef,undef,undef, 0.4 );
      #print FCC "windowid $id move ${x}p ${y}p\n";
      
      my $ctrx = $x + $width/2;

      my $half = $SW/2;
      if ($x < $half && $x + $width > $half){

        # If a window is partially on one screen and partially on the other
        # screen, move it completely to the screen containing its center of gravity.

        if ($ctrx < $half/2 ||
            ($ctrx < $half && $width >= $half/2 && $height >= $SH/2) ){
          # Move to the left half of the left screen
          print "windowid $id move 0p 0p\n";
          print FCC "windowid $id move 0p 0p\n";
        }elsif ($ctrx < $half){
          # Move so that the right edge of the window is right in the middle
          my $nx = $half - $width;
          print "windowid $id move ${nx}p ${y}p\n";
          print FCC "windowid $id move ${nx}p ${y}p\n";
        }else{
          # Move to the right screen
          print "windowid $id move ${half}p ${y}p\n";
          print FCC "windowid $id move ${half}p ${y}p\n";
        }
        
      }
      
      #print "windowid $id move 231p 0p\n";
      #print FCC "windowid $id move 231p 0p\n";

      # focus it
      #print FCC "windowid $id focus\n";
      #print "windowid $id focus\n";
      #FvwmCommand -mi1 'windowid 0x012dabc8 focus'
    }
    
  } elsif (/^(0x[\da-f]+) destroy/){
    
    $id = $1;
    delete $Windows{$id};
    print "$id destroyed\n";
  }

}


#   elsif ( /^(0x[\da-f]+) focus change/ ) {
    
# #     $curr_focus = $1;
# #     print "$_\n";
# #     print "Curr focus is $curr_focus\n";

# #     # Find the id of the previous window holding the focus
# #     my $prev_focus = "";
# #     foreach my $id (sort { $Windows{$b} <=> $Windows{$a} } keys %Windows){
# #       $prev_focus = $id;
# #       last;
# #     }

# #     print "Prev focus was $prev_focus $Windows{$prev_focus}\n";
# #     if (exists $Windows{$curr_focus}){
# #       $tmp = $Windows{$prev_focus};
# #       $Windows{$prev_focus} = $Windows{$curr_focus};
# #       $Windows{$curr_focus} = $tmp;
# #     }
# #     print "Curr focus is $curr_focus $Windows{$curr_focus}\n";
# #     print "Prev focus was $prev_focus $Windows{$prev_focus}\n";
    
#   } elsif (/^(0x[\da-f]+) destroy/){
    
# #     $id = $1;
# #     delete $Windows{$id};
    
# #     print "$id destroyed\n";
    
# #     if (exists $Windows{$prev_focus}){
      
# #       print "Switch focus from $curr_focus to $prev_focus\n";
# #       $curr_focus = $prev_focus;
      
# #       select(undef,undef,undef, 0.4 );
# #       print FCC "windowid $curr_focus focus\n";
# #       print "windowid $curr_focus focus\n";
      
# #     }else{
      
# #       #print "Window $curr_focus does not exist\n";
# #       #print "But window $prev_focus2 exists\n";
      
# #     }
    
#   }
  
# }
  
# #   elsif ( /^$id class/ ) {

# #     if ( !/\sNetscape/ ) {
# #       # not Netscape
# #       last;
# #     }

# #     # the next line should be resource line
# #     $_ = <FCM>;

# #     # resource line tells what the window is
# #     if ( /^$id resource/ ) {

# #       # search for Netscape popups
# #       if ( /\s+\w+popup/ ) {

# #         # fvwm doesn't like commands from modules too fast
# #         select(undef,undef,undef, 0.4 );

# #         # focus it
# #         print FCC "windowid $id focus\n";

# #       }
# #       # search for Netscape download or upload window
# #       elsif ( /\s+(Down|Up)load/ ) {
# #         select(undef,undef,undef, 0.4 );

# #         # move to the right edge, keep the whole window in screen
# #         $x = $SW - $width;
# #         print FCC "windowid $id move ${x}p ${y}p\n";
# #       }
# #       last;
# #     }
# #   }




