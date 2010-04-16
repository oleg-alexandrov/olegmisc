#!/bin/bash -login

# ulimit -f 2000000000   # max file size (200MB)

# \rm -fv ~/.nfs0* # another silly bug

# touch `uname -n` # this must be first! Create a file showing current machine
# xrdb -load ~/.Xdefaults                                              # X settings 
# xmodmap ~/.xmodmaprc                                                 # keyboard
# xset b 10 90 20                        # cool the beep

# # these can afford a small delay
# (sleep 2;                                                       \  # take a break
# if [ `uname -n` == "blythe" ]; then ~/bin/rxvt  -e mutt & fi;   \  # mail
# #xload -geo 122x50-0-248&                                       \  # xload
# sleep 2; nohup nice -19 ~/bin/background > /dev/null 2>&1 &     \  # background
# if [ `uname -n` == "blythe" ]; then sleep 5;  ~/bin/xe.pl -iconic& else ~/bin/xe2; fi;  \  # xemacs 
# sleep 3; firefox &                                              \  # firefox web browser
# #sleep 100; ~/bin/kill_some_xsessions.pl
# )& 

# # this should run automatically from crontab, but let me put it here too, just in case.
# nohup nice -19 ~/bin/batch_backup.pl >> ~/diverse/backup_log.txt & 

# #\rm -fv ~/.xsession-errors.old

#startkde
#fvwm

xv -quit -root ~/diverse/wallpapers/NASA-Apollo8-Dec24-Earthrise_longer.jpg

~/diverse/fvwm/bin/fvwm

#(sleep 0.5; killall -9 fvwm) & # to avoid yet another bug
#   fvwm -f ~/.fvwm/rc.fvwm 
#   startxfce4
#   gnome-session
#   startkde

# export WINDOWBIN=

# #clean up on exit
# \rm -f `uname -n`
# \rm -f ~/matlab_crash*
# \rm -f ~/.plugin*trace 
# \rm -rfv ~/.jpi_cache/
# \rm -rfv ~/.java/
# \rm -f ~/.openoffice-lock
# \rm -f ~/.fonts.cache*
# \rm -fv ~/.nfs0* # another silly bug
# \rm -fv ~/.xsession-errors.old 
