#!/bin/bash -login

xterm

# ulimit -f 2000000000   # max file size (200MB)

# Make the backspace behave
stty erase '^?'

# Bind the windows key for use in fvwm
xmodmap -e  "add mod3 = Super_L"

# Disable control s and control q, so that I can use them in vim.
#stty -a # see what is enabled
stty stop  undef # control s
stty start undef # control q

# touch `uname -n` # this must be first! Create a file showing current machine
# xrdb -load ~/.Xdefaults                                              # X settings 
# xmodmap ~/.xmodmaprc                                                 # keyboard
# xset b 10 90 20                        # cool the beep
# nohup nice -19 ~/bin/background > /dev/null 2>&1 &     \  # background
# nohup nice -19 ~/bin/batch_backup.pl >> ~/diverse/backup_log.txt & 

xv -quit -root ~/diverse/wallpapers/NASA-Apollo8-Dec24-Earthrise_optimal.jpg

xclock -geo -0+0 -brief -d -bg white >/dev/null 2>&1&

~/bin/unclutter -idle 1&

if [ -e ~/diverse/fvwm/bin/fvwm ]; then 
    ~/diverse/fvwm/bin/fvwm
else
  fvwm
fi

