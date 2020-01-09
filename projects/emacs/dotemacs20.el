; Saves a history of files opened previously (including other times XEmacs was used - very useful)
(require `savehist)
(setq savehist-file "~/.xemacs/history")
(setq savehist-length 1000)
(savehist-load)

; Saves the position the cursor was in a file before the file was closed.
(load-library "saveplace")
(setq save-place-file "~/.xemacs/places")
(setq shadow-todo-file "~/.xemacs/shadow-todo")

