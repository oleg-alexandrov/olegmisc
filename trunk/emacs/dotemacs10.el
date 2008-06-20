(custom-set-variables               ; only one entry with this name must exist in custom.el
 '(delete-key-deletes-forward t)    ; make the delete key delete the next character (as expected)
 '(line-number-mode t)              ; show the line and column number of the cursor in the ...
 '(column-number-mode t)            ; ... lower part of the screen (very useful)
 '(make-backup-files nil)           ; do not make back-up files (they just trash the directories)
 '(visible-bell t)                  ; flash the screen instead of beeping (which can be annoying)
 '(mouse-yank-at-point t)           ; paste where the cursor is, and not where the mouse clicks in
 '(user-mail-address "my@email" t)  ; your e-mail address (otherwise XEmacs will nag)
 '(query-user-mail-address nil)     ; don't ask what my e-mail address is all the time
)                                   ; don't miss this closing parenthesis!


