; A first attempt to make "control z" work as undo. 
(global-set-key [(control z)] 'undo)

; The above does not work on newer versions of XEmacs. Then, highjack 
; Xemacs's zap-up-to-char function itself and force it to work as "undo".
(defun zap-up-to-char () 
  (interactive) 
  (undo)
)

