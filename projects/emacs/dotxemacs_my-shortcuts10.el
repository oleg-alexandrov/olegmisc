(global-set-key [(control o)] 'find-file)              ; use Ctrl-o to open a (new) file
(global-set-key [(control n)] 'find-file-other-frame)  ; open a file in a new window with Ctrl-n
(global-set-key [(control s)] 'save-buffer)            ; save with Ctrl-s
(global-set-key [(meta s)]    'write-file)             ; 'save file as...' with Alt-s ('meta' is
                                                       ; just another name for the 'Alt' key)
(global-set-key [(control q)] 'save-buffers-kill-emacs); exit XEmacs with Ctrl-q
(global-set-key [(meta q)]    'kill-this-buffer)       ; delete changes (don't save) with Alt-q

(global-set-key [(control t)] 'ispell-buffer)          ; spell-check with Ctrl-t
(global-set-key [(control r)] 'replace-string)         ; search and replace with Ctrl-r

(require 'redo)                                        ; load the 'redo' package
(global-set-key [(meta z)]    'redo)                   ; 'redo', that is, revert the last 'undo'

