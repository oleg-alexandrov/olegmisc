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

; A first attempt to make "control z" work as undo. 
(global-set-key [(control z)] 'undo)

; The above does not work on newer versions of XEmacs. Then, highjack 
; Xemacs's zap-up-to-char function itself and force it to work as "undo".
(defun zap-up-to-char () 
  (interactive) 
  (undo)
)

; search forward with Ctrl-f
(global-set-key [(control f)] 'isearch-forward)
(define-key isearch-mode-map [(control f)] (lookup-key isearch-mode-map "\C-s"))
(define-key minibuffer-local-isearch-map [(control f)]
  (lookup-key minibuffer-local-isearch-map "\C-s"))

; search backward with Alt-f
(global-set-key [(meta f)] 'isearch-backward)
(define-key isearch-mode-map [(meta f)] (lookup-key isearch-mode-map "\C-r"))
(define-key minibuffer-local-isearch-map [(meta f)]
  (lookup-key minibuffer-local-isearch-map "\C-r"))

