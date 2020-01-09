(setq vm-folder-directory "~/mail/"
      vm-primary-inbox "~/mail/INBOX"
      vm-crash-box "~/mail/INBOX.CRASH"
      mail-archive-file-name "~/mail/sent-mail"
      vm-folders-summary-database "~/mail/.vm.folders.db"
      )
(setq vm-delete-after-archiving t)
(setq vm-delete-after-saving t)
(setq mail-signature-file "~/.signature" mail-signature t)
(setq vm-frame-per-folder nil)
(setq vm-use-toolbar nil)
(setq vm-frame-per-composition nil)
(setq vm-use-lucid-highlighting t)
(setq vm-included-text-prefix "> ") ;; reply string
(setq vm-forwarded-headers '("From:" "To:" "Subject:" "Date:"))
(setq vm-visible-headers  '("From:" "Sender:"  "To:" "Cc:"  "Subject:" "Date:"))


; some colors
(require 'highlight-headers)
(make-face-bold      'message-headers)
(make-face-unitalic  'message-headers)
(set-face-foreground 'message-headers "red")
(make-face-unbold    'message-header-contents)
(make-face-unitalic  'message-header-contents)
(set-face-foreground 'message-header-contents "blue")
(make-face-bold      'message-highlighted-header-contents)
(make-face-unitalic  'message-highlighted-header-contents)
(set-face-foreground 'message-highlighted-header-contents "blue")
(make-face-unbold    'message-cited-text)
(make-face-unitalic  'message-cited-text)
(set-face-foreground 'message-cited-text "green")



(setq vm-auto-get-new-mail nil)
;; Non-nil value causes VM to automatically move mail from spool files
;; to a mail folder when the folder is first visited


(setq vm-highlighted-header-regexp "From:\\|Subject:")

; order in which to show messages
(add-hook 'vm-summary-mode-hook
	  '(lambda()
	     (vm-sort-messages "reversed-date")
	     ))

;; something of the same kind
(add-hook 'vm-arrived-message-hook
	  '(lambda()
	     (vm-sort-messages "reversed-date")
	     ))


;; Delete message and move forward to next message
(defun vm-delete-message-forward ()
  "Delete message and move forward to next message"
  (interactive)
  (vm-delete-message 1)
  (vm-next-message)
)

; some keyboard shortcuts
(add-hook 'vm-mode-hook
	  '(lambda()
	     (define-key vm-mode-map "c" 'vm-mail) ; compose
	     (define-key vm-mode-map "g" 'vm-visit-folder); switch to a different folder
	     (define-key vm-mode-map "N" 'vm-get-new-mail) ; fetch new mail
	     (define-key vm-mode-map "x" 'vm-expunge-folder) ; remove messages flagged to be deleted
	     (define-key vm-mode-map "r" 'vm-reply-include-text)
	     (define-key vm-mode-map "d" 'vm-delete-message-forward)
	     (define-key vm-mode-map "h" 'vm-expose-hidden-headers)
	     (define-key vm-mode-map [(return)] 'vm-scroll-forward); next message
	     
))


;;; other keybindings. Use with care. Might conflict some of the above.

;(define-key vm-mode-map [(up)] 'vm-previous-message)
;(define-key vm-mode-map [(down)] 'vm-next-message)
;(define-key vm-mode-map " " 'vm-scroll-forward)
;(define-key vm-mode-map "A" 'vm-auto-archive-messages)
;(define-key vm-mode-map "B" 'vm-resend-message)
;(define-key vm-mode-map "D" 'vm-decode-mime-message)
;(define-key vm-mode-map "F" 'vm-followup-include-text) 
;(define-key vm-mode-map "G" 'vm-sort-messages)
;(define-key vm-mode-map "H" 'vm-folders-summarize)
;(define-key vm-mode-map "N" 'vm-next-message-no-skip)
;(define-key vm-mode-map "P" 'vm-previous-message-no-skip)
;(define-key vm-mode-map "R" 'vm-reply-include-text)
;(define-key vm-mode-map "S" 'vm-save-folder)
;(define-key vm-mode-map "U" 'vm-unread-message)
;(define-key vm-mode-map "\C-?" 'vm-scroll-backward)
;(define-key vm-mode-map "\C-\M-n" 'vm-move-message-forward)
;(define-key vm-mode-map "\C-\M-p" 'vm-move-message-backward)
;(define-key vm-mode-map "\C-d" 'vm-delete-message-backward)
;(define-key vm-mode-map "\M-g" 'vm-goto-message)
;(define-key vm-mode-map "\M-n" 'vm-next-unread-message)
;(define-key vm-mode-map "\M-p" 'vm-previous-unread-message)
;(define-key vm-mode-map "\M-r" 'vm-resend-bounced-message)
;(define-key vm-mode-map "\r" 'vm-goto-message)  
;(define-key vm-mode-map "\t" 'vm-goto-message-last-seen)
;(define-key vm-mode-map "^" 'vm-goto-parent-message)
;(define-key vm-mode-map "a" 'vm-set-message-attributes)
;(define-key vm-mode-map "b" 'vm-scroll-backward)
;(define-key vm-mode-map "c" 'vm-mail)
;(define-key vm-mode-map "d" 'vm-delete-message)
;(define-key vm-mode-map "e" 'vm-edit-message)
;(define-key vm-mode-map "f" 'vm-followup)
;(define-key vm-mode-map "h" 'vm-summarize) 
;(define-key vm-mode-map "j" 'vm-discard-cached-data)
;(define-key vm-mode-map "k" 'vm-kill-subject)
;(define-key vm-mode-map "n" 'vm-next-message)
;(define-key vm-mode-map "p" 'vm-previous-message)
;(define-key vm-mode-map "qq" 'vm-continue-composing-message)
;(define-key vm-mode-map "r" 'vm-reply)
;(define-key vm-mode-map "s" 'vm-save-message)  
;(define-key vm-mode-map "t" 'vm-expose-hidden-headers)
;(define-key vm-mode-map "u" 'vm-undelete-message)
;(define-key vm-mode-map "w" 'vm-save-message-sans-headers)
;(define-key vm-mode-map "z" 'vm-forward-message)
;(define-key vm-mode-map "|" 'vm-pipe-message-to-command)
;(define-key vm-mode-map [backspace] 'vm-scroll-backward)
;(define-key vm-mode-map [delete] 'vm-scroll-backward)
