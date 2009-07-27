(require 'ido)
(ido-mode t)
(setq ido-confirm-unique-completion t)
(setq ido-default-buffer-method 'samewindow)
(setq ido-use-filename-at-point t)
(ido-mode t)

;(set-face-background 'ido-first-match "white")
;(set-face-foreground 'ido-subdir "blue3")
(icomplete-mode 1)

;;bind keyboard-escape-quit to a sequence of 2 escapes instead of 3 of them.
(when (console-on-window-system-p)
  (global-set-key '(meta escape) 'keyboard-escape-quit)
  (define-key isearch-mode-map '(meta escape) 'isearch-cancel))

;; Filename completion ignores these.
(setq completion-ignored-extensions
      (append completion-ignored-extensions
	      '(".dvi" ".aux" ".toc" ".ps" ".pdf" ".log" ".bbl" ".blg" ".thm"
		".sty" ".mexglx" ".cmd")))


;;;; 1999-07-12 Noah Friedman <friedman@splode.com>
; (defun make-buffer-file-executable-if-script-p ()
;   "Make file executable according to umask if not already executable.
; If file already has any execute bits set at all, do not change existing
; file modes."
;   (and (save-excursion
;          (save-restriction
;            (widen)
;            (goto-char (point-min))
;            (save-match-data
;              (looking-at "^#!"))))
;        (let* ((current-mode (file-modes (buffer-file-name)))
;               (add-mode (logand ?\411 (default-file-modes))))
;          (or (/= (logand ?\411 current-mode) 0)
;              (zerop add-mode)
;              (set-file-modes (buffer-file-name)
;                              (logior current-mode add-mode))))))
;;(add-hook 'after-save-hook 'make-buffer-file-executable-if-script-p)

(defun dos-to-unix ()
  "Remove those annoying ^M from the end of lines in files imported from Windows"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match ""))
  (goto-char (point-min))
  )

  
;;; Jump forward and backward through buffer list ; XEmacs mailing list 
(defun gse-unbury-buffer ()
  "Switch to the buffer at the bottom of the buffer list, if it's not a
'hidden' buffer."
  (interactive)
  (let ((all-buffers (buffer-list))
        (done nil)
        (i 1))
    (setq i (- (length all-buffers) 1))
    (while (and (not done) (>= i 0))
      (let ((buf (nth i all-buffers))
            (first-char ""))
        (setq first-char (substring (buffer-name buf) 0 1))
        (if (not (or
                  (equal first-char "*")
                  (equal first-char " ")))
            (progn
              (switch-to-buffer buf)
              (setq done t))
          (setq i (- i 1))
          )))
    ))

(defun gse-bury-buffer ()
  "Bury the current buffer until you find a non-'hidden' buffer."
  (interactive) (bury-buffer)
  (let ((all-buffers (buffer-list))
	(done nil)
	(i 0))
    (while (and (not done) (<= i (length all-buffers)))
      (let ((buf (nth i all-buffers))
	    (first-char ""))
	(setq first-char (substring (buffer-name buf) 0 1))
	(if (not (or
		  (equal first-char "*")
		  (equal first-char " ")))
	    (progn
	      (switch-to-buffer buf)
	      (setq done t))
	  (setq i (+ i 1))
	  )))
    ))

(defun my-insert-parantheses ()
  "A simple stupid function"
  (interactive)
  (insert "()")
  (backward-char 1)
  )


;; Rearrange the modeline so that everything is to the left of the
;; long list of minor modes, which is relatively unimportant but takes
;; up so much room that anything to the right is obliterated.
(setq-default
 modeline-format
 (list
  ""
  (if (boundp 'modeline-multibyte-status) 'modeline-multibyte-status "")
  (cons modeline-modified-extent 'modeline-modified)
  (cons modeline-buffer-id-extent
 	(list (cons modeline-buffer-id-left-extent
 		    (cons 15 (list
 			      (list 'line-number-mode "%l ")
 			      (list 'column-number-mode "%c ")
 			      (cons -3 "%p"))))
 	      (cons modeline-buffer-id-right-extent "%17b")))
  "   "
  'global-mode-string
  "   %[("
  (cons modeline-minor-mode-extent
 	(list "" 'mode-name 'minor-mode-alist))
  (cons modeline-narrowed-extent "%n")
  'modeline-process
  ")%]----"
  "%-"
  ))

;; Get rid of modeline information taking up too much space -- in
;; particular, minor modes that are always enabled.
(setq pending-delete-modeline-string "")
(setq filladapt-mode-line-string "")
;; lazy-lock doesn't have a variable for its modeline name, so we have
;; to do a bit of surgery.
(and (assoc 'lazy-lock-mode minor-mode-alist)
     (setcdr (cdr (cadr (assoc 'lazy-lock-mode minor-mode-alist))) ""))

(defun un-comment-region ()
  (interactive)
  (comment-region (point) (mark) -1)
  )

(defun yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (yank)
  (call-interactively 'indent-region))

(defun smart-space ()
  (interactive)
  (if (not (expand-abbrev))
      (progn (insert " ")
 	     (do-auto-fill)
	     ))
  )


;  in this way one can move back and forth between previous cuts and copies (that is, navigate the clipboard)
;  with alt-y and alt-u
(defun pop-prev-yank ()
  (interactive)
  (yank-pop -1)
  )

(defun my-delete-tail ()
  (interactive)
  (let ((beg (point)))
    (end-of-buffer)
    (delete-region beg (point)))
  (beginning-of-buffer)
  )

(defun my-dummy-function ()
  (interactive)
  ;; do nothing
  )

(defun my-insert-space ()
  (interactive)
  (insert " ")
  )

;  search forward with control-f and backward with alt-f
(global-set-key [(control f)] 'isearch-forward)
(define-key isearch-mode-map [(control f)] (lookup-key isearch-mode-map "\C-s"))
(define-key minibuffer-local-isearch-map [(control f)]
  (lookup-key minibuffer-local-isearch-map "\C-s"))

(global-set-key [(meta f)] 'isearch-backward)
(define-key isearch-mode-map [(meta f)] (lookup-key isearch-mode-map "\C-r"))
(define-key minibuffer-local-isearch-map [(meta f)]
  (lookup-key minibuffer-local-isearch-map "\C-r"))



(defun my-fill-paragraph-or-region ()
  (interactive)
  (call-interactively 'fill-paragraph-or-region)
  (next-line 1)
  )


(defun tetris ()
  (interactive)
  (message "function not defined!")
  )

(defun duplicate-line ()
  "Comments the current line and goes to the next one"
  (interactive)
  (copy-region-as-kill (point-at-bol) (point-at-eol))
  (end-of-line)
  (insert "\n")
  (yank)
  (beginning-of-line)
  )

(defun jump-and-insert-space () 
  (interactive)
  (forward-char 1)
  (insert " ")
  )

; (defun my-htmlize-make-file-name (file)
;   "Make an HTML file name from FILE. Stolen from htmlize.el"
;   (interactive)
;   (let ((extension (htmlize-file-name-extension file))
;  	(sans-extension (file-name-sans-extension file)))
;     (if (or (equal extension "html")
;  	    (equal extension "htm")
;  	    (equal sans-extension ""))
;  	(concat file ".html")
;       (concat sans-extension ".html"))))
;
;
; (defun my-htmlize ()
;   "Htmilze a file called say mygraph.m and save it as mygraph.html.
;  Delete the portion before <pre> and after </pre>. Do not insert the last-modfied string"
;  
;   (interactive)
;   (let ((new-html-buffer
; 	 (my-htmlize-make-file-name
; 	  (file-name-nondirectory
; 	   (buffer-file-name)))))
;     (htmlize-buffer)
;     (beginning-of-buffer)
;     (let ((beg (point)))
;       (search-forward "<pre>")
;       (search-forward "\n")
;       (delete-region beg (point)))
;     (insert
;      "<p />
;  <table border=\"1\" cellspacing=\"0\" cellpadding=\"2\"  bgcolor=\"#e0e0e0\"  width=\"100%\">
;  <tr><td>
;  <pre>") ;; thus don't let <pre> stand on a line by itself
;     (search-forward "</pre>")
;     (let ((beg (point)))
;       (end-of-buffer)
;       (delete-region beg (point)))
;     (insert "\n</td></tr></table>")
;     (beginning-of-buffer)
;     (while (re-search-forward "[\n]+</span>" nil t) (replace-match "</span>\n")) ; make the source look nicer
;     (beginning-of-buffer)
;     (re-search-forward "[ \t\n]*</pre>" nil t) (replace-match "</pre>")
;
;  (setq hm--html-automatic-update-modified-line nil); do not insert the last-modfied string
;     (write-file new-html-buffer)
;  (setq hm--html-automatic-update-modified-line t) ; change back
;     (kill-buffer (current-buffer))
;     ))

;;;; see another example below with search and replace
; (defun copy-to-blythe ()
;   (interactive)
;   (let ((origin-file (buffer-file-name)))
;     (let ((destination-file (concat "/u/cedar/h1/afa/aoleg" (nth 1 (split-string origin-file "oalexandrov")) )))
;       (let ((command (concat "scp " origin-file " aoleg@blythe.math.ucla.edu:" destination-file)))
;  	(message command)
;  	(shell-command command)
;  	(message (concat "Done: " command))
;  	)
;       )))

; (defun save-buffer-and-copy-to-blythe ()
;   (interactive)
;   (save-buffer)
;   (copy-to-blythe)
;   )


(defun  copy-to-shc ()
  (interactive)
  
  (let (( my_file (buffer-file-name) )) ; assign to my_file the current file
					;(replace-string)

    (if (string-match "/home/han/" my_file)

	(let ((my_new_file (replace-match "/shc/data-store01/acm/han/" t "/home/han/" my_file) ))
	  (let (( my_command (concat "scp " my_file " han@shc.cacr.caltech.edu:" my_new_file )))
	    (message my_command)
	    (shell-command my_command)
	    (message (concat "Done!!! " my_command) )
	    )
	  )
      )
    
    )
  )

(defun call-tkdiff ()
  (interactive)

  (shell-command (concat "/home/olegalex/bin/call_tkdiff.pl " (buffer-file-name) )  t)
  
  )


(defun un-define-mode-abbrev (name)
  "Define ABBREV as a mode-specific abbreviation for EXPANSION."
  (interactive "sUndefine mode abbrev: ")
  (define-abbrev (or local-abbrev-table
		     (error "Major mode has no abbrev table"))
    (downcase name) nil nil 0)
  )

(defun kill-all-line ()
  (interactive)
  (beginning-of-line)
  (kill-line 1)
  )

(defun list-directory ()
  (interactive)
  )

(defun open-spq ()
  (interactive)
  (find-file "~/diverse/mj.txt")
  (end-of-buffer)
  (insert "\n\n")
  (shell-command "date"  t)
  (end-of-line)
  (insert "\n\n")
  (end-of-buffer)
  )

(defun my-exit-mail ()
  (interactive)
  (save-buffer)
  (gnuserv-edit)
  )

 
(defun pm2wiki ()
  (interactive)
  (shell-command-on-region (point-min) (point-max) "~/bin/pm2wp.pl" nil t)
  )
      
(defun putcat ()
  (interactive)
  (shell-command-on-region (point-min) (point-max) "~/.xemacs/put_cat.pl" nil t)
  )

(defun new-line-and-indent ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-according-to-mode)
  )


(defun insert-brackets ()
  (interactive)
  (insert "\[\]")
  (backward-char 1)
  )

(defun insert-semicolon-and-newline ()
  (interactive)
  (end-of-line)
  (insert ";\n")
  (indent-according-to-mode)
  )

;; When in X, enable mouse wheel
(defun mwheel-up () (interactive) (scroll-up 1))
(defun mwheel-down () (interactive) (scroll-down 1))

;; Scroll one line at a time when moving cursor off-screen,
;; instead of half a page.
(setq scroll-step 1)

(defun align-repeat (start end regexp)
  "repeat alignment with respect to 
     the given regular expression"
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end 
                (concat "\\(\\s-*\\)" regexp) 1 1 t))

(global-set-key     'button4 'mwheel-down)
(global-set-key     'button5 'mwheel-up)
(global-set-key [(control \')] 'my-delete-tail)
(global-set-key [(control b)] 'byte-compile-and-load-file)
(global-set-key [(control backspace)] 'backward-kill-line)
(global-set-key [(control delete)] 'kill-line)
(global-set-key [(control j)] 'my-fill-paragraph-or-region)
(global-set-key (kbd "LFD") 'my-fill-paragraph-or-region)
    
(global-set-key [(control k)] 'kill-all-line)
(global-set-key [(control l)] 'load-file)
(global-set-key [(control meta a)] 'define-mode-abbrev)
;(global-set-key [(control meta left)] 'gse-bury-buffer)
;(global-set-key [(control meta right)] 'gse-unbury-buffer)
(global-set-key [(control o)] 'ido-find-file) ; open a file or create a new file with Control-o
(global-set-key [(control r)]  'query-replace)
(global-set-key [(control s)] 'save-buffer) ; save with Control-s
(global-set-key [(control space)] 'jump-and-insert-space)
(global-set-key [(control u)] 'yank)
(global-set-key [(control x) (control d)] 'my-dummy-function)
(global-set-key [(control x) (control z)] 'my-dummy-function)
(global-set-key [(control x) (d)] 'duplicate-line)
(global-set-key [(control x) (f)] 'describe-function)
(global-set-key [(control x) (k)] 'describe-key)
(global-set-key [(control x) (l)] 'duplicate-line)
(global-set-key [(control x) (u)] 'un-define-mode-abbrev)
(global-set-key [(meta \9)]  'my-insert-parantheses)
(global-set-key [(meta a)] 'define-mode-abbrev)
(global-set-key [(meta c)] 'comment-region)
(global-set-key [(meta control a)] 'define-mode-abbrev)
(global-set-key [(meta j)] 'forward-char)
(global-set-key [(meta q)] 'kill-this-buffer)
;(global-set-key [(meta s)] 'write-file)	; 'save file as...' with Alt-s
(global-set-key [(meta space)] 'dabbrev-expand)
(global-set-key [(meta t)] 'pop-prev-yank)
(global-set-key [(meta u)] 'un-comment-region)
(global-set-key [(meta z)] 'redo)
(global-set-key [(shift space)] 'my-insert-space)
(global-set-key [(space)] 'smart-space)
(global-set-key [(control \.)] (function (lambda () (interactive) (forward-char 1))))
(global-set-key [(control \,)] (function (lambda () (interactive) (backward-char 1))))
(global-set-key [(meta \')]  (function (lambda () (interactive) (insert "\"\"") (backward-char 1))))
(global-set-key [(control return)] 'my-exit-mail)
(global-set-key [(meta r)] 'copy-to-register)
(global-set-key [(meta i)] 'insert-register)
(global-set-key [(control x) (t)] 'call-tkdiff)
(global-set-key [(meta v)] 'revert-buffer)
(global-set-key [(meta return)] 'new-line-and-indent)
(global-set-key [(meta l)] 'align-repeat)
(local-set-key [(meta \[)] 'insert-brackets)
(global-set-key [(meta \;)] 'insert-semicolon-and-newline)
(global-set-key [(meta o)] 'ido-switch-buffer)
;; terminal keys
;(global-set-key "\e[7~" 'beginning-of-line)
;(global-set-key "\e[8~" 'end-of-line)


; A first attempt to make "control z" work as undo. 
(global-set-key [(control z)] 'undo)

; The above does not work on newer versions of XEmacs. The key
; "Control-z" is highjacked by XEmacs for the "zap-up-to-char"
; function and the global-set-key command above is ignored. The
; solution we choose is to highjack Xemacs's zap-up-to-char function
; itself and force it to do "undo".

(defun zap-up-to-char ()
  (interactive)
  (undo)
  )

; do the same for suspend-emacs in terminal mode
(defun suspend-emacs ()
  (interactive)
  (undo)
  )

