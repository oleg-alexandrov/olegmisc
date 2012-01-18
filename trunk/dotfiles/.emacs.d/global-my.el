(defun isearch-set-initial-string ()
  (remove-hook 'isearch-mode-hook 'isearch-set-initial-string)
  (setq isearch-string isearch-initial-string)
  (isearch-search-and-update))

(defun isearch-forward-at-point (&optional regexp-p no-recursive-edit)
  "Interactive search forward for the symbol at point."
  (interactive "P\np")
  (if regexp-p (isearch-forward regexp-p no-recursive-edit)
    (let* ((end (progn (skip-syntax-forward "w_") (point)))
           (begin (progn (skip-syntax-backward "w_") (point))))
      (if (eq begin end)
          (isearch-forward regexp-p no-recursive-edit)
        (setq isearch-initial-string (buffer-substring begin end))
        (add-hook 'isearch-mode-hook 'isearch-set-initial-string)
        (isearch-forward regexp-p no-recursive-edit)))))


;; Filename completion ignores these.
(setq completion-ignored-extensions
      (append completion-ignored-extensions
	      '(".dvi" ".aux" ".toc" ".ps" ".pdf" ".log" ".bbl" ".blg" ".thm"
		".sty" ".mexglx" ".cmd")))


;; Check for shebang magic in file after save, make executable if found.
(setq my-shebang-patterns 
      (list "^#!/usr/.*/perl\\(\\( \\)\\|\\( .+ \\)\\)-w *.*" 
            "^#!/usr/.*/sh"
	        "^#!/usr/.*/bash"
	        "^#!/bin/sh"
	        "^#!/bin/bash"))

(defun make-file-executable-if-script ()
  (interactive)
       (if (not (= (shell-command (concat "test -x " (buffer-file-name))) 0))
           (progn 
	     ;; This puts message in *Message* twice, but minibuffer
    	     ;; output looks better.
	     (message (concat "Wrote " (buffer-file-name)))
	     (save-excursion
	       (goto-char (point-min))
	       ;; Always checks every pattern even after
	       ;; match.  Inefficient but easy.
	       (dolist (my-shebang-pat my-shebang-patterns)
	         (if (looking-at my-shebang-pat)
		     (if (= (shell-command  
			     (concat "chmod u+x " (buffer-file-name)))
			    0)
		         (message (concat 
			           "Wrote and made executable " 
			           (buffer-file-name))))))))
         ;; This puts message in *Message* twice, but minibuffer output
         ;; looks better.
         (message (concat "Wrote " (buffer-file-name))))
       )


(add-hook 'after-save-hook 'make-file-executable-if-script)

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


;;; Rearrange the modeline so that everything is to the left of the
;;; long list of minor modes, which is relatively unimportant but takes
;;; up so much room that anything to the right is obliterated.
;(setq-default
; modeline-format
; (list
;  ""
;  (if (boundp 'modeline-multibyte-status) 'modeline-multibyte-status "")
;  (cons modeline-modified-extent 'modeline-modified)
;  (cons modeline-buffer-id-extent
; 	(list (cons modeline-buffer-id-left-extent
; 		    (cons 15 (list
; 			      (list 'line-number-mode "%l ")
; 			      (list 'column-number-mode "%c ")
; 			      (cons -3 "%p"))))
; 	      (cons modeline-buffer-id-right-extent "%17b")))
;  "   "
;  'global-mode-string
;  "   %[("
;  (cons modeline-minor-mode-extent
; 	(list "" 'mode-name 'minor-mode-alist))
;  (cons modeline-narrowed-extent "%n")
;  'modeline-process
;  ")%]----"
;  "%-"
;  ))

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
  (shell-command-on-region (point-min) (point-max) "~/.emacs.d/put_cat.pl" nil t)
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

;;; When in X, enable mouse wheel
;(defun mwheel-up () (interactive) (scroll-up 1))
;(defun mwheel-down () (interactive) (scroll-down 1))

;; Scroll one line at a time when moving cursor off-screen,
;; instead of half a page.
(setq scroll-step 1)

(defun goto-match-paren ()
  "Go to the matching parenthesis if on parenthesis"
  (interactive)
  (cond ((looking-at "[\(\{]") (forward-list 1) (backward-char 1))
        ((looking-at "[\)\}]") (forward-char 1) (backward-list 1))
        )
)

(defun mark-paragraph-and-align ()
  (interactive)
  (call-interactively 'mark-paragraph)
  (call-interactively 'align-repeat)
  )

(defun align-repeat (start end regexp)
  "repeat alignment with respect to 
     the given regular expression"
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end 
                (concat "\\(\\s-*\\)" regexp) 1 1 t))

(defun scp-copy ()
  (interactive)
  (let ((cmd (concat "rsync -avz " (buffer-file-name) " oalexan1@byss:" (replace-regexp-in-string "/Users/oalexandrov/" "/home/oalexan1/" (buffer-file-name) t)
  ;(let ((cmd (concat "scp " (buffer-file-name) " oalexandrov@198.10.124.55:" (replace-regexp-in-string "Users" "home" (buffer-file-name) t)
                     " >/dev/null 2>&1"
                     )
             ))
    (shell-command cmd)
    (message cmd)
    )
  )

(defun save-and-copy ()
  (interactive)
  (save-buffer)
  (make-file-executable-if-script)
  (scp-copy)
  )

; In a terminal we request that we want to open a certain file
; by putting its name in ~/.fileToOpen.
; Here we open the file with that name.
; If a line number is specified, we go to that line.
(defun find-requested-file ()
  (interactive)
  (find-file "~/.fileToOpen")
  (switch-to-buffer ".fileToOpen")

                                        ; Find file name
  (beginning-of-buffer)
  (beginning-of-line)
  (setq bf (point))
  (end-of-line)
  (setq ef (point))
  (setq file (buffer-substring bf ef))

  ; Find line number
  (beginning-of-buffer)
  (beginning-of-line)
  (setq lineno 1)
  (next-line 1)
  (cond ( (looking-at "line: ")
          (search-forward "line: ")
          (setq bl (point))
          (end-of-line)
          (setq el (point))
          (setq lineno (buffer-substring bl el))
          (setq lineno (string-to-number lineno))
          )
        )

  (find-file file)
  (goto-line lineno)
  
  )

;(global-set-key     'button4 'mwheel-down)
;(global-set-key     'button5 'mwheel-up)
(global-set-key [(control \')] 'my-delete-tail)
(global-set-key [(control b)] 'byte-compile-and-load-file)
(global-set-key [(control backspace)] 'backward-kill-line)
(global-set-key [(control delete)] 'kill-line)
(global-set-key [(control j)] 'my-fill-paragraph-or-region)
;(global-set-key [(tab)] 'indent-according-to-mode)
(global-set-key (kbd "LFD") 'my-fill-paragraph-or-region)
    
(global-set-key [(control k)] 'kill-all-line)
(global-set-key [(control l)] 'load-file)
(global-set-key [(control meta a)] 'define-mode-abbrev)
;(global-set-key [(control meta left)] 'gse-bury-buffer)
;(global-set-key [(control meta right)] 'gse-unbury-buffer)
(global-set-key [(control o)] 'ido-find-file) ; open a file or create a new file with Control-o
(global-set-key [(control r)]  'query-replace)
(global-set-key [(control s)] 'save-and-copy) ; save with Control-s
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
;(global-set-key [(meta t)] 'pop-prev-yank)
(global-set-key [("\M-t")] 'toggle-cpp-h)
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
(global-set-key [(control p)] 'scp-copy)
(global-set-key [(control t)] 'isearch-forward-at-point)
(global-set-key [(control tab)] 'other-window)
(global-set-key [(meta o)] 'iswitchb-buffer)
(global-set-key [(meta \;)] 'insert-semicolon-and-newline)
(define-key osx-key-mode-map [(home)]       'beginning-of-line)
(define-key osx-key-mode-map [(end)]        'end-of-line)
(define-key osx-key-mode-map [C-kp-delete]  'kill-line)
(define-key osx-key-mode-map [(meta g)] 'goto-line)
(define-key osx-key-mode-map [(control v)] 'yank)
(define-key osx-key-mode-map [(meta t)] 'toggle-cc-h)
(global-set-key [(meta v)] 'find-requested-file)

;; terminal keys
;(global-set-key "\e[7~" 'beginning-of-line)
;(global-set-key "\e[8~" 'end-of-line)

(global-unset-key [("\M- ")])
(global-set-key [("\M- ")] 'dabbrev-expand)

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

; Get around another highjacking
(defun just-one-space ()
  (interactive)
  (call-interactively 'dabbrev-expand)
  )