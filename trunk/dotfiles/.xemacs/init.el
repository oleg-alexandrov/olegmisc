;;init.el

; Point to the directory(ies) where all other files to be loaded are located.
(setq load-path (cons (expand-file-name "~/.xemacs/") load-path))

(fset 'yes-or-no-p 'y-or-n-p)  ;; Make all "yes or no" prompts show "y or n" instead

(font-lock-mode 1)

;;start gnuserv.
(gnuserv-start)

;; history menu
;(load-library "mas-file-history")
;(setq mas-file-history-menu-title "History")
;(setq mas-file-history-menu-path nil)

;; an awesome library for htmlizing Xemacs's syntax highlightning
;(load-library "htmlize")

;;autosave
(setq auto-save-directory "~/.xemacs/")

;load the redo package
(require 'redo)

;Saves a history of commands used previously (including other times XEmacs was used). 
(require `savehist)
(setq savehist-file "~/.xemacs/history")
(setq savehist-length 1000)
(savehist-mode 1)

;Saves the position the cursor was in a file before the file was closed.
(load-library "saveplace")
(setq save-place-file "~/.xemacs/places")
(setq shadow-todo-file "~/.xemacs/shadow-todo")

;; my toolbar with gnome icons
;(load-library "toolbar-my")

;; abbreviations
(setq abbrev-file-name "~/.xemacs/abbreviations-my.el")
(if (file-readable-p abbrev-file-name)
    (read-abbrev-file abbrev-file-name))

;; dictionary
;(setq ispell-prefer-aspell t)
;(setq ispell-personal-dictionary "~/.xemacs/ispell-dictionary")

;;some functions and global keys
(load-library "global-my")

; to automatically remove annoying ^M from end of windows files. But this slows things down a bit
;;(add-hook 'find-file-hooks 'dos-to-unix) 

; switch buffers
(setq iswitchb-buffer-ignore '("^ " "^\\*"))
(setq iswitchb-default-method 'samewindow)
(setq iswitchb-case t); case insensitive
(require 'edmacro)
(defun iswitchb-local-keys ()
  (mapc (lambda (K) 
          (let* ((key (car K)) (fun (cdr K)))
            (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
        '(("<right>" . iswitchb-next-match)
          ("<left>"  . iswitchb-prev-match)
          ("<up>"    . ignore             )
          ("<down>"  . ignore             ))))
(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

; turn on spelling for source code 
;(flyspell-prog-mode)

;;auto-insert stuff
(require 'autoinsert)
(add-hook 'find-file-hooks 'auto-insert)
(setq auto-insert-directory "~/.xemacs/")
(setq auto-insert-query nil)
(load-library "autoinsert-my")

; make the delete key work properly in tcl
(add-hook 'tcl-mode-hook
          (local-set-key [(delete)] 'backward-or-forward-delete-char)
          )

;;to enable control-x to copy, control v to paste,  etc. 
(load-library "cua-mode")
(CUA-mode 1)

; no bell
(setq bell-volume 0)

;; ------------------- mode specific settings ---------

;; auto-mode-alist
(setq auto-mode-alist
      (append '(
		("\\.bash"                . shell-script-mode)
		("\\.sh"                  . shell-script-mode)
		("\\.aliases"             . shell-script-mode)
		("\\.param"               . shell-script-mode)
		("\\.java$"               . java-mode)
		("\\.C$"                  . c++-mode)
		("\\.cc$"                 . c++-mode)
		("\\.hh$"                 . c++-mode)
		("\\.h$"                  . c++-mode)
		("\\.cc$"                 . c-mode)
		("\\.tex$"                . LaTeX-mode)
		("\\.php$"                . c++-mode)
		("\\.s?html?\\'"          . html-mode)
		("\\.\\([pP][Llm]\\)\\'"  . cperl-mode)
		("\\.\\(acumask\\)\\'"    . cperl-mode)
		("\\.\\(param\\)\\'"      . text-mode)
		("\\.m\\'"                . matlab-mode)
		("muttrc\\'"              . muttrc-mode)
		("^\/tmp\/mutt"           . mutt-mode)
		) auto-mode-alist))



;; mutt mode.
 (autoload 'mutt-mode "~/.xemacs/mutt.el" "Major mode to compose mail" t)
 (add-hook 'mutt-mode-hook 
 	  '(lambda()
 	     (load-library "mail-my")
 	     ))


(add-hook 'fundamental-mode-user-hook
	  '(lambda()
	     (local-set-key [(control j)] 'fill-paragraph)
	  ))


;; plain text mode
(add-hook 'text-mode-hook 
	  '(lambda()
;	     (flyspell-mode 1) ;; underline spelling mistakes
	     ))


;;matlab
(autoload 'matlab-mode "~/.xemacs/matlab.el" "matlab major mode." t)
(add-hook 'matlab-mode-hook 
	  '(lambda()
	     ;;my personal stuff
	     (load-library "matlab-my")
	     ))
	     

;; muttrc mode.
(autoload 'muttrc-mode "muttrc-mode.el"
  "Major mode to edit muttrc files" t)



;TeX
;(require 'tex-site)
(add-hook 'LaTeX-mode-hook 
	  '(lambda()
	     ;; LaTeX toolbar
	     (load-library "oleg-latex-toolbar")
	     
	     ;;my personal latex stuff
	     (load-library "latex-my")
	     ))

;;Fortran
(add-hook 'fortran-mode-hook 
	  '(lambda()
	     (load-library "fortran-my")
	     ))

;;Fortran90
(add-hook 'f90-mode-hook 
	  '(lambda()
         (load-library "fortran90-my")
	     ))

;;C
(add-hook 'c-mode-hook 
	  '(lambda()
	     (load-library "perl-my")
	     ))


;;C++
(add-hook 'c++-mode-hook 
	  '(lambda()
	     (load-library "c++-my")
	     ))
(add-hook 'c++-mode-hook 'turn-on-auto-revert-mode)

(add-hook 'java-mode-hook 
	  '(lambda()
	     (load-library "c++-my")
	     (load-library "java-my")
	     ))
(add-hook 'java-mode-hook 'turn-on-auto-revert-mode)

;;Perl
(add-hook 'cperl-mode-hook 
	  '(lambda()
	     (load-library "perl-my")
	     ))


;;;Emacs-Lisp
(add-hook 'emacs-lisp-mode-hook 
	  '(lambda()
	     (load-library "emacs-lisp-my")
	     
	     ))

;; Shell
(add-hook 'shell-script-mode-hook 
	  '(lambda()
	     (load-library "shell-script-my")
	     
	     ))

;Html
;(add-hook 'html-mode-hook 'hm--html-mode)
(add-hook 'html-mode-hook 
	  '(lambda()
	     (load-library "html-my")
	     ))


;; never indent using tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)


; Bookmarks
(setq bookmark-save-flag 1)
(setq bookmark-default-file "~/.xemacs/bookmarks.bmk")
(bookmark-load bookmark-default-file)
(setq tags-case-fold-search t)
(setq case-fold-search t)
(visit-tags-table "~/opc/dev/TAGS")
