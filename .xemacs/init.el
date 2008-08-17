;;init.el

; Point to the directory(ies) where all other files to be loaded are located.
(setq load-path (cons (expand-file-name "~/.xemacs/") load-path))

(fset 'yes-or-no-p 'y-or-n-p)  ;; Make all "yes or no" prompts show "y or n" instead

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

;;load the redo package
(require 'redo)


;;Saves a history of commands used previously (including other times XEmacs was used). 
(require `savehist)
(setq savehist-file "~/.xemacs/history")
(setq savehist-length 1000)
(savehist-load)

;Saves the position the cursor was in a file before the file was closed.
(load-library "saveplace")
(setq save-place-file "~/.xemacs/places")
(setq shadow-todo-file "~/.xemacs/shadow-todo")

;; my toolbar with gnome icons
(load-library "toolbar-my")

;;my abbreviations
(setq abbrev-file-name "~/.xemacs/abbreviations-my.el")
(if (file-readable-p abbrev-file-name)
    (read-abbrev-file abbrev-file-name))

;; dictionary
(setq ispell-personal-dictionary "~/.xemacs/ispell-dictionary")

;;some functions and global keys
(load-library "global-my")

; to automatically remove annoying ^M from end of windows files. But this slows things down a bit
;;(add-hook 'find-file-hooks 'dos-to-unix) 

(load-library "pc-select")
(pc-select-mode)

; switch buffers
(load-library "wcy-swbuff")

;;auto-insert stuff
(add-hook 'find-file-hooks 'auto-insert)
(setq auto-insert-directory "~/.xemacs/")
(setq auto-insert-query nil)
(load-library "autoinsert-my")


;;to enable control-x to copy, control v to paste,  etc. 
(load-library "cua-mode")
(CUA-mode 1)

; no bell
(setq bell-volume 0)

;; ------------------- mode specific settings ---------

;; auto-mode-alist
(setq auto-mode-alist
      (append '(
		("\\.bash"  . shell-script-mode)
		("\\.aliases"  . shell-script-mode)
		("\\.C$"  . c++-mode)
		("\\.cc$" . c++-mode)
		("\\.hh$" . c++-mode)
		("\\.h$"  . c++-mode)
		("\\.cc$" . c-mode)
		("\\.tex$" . LaTeX-mode)
		("\\.php$" . c++-mode)
		("\\.s?html?\\'" . html-mode)
		("\\.\\([pP][Llm]\\)\\'" . cperl-mode)
		("\\.m\\'" . matlab-mode)
		("muttrc\\'" . muttrc-mode)
		("^\/tmp\/mutt" . mutt-mode)
		) auto-mode-alist))



;; mutt mode.
 (autoload 'mutt-mode "~/.xemacs/mutt.elc" "Major mode to compose mail" t)
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
(autoload 'matlab-mode "~/.xemacs/matlab.elc" "matlab major mode." t)
(add-hook 'matlab-mode-hook 
	  '(lambda()
	     ;;my personal stuff
	     (load-library "matlab-my")
	     ))
	     

;; muttrc mode.
(autoload 'muttrc-mode "muttrc-mode.elc"
  "Major mode to edit muttrc files" t)



;TeX
(require 'tex-site)
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

;Html
;(add-hook 'html-mode-hook 'hm--html-mode)
(add-hook 'html-mode-hook 
	  '(lambda()
	     (load-library "html-my")
	     ))


;; never indent using tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)
