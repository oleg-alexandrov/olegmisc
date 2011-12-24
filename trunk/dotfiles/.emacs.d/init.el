;;init.el

; Point to the directory(ies) where all other files to be loaded are located.
(setq load-path (cons (expand-file-name "~/.emacs.d/") load-path))

(fset 'yes-or-no-p 'y-or-n-p)  ;; Make all "yes or no" prompts show "y or n" instead

(setq make-backup-files nil) 
(setq version-control 'never)
(setq vc-cvs-stay-local nil)

(setq mouse-yank-at-point t)

(setq default-frame-alist
      '(       
        (cursor-color . "Red")
        (cursor-type . bar)
))

(global-auto-revert-mode 1)
(add-hook 'c++-mode-hook 'turn-on-auto-revert-mode)
(add-hook 'java-mode-hook 'turn-on-auto-revert-mode)

;;start gnuserv.
;(gnuserv-start)(setq make-backup-files nil) 

;; history menu
;(load-library "mas-file-history")
;(setq mas-file-history-menu-title "History")
;(setq mas-file-history-menu-path nil)

;; an awesome library for htmlizing Xemacs's syntax highlightning
;(load-library "htmlize")

;;autosave
(setq auto-save-directory "~/.emacs.d/")

;;load the redo package
;(require 'redo)

(require 'paren)
(show-paren-mode t)                 ; turn paren-mode on

;;Saves a history of commands used previously (including other times XEmacs was used). 
(require `savehist)
(setq savehist-file "~/.emacs.d/history")
(setq savehist-length 1000)
(savehist-load)

;Saves the position the cursor was in a file before the file was closed.
(load-library "saveplace")
(setq save-place-file "~/.emacs.d/places")
(setq shadow-todo-file "~/.emacs.d/shadow-todo")

;; my toolbar with gnome icons
;(load-library "toolbar-my")

;;my abbreviations
(setq abbrev-file-name "~/.emacs.d/abbreviations-my.el")
(if (file-readable-p abbrev-file-name)
    (read-abbrev-file abbrev-file-name))

;; dictionary
(setq ispell-personal-dictionary "~/.emacs.d/ispell-dictionary")

;;some functions and global keys
(load-library "global-my")

;(load-file "~/.emacs.d/follow-mouse.el")
;(turn-on-follow-mouse); buggy with Mac Spaces

; to automatically remove annoying ^M from end of windows files. But this slows things down a bit
;;(add-hook 'find-file-hooks 'dos-to-unix) 

;(load-library "pc-select")
;(pc-select-mode)

;; switch buffers
;(load-library "wcy-swbuff")
; switch buffers
(iswitchb-mode 1)
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
(add-hook 'find-file-hooks 'auto-insert)
(setq auto-insert-directory "~/.emacs.d/")
(setq auto-insert-query nil)
(load-library "autoinsert-my")

; make the delete key work properly in tcl
;(add-hook 'tcl-mode-hook
          ;(local-set-key [(delete)] 'backward-or-forward-delete-char)
;          )

; no bell
(setq bell-volume 0)

;; ------------------- mode specific settings ---------

;; auto-mode-alist
(setq auto-mode-alist
      (append '(
		("\\.sh"  . shell-script-mode)
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
		("\\.\\(acumask\\)\\'" . cperl-mode)
		("\\.\\(param\\)\\'" . cperl-mode)
		("\\.m\\'" . matlab-mode)
		("muttrc\\'" . muttrc-mode)
		("^\/tmp\/mutt" . mutt-mode)
		) auto-mode-alist))



;; mutt mode.
 (autoload 'mutt-mode "~/.emacs.d/mutt.elc" "Major mode to compose mail" t)
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
(autoload 'matlab-mode "~/.emacs.d/matlab.el" "matlab major mode." t)
(add-hook 'matlab-mode-hook 
	  '(lambda()
	     ;;my personal stuff
	     (load-library "matlab-my")
	     ))
	     

;; muttrc mode.
(autoload 'muttrc-mode "muttrc-mode.elc"
  "Major mode to edit muttrc files" t)



;;TeX
;(require 'tex-site)
;(add-hook 'LaTeX-mode-hook 
;	  '(lambda()
;	     ;; LaTeX toolbar
;	     (load-library "oleg-latex-toolbar")
	     
;	     ;;my personal latex stuff
;	     (load-library "latex-my")
;	     ))

;;shell
(add-hook 'shell-script-mode-hook 
	  '(lambda()
	     (load-file "~/.emacs.d/shell-script-my.el")
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
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(scroll-bar-mode (quote right)))
