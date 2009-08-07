;;init.el

; Point to the directory(ies) where all other files to be loaded are located.
(setq load-path (cons (expand-file-name "~/.emacs.d/") load-path))

(fset 'yes-or-no-p 'y-or-n-p)  ;; Make all "yes or no" prompts show "y or n" instead

(setq make-backup-files nil) 

;;start gnuserv.
;(gnuserv-start)(setq make-backup-files nil) 

;; The very nifty ido package, that allows one to switch to a buffer by typing
;; only a few consecutive characters of its name.
;; It does not work well with multiple windows though.
;(load-library "ido")
;(ido-mode t)

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

; to automatically remove annoying ^M from end of windows files. But this slows things down a bit
;;(add-hook 'find-file-hooks 'dos-to-unix) 

;(load-library "pc-select")
;(pc-select-mode)

;; switch buffers
;(load-library "wcy-swbuff")

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

;;to enable control-x to copy, control v to paste,  etc. 
;(load-library "cua-mode")
;(CUA-mode 1)

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
(autoload 'matlab-mode "~/.emacs.d/matlab.elc" "matlab major mode." t)
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
 '(cua-mode t nil (cua-base))
 '(scroll-bar-mode (quote right)))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 123 :width normal :foundry "b&h" :family "Luxi Mono"))))
 '(cperl-array-face ((((type x) (class color) (background light dark)) (:foreground "orangered" :bold t))))
 '(cperl-hash-face ((((class color) (background dark)) (:foreground "Red" :background "black" :bold t :italic nil))))
 '(cperl-nonoverridable-face ((((class color) (background dark)) (:foreground "orange" :family "Courier" :bold t :italic nil))))
 '(custom-button-face ((t (:bold t :foreground "#3fdfcf"))) t)
 '(custom-face-tag-face ((t (:bold nil :italic nil :underline t))) t)
 '(custom-group-tag-face ((((class color) (background light)) (:underline t :foreground "blue"))) t)
 '(custom-saved-face ((t (:underline t :foreground "orange"))) t)
 '(custom-state-face ((((class color) (background light)) (:foreground "dark green"))) t)
 '(custom-variable-button-face ((t (:bold t :underline t :foreground "white"))) t)
 '(diff-context-face ((((class color) (background light)) (:foreground "yellow"))))
 '(dired-face-boring ((((type x pm mswindows) (class color grayscale) (background light)) (:foreground "red"))))
 '(dired-face-permissions ((t (:foreground "green"))))
 '(flyspell-duplicate-face ((((class color)) (:foreground "OrangeRed"))))
 '(flyspell-incorrect-face ((((class color)) (:foreground "OrangeRed"))))
 '(font-latex-bold-face ((((class color) (background light)) (:bold t))))
 '(font-latex-italic-face ((nil (:bold nil :italic nil))))
 '(font-latex-math-face ((t (:bold nil :foreground "green3"))))
 '(font-latex-sedate-face ((((class color) (background light)) (:foreground "gold"))))
 '(font-latex-title-1-face ((((class color) (background dark)) (:foreground "yellow" :family "helvetica" :bold t))))
 '(font-latex-title-2-face ((((class color) (background dark)) (:foreground "yellow" :family "helvetica" :bold t))))
 '(font-latex-title-3-face ((((class color) (background dark)) (:foreground "yellow" :family "helvetica" :bold t))))
 '(font-lock-builtin-face ((((class color) (background dark)) (:foreground "red"))))
 '(font-lock-comment-face ((t (:foreground "orange3"))))
 '(font-lock-constant-face ((t (:foreground "green" :weight bold))))
 '(font-lock-doc-string-face ((t (:foreground "green3"))))
 '(font-lock-function-name-face ((t (:foreground "blue" :bold t))))
 '(font-lock-keyword-face ((t (:foreground "gold"))))
 '(font-lock-preprocessor-face ((t (:foreground "red" :family "adobe" :bold t :italic nil))))
 '(font-lock-reference-face ((t (:foreground "green2"))))
 '(font-lock-string-face ((t (:bold nil :foreground "green3"))))
 '(font-lock-type-face ((t (:foreground "#886fff" :bold t))))
 '(font-lock-variable-name-face ((t (:foreground "yellow" :bold t))))
 '(font-lock-warning-face ((((class color) (background light)) (:foreground "Violetred" :bold t))))
 '(font-wikipedia-bold-face (((:bold t))))
 '(gnus-cite-face-7 ((((class color) (background light)) (:foreground "yellow"))))
 '(gnus-header-content-face ((((class color) (background light)) (:foreground "red" :italic t))))
 '(green ((t (:foreground "green"))) t)
 '(highlight ((t (:foreground "red3" :background "white"))))
 '(hyper-apropos-documentation ((((class color) (background light)) (:foreground "red"))))
 '(info-node ((t (:bold t))))
 '(isearch ((t (:foreground "red" :background "white"))))
 '(list-mode-item-selected ((t (:foreground "green"))) t)
 '(message-cited-text ((t (:foreground "green"))))
 '(message-header-contents ((t (:italic nil))))
 '(message-headers ((t (:foreground "blue" :bold t))))
 '(message-highlighted-header-contents ((t (:bold t))))
 '(message-separator-face ((((class color) (background light)) (:foreground "red"))))
 '(message-url ((t (:foreground "orange" :bold t))))
 '(mh-show-to-face ((((class color) (background light)) (:foreground "red"))))
 '(mutt-header-keyword-face ((((class color) (background dark)) (:foreground "cyan" :bold nil))))
 '(mutt-header-value-face ((((class color) (background dark)) (:foreground "indianred1"))))
 '(mutt-multiply-quoted-text-face ((((class color) (background dark)) (:foreground "gold" :italic nil))))
 '(mutt-quoted-text-face ((((class color) (background dark)) (:foreground "green" :italic nil))))
 '(right-margin ((t (:bold nil :italic nil))) t)
 '(secondary-selection ((t (:foreground "white" :background "red"))))
 '(text-cursor ((t (:foreground "black" :background "green"))) t)
 '(underline ((t nil)))
 '(viper-minibuffer-insert-face ((((class color)) (:foreground "white" :background "black"))))
 '(widget-field-face ((((class grayscale color) (background light)) (:foreground "black" :background "white"))) t)
 '(x-face ((t (:foreground "gold" :background "black"))))
 '(zmacs-region ((t (:background "RoyalBlue"))) t))
