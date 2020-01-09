;; init.el - the XEmacs configuration file. This will load some other files
;; from the .xemacs directory. Starting with version XEmacs 21.4, one
;; can use the file .xemacs/init.el as a start-up file instead of init.el

(custom-set-variables               ; only one entry with this name must exist in custom.el
 '(delete-key-deletes-forward t)    ; make the delete key delete the next character (as expected)
 '(line-number-mode t)              ; show the line and column number of the cursor in the ...
 '(column-number-mode t)            ; ... lower part of the screen (very useful)
 '(make-backup-files nil)           ; do not make back-up files (they just trash the directories)
 '(visible-bell t)                  ; flash the screen instead of beeping (which can be annoying)
 '(mouse-yank-at-point t)           ; paste where the cursor is, and not where the mouse clicks in
 '(user-mail-address "my@email" t)  ; your e-mail address (otherwise XEmacs will nag)
 '(query-user-mail-address nil)     ; don't ask what my e-mail address is all the time
)                                   ; don't miss this closing parenthesis!


; Saves a history of files opened previously (including other times XEmacs was used - very useful)
(require `savehist)
(setq savehist-file "~/.xemacs/history")
(setq savehist-length 1000)
(savehist-load)

; Saves the position the cursor was in a file before the file was closed.
(load-library "saveplace")
(setq save-place-file "~/.xemacs/places")
(setq shadow-todo-file "~/.xemacs/shadow-todo")

(load-file "~/.xemacs/cua-mode.el")  ; load cua-mode every time XEmacs is started
(CUA-mode 1) ; run cua-mode (a package to enable MS Windows type keyboard shortcuts)

(load-library "pc-select")    ; a package which enables text selection ...
(pc-select-mode)              ; ... with the shift and arrow keys

(load-file "~/.xemacs/my-shortcuts.el")  ; will load this file every time XEmacs is started



; associate the files with the .tex extension with the LaTeX mode
(setq auto-mode-alist (append '(("\\.tex$"  . LaTeX-mode)) auto-mode-alist))

; load the LaTeX mode
(require 'tex-site)

; Tell XEmacs to load `my-latex.el' when opening LaTeX files
(add-hook 'LaTeX-mode-hook
  '(lambda()
     (load-file "~/.xemacs/my-latex.el")  ; load these LaTeX preferences
     ))

(setq-default abbrev-mode t)                            ; enable abbreviations
(setq save-abbrevs t)                                   ; save abbreviations upon exiting xemacs
(setq abbrev-file-name "~/.xemacs/my-abbreviations.el") ; the file storing the abbreviations
(if (file-readable-p abbrev-file-name)                  ; read the abbreviations every
  (read-abbrev-file abbrev-file-name)                   ; time xemacs is started
  )

(load-file "~/.xemacs/my-toolbar.el"); load the toolbar

(require 'font-lock) ; enable syntax highlighting

; Syntax highlighting. Dark background.
; Insert this in custom.el. Then restart XEmacs.

(custom-set-faces ; only one 'custom-set-faces' entry must exist in custom.el
 '(default ((t (:foreground "white" :background "black" :size "12" :family "courier"))) t)
 '(cperl-array-face ((t (:foreground "orangered" :bold t))))
 '(cperl-hash-face ((t (:foreground "Red" :bold t))))
 '(cperl-nonoverridable-face ((t (:foreground "orange" :bold t))))
 '(custom-button-face ((t (:bold t :foreground "#3fdfcf"))))
 '(custom-group-tag-face ((t (:underline t :foreground "blue"))))
 '(custom-saved-face ((t (:underline t :foreground "orange"))))
 '(custom-state-face ((t (:foreground "green3"))))
 '(custom-variable-button-face ((t (:bold t :underline t :foreground "white"))))
 '(dired-face-permissions ((t (:foreground "green"))))
 '(font-latex-bold-face ((((class color) (background light)) (:bold t))))
 '(font-latex-italic-face ((((class color) (background light)) (:italic t))))
 '(font-latex-math-face ((((class color) (background light)) (:foreground "green3"))))
 '(font-latex-sedate-face ((((class color) (background light)) (:foreground "gold"))))
 '(font-lock-comment-face ((t (:foreground "orange3"))))
 '(font-lock-doc-string-face ((t (:foreground "Wheat3"))))
 '(font-lock-function-name-face ((t (:foreground "blue" :bold t))))
 '(font-lock-keyword-face ((t (:foreground "gold"))))
 '(font-lock-preprocessor-face ((t (:foreground "red" :bold t))))
 '(font-lock-reference-face ((t (:foreground "orangered"))))
 '(font-lock-string-face ((t (:foreground "green3"))))
 '(font-lock-type-face ((t (:foreground "#886fff" :bold t))))
 '(font-lock-variable-name-face ((t (:foreground "yellow" :bold t))))
 '(font-lock-warning-face ((t (:foreground "Violetred"  :bold t))))
 '(highlight ((t (:foreground "red3" :background "white"))) t)
 '(isearch ((t (:foreground "red" :background "white"))) t)
 '(list-mode-item-selected ((t (:foreground "green"))) t)
 '(message-cited-text ((t (:bold t :italic nil))))
 '(secondary-selection ((t (:foreground "white" :background "red"))) t)
 '(text-cursor ((t (:foreground "black" :background "green"))) t)
 '(zmacs-region ((t (:background "RoyalBlue"))) t)
 )

