; Paste this as 'toolbar-my.el' in '~/.xemacs', and put the icons in '~/.xemacs/icons'.
; This line needs to be inserted in .emacs: (load-file "~/.xemacs/toolbar.el-my")

; The contents of this file are based on the toolbar at
; http://www.linguistik.uni-erlangen.de/~mxp/xemacs-toolbar
; and on the latex toolbar at http://ee.usyd.edu.au/~thlai/emacs/index.html

; the location of icons
(setq my-toolbar-icon-directory (expand-file-name "~/.xemacs/icons/"))

; define the action of the 'redo' button
(defcustom toolbar-redo-function 'redo
 "*Function to call when the redo icon is selected."
  :type '(radio (function-item redo) (function :tag "Other")) :group 'toolbar)
(defun toolbar-redo ()
  (interactive)
  (call-interactively toolbar-redo-function))

; define the action of the 'search' button
(defcustom toolbar-search-function 'isearch-forward
  "*Function to call when the search icon is selected."
  :type '(radio (function-item search) (function :tag "Other")) :group 'toolbar)
(defun toolbar-search ()
  (interactive)
  (call-interactively toolbar-search-function))

; a function which will make a toolbar button with a given icon file name
(defun my-toolbar-make-button (file)
  (setq file (concat my-toolbar-icon-directory "file-cap-up.xpm"))
;  (setq file (expand-file-name file my-toolbar-icon-directory))
  (if (file-readable-p file)
      (toolbar-make-button-list file)
    (error "cannot find pixmap %s" file)))

(concat my-toolbar-icon-directory "file-cap-up.xpm")

;;  making the buttons
(defconst toolbar-file-icon       (toolbar-make-button-list  (concat my-toolbar-icon-directory "file-cap-up.xpm")))
(defconst toolbar-disk-icon       (toolbar-make-button-list  (concat my-toolbar-icon-directory "disk-cap-up.xpm")))
(defconst toolbar-printer-icon    (toolbar-make-button-list  (concat my-toolbar-icon-directory "print-cap-up.xpm")))
(defconst toolbar-cut-icon        (toolbar-make-button-list  (concat my-toolbar-icon-directory "cut-cap-up.xpm")))
(defconst toolbar-copy-icon       (toolbar-make-button-list  (concat my-toolbar-icon-directory "copy-cap-up.xpm")))
(defconst toolbar-paste-icon      (toolbar-make-button-list  (concat my-toolbar-icon-directory "paste-cap-up.xpm")))
(defconst toolbar-undo-icon       (toolbar-make-button-list  (concat my-toolbar-icon-directory "undo-cap-up.xpm")))
(defconst toolbar-redo-icon       (toolbar-make-button-list  (concat my-toolbar-icon-directory "redo-cap-up.xpm")))
(defconst toolbar-spell-icon      (toolbar-make-button-list  (concat my-toolbar-icon-directory "spell-cap-up.xpm")))
(defconst toolbar-replace-icon    (toolbar-make-button-list  (concat my-toolbar-icon-directory "replace-cap-up.xpm")))
(defconst toolbar-search-icon     (toolbar-make-button-list  (concat my-toolbar-icon-directory "search-cap-up.xpm")))
(defconst toolbar-mail-icon       (toolbar-make-button-list  (concat my-toolbar-icon-directory "mail-cap-up.xpm")))



;  making the buttons
;(defconst toolbar-file-icon      (my-toolbar-make-button "file-cap-up.xpm"))
;(defconst toolbar-disk-icon      (my-toolbar-make-button "disk-cap-up.xpm"))
;(defconst toolbar-printer-icon   (my-toolbar-make-button "print-cap-up.xpm"))
;(defconst toolbar-cut-icon       (my-toolbar-make-button "cut-cap-up.xpm"))
;(defconst toolbar-copy-icon      (my-toolbar-make-button "copy-cap-up.xpm"))
;(defconst toolbar-paste-icon     (my-toolbar-make-button "paste-cap-up.xpm"))
;(defconst toolbar-undo-icon      (my-toolbar-make-button "undo-cap-up.xpm"))
;(defconst toolbar-redo-icon      (my-toolbar-make-button "redo-cap-up.xpm"))
;(defconst toolbar-spell-icon     (my-toolbar-make-button "spell-cap-up.xpm"))
;(defconst toolbar-replace-icon   (my-toolbar-make-button "replace-cap-up.xpm"))
;(defconst toolbar-search-icon    (my-toolbar-make-button "search-cap-up.xpm"))
;(defconst toolbar-mail-icon      (my-toolbar-make-button "mail-cap-up.xpm"))


; defining the toolbar
(defconst my-toolbar'(
   [toolbar-file-icon        toolbar-open      t   "Open a file"]
;  [toolbar-folder-icon      toolbar-dired     t   "Edit a directory"]
   [toolbar-disk-icon        toolbar-save      t   "Save buffer"]
   [toolbar-printer-icon     toolbar-print     t   "Print document"]
   [toolbar-cut-icon         toolbar-cut       t   "Kill region"]
   [toolbar-copy-icon        toolbar-copy      t   "Copy region"]
   [toolbar-paste-icon       toolbar-paste     t   "Paste from clipboard"]
   [toolbar-undo-icon        toolbar-undo      t   "Undo edit"]
   [toolbar-redo-icon        toolbar-redo      t   "Redo edit"]
   [toolbar-spell-icon       toolbar-ispell    t   "Check spelling"]
;   [toolbar-search-icon      toolbar-search    t   "Search"]
   [toolbar-replace-icon     toolbar-replace   t   "Search & Replace"]
   [toolbar-mail-icon        toolbar-mail      t   "Read mail"]
;  [toolbar-info-icon        toolbar-info      t   "Info documentation"]
;  [toolbar-compile-icon     toolbar-compile   t   "Start a compilation"]
;  [toolbar-debug-icon       toolbar-debug     t   "Start a debugger"]
;  [toolbar-news-icon        toolbar-news      t   "Read news"]
  )
)
   
; installing the toolbar
(setq locale 'global)
(set-specifier top-toolbar (cons locale my-toolbar)); put the toolbar on top

