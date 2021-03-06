;(custom-set-variables
; '(require-final-newline nil)
; '(mas-exclude-file-regexp-list (quote ("^/tmp" "^*.shadow" "^.*/history" "^.*/places")))
; '(font-lock-auto-fontify t)
; '(cperl-continued-statement-offset 3)
; '(abbrev-mode t t)
; '(font-menu-ignore-scaled-fonts nil)
; '(ps-paper-type (quote letter))
; '(matlab-comment-region-s "%")
; '(save-place-version-control nil)
; '(temp-buffer-show-function (quote show-temp-buffer-in-current-frame))
; '(version-control t)
; '(hm--html-log-date-format "%B %d, %Y")
; '(toolbar-ispell-function (quote toolbar-ispell-internal))
; '(font-lock-maximum-decoration t)
; '(next-line-add-newlines nil)
; '(abbrev-all-caps nil)
; '(font-lock-mode-enable-list t)
; '(cperl-label-offset -4)
; '(mouse-avoidance-mode nil nil (avoid))
; '(scroll-step 2)
; '(pc-select-selection-keys-only t t)
; '(delete-key-deletes-forward t)
; '(hm--html-modified-end-tag "")
; '(load-home-init-file t t)
; '(cperl-electric-keywords t)
; '(buffers-menu-submenus-for-groups-p nil)
; '(save-place t nil (saveplace))
; '(save-abbrevs t)
; '(toolbar-cut-function (quote kill-primary-selection))
; '(font-lock-use-colors t)
; '(auto-save-list-file-prefix nil t)
; '(hm--html-modified-prefix "Updated: ")
; '(column-number-mode t)
; '(hm--html-automatic-created-comment nil)
; '(mas-file-history-save-filename "~/.emacs.d/.mas-file-history")
; '(shadow-files-to-copy nil t)
; '(default-toolbar-position (quote top))
; '(overwrite-mode nil)
; '(save-place-limit 1000)
; '(cperl-lazy-help-time (quote null))
; '(lazy-lock-mode nil nil (lazy-lock))
; '(xdvi-logfile "~/.emacs.d/.xdvi-log")
; '(recent-files-permanent-submenu t)
; '(hm--html-automatic-update-modified-line t)
; '(font-lock-use-fonts nil)
; '(cperl-invalid-face nil)
; '(hm--html-modified-start-tag "")
; '(debug-on-quit nil)
;; '(bar-cursor 2)
; '(make-backup-files nil)
; '(font-menu-this-frame-only-p nil)
; '(zmacs-regions t)
; '(ps-print-color-p t)
; '(case-replace t)
; '(toolbar-print-function (quote lpr-buffer))
; '(buffers-tab-max-size 10)
; '(buffers-menu-sort-function (quote sort-buffers-menu-by-mode-then-alphabetically))
; '(debug-on-error nil)
; '(toolbar-mail-reader (quote mutt))
; '(hm--html-automatic-changed-comment nil)
; '(TeX-default-mode (quote LaTeX-mode))
; '(truncate-lines nil)
; '(line-number-mode t)
; '(longlines-show-hard-newlines nil)
; '(hm--html-title-date-format "%B %d, %Y")
; '(cperl-min-label-indent 2)
; '(buffers-menu-grouping-function (quote group-buffers-menu-by-mode-then-alphabetically))
; '(kill-whole-line t)
; '(teach-extended-commands-p t)
; '(pc-select-meta-moves-sexps t t)
; '(cperl-comment-column 30)
; '(mouse-yank-at-point t)
; '(mouse-track-rectangle-p nil)
; '(teach-extended-commands-timeout 4)
; '(cperl-hairy nil)
; '(complex-buffers-menu-p nil)
;; '(blink-cursor-mode nil nil (blink-cursor))
; '(hm--html-automatic-create-modified-line t)
; '(font-lock-mode-disable-list nil)
; '(highlight-headers-highlight-citation-too t)
; '(user-mail-address "aoleg@math.umn.edu")
; '(query-user-mail-address nil)
; '(font-lock-maximum-size 2560000)
; '(get-frame-for-buffer-default-instance-limit nil)
; '(xdvi-bin "xdvi")
; '(cperl-under-as-char t)
; '(pc-selection-mode t t))

;;(custom-set-faces
;;; '(default ((t (:size "14pt" :family "Courier" :foreground "white" :background "black"))) t)
;; '(text-cursor ((t (:foreground "black" :background "green"))) t)
;; '(info-node ((t (:bold t))))
;; '(secondary-selection ((t (:foreground "white" :background "red"))) t)
;; '(widget-field-face ((((class grayscale color) (background light)) (:foreground "black" :background "white"))))
;; '(cperl-nonoverridable-face ((((class color) (background dark)) (:foreground "orange" :family "Courier" :bold t :italic nil))))
;; '(cperl-array-face ((((type x) (class color) (background light dark)) (:foreground "orangered" :bold t))))
;; '(mutt-header-keyword-face ((((class color) (background dark)) (:foreground "cyan" :bold nil))))
;; '(font-latex-title-1-face ((((class color) (background dark)) (:foreground "yellow" :family "helvetica" :bold t))))
;; '(font-lock-string-face ((t (:bold nil :foreground "green3"))))
;; '(dired-face-permissions ((t (:foreground "green"))))
;; '(font-lock-reference-face ((t (:foreground "green2"))))
;; '(flyspell-duplicate-face ((((class color)) (:foreground "OrangeRed"))))
;; '(message-headers ((t (:foreground "blue" :bold t))))
;; '(mutt-header-value-face ((((class color) (background dark)) (:foreground "indianred1"))))
;; '(custom-group-tag-face ((((class color) (background light)) (:underline t :foreground "blue"))))
;; '(cperl-hash-face ((((class color) (background dark)) (:foreground "Red" :background "black" :bold t :italic nil))))
;; '(font-lock-doc-string-face ((t (:foreground "green3"))))
;; '(mh-show-to-face ((((class color) (background light)) (:foreground "red"))))
;; '(font-lock-preprocessor-face ((t (:foreground "red" :family "adobe" :bold t :italic nil))))
;; '(viper-minibuffer-insert-face ((((class color)) (:foreground "white" :background "black"))))
;; '(font-lock-variable-name-face ((t (:foreground "yellow" :bold t))))
;; '(font-latex-bold-face ((((class color) (background light)) (:bold t))))
;; '(right-margin ((t (:bold nil :italic nil))) t)
;; '(flyspell-incorrect-face ((((class color)) (:foreground "OrangeRed"))))
;; '(custom-face-tag-face ((t (:bold nil :italic nil :underline t))))
;; '(green ((t (:foreground "green"))) t)
;; '(gnus-header-content-face ((((class color) (background light)) (:foreground "red" :italic t))))
;; '(custom-state-face ((((class color) (background light)) (:foreground "dark green"))))
;; '(font-lock-warning-face ((((class color) (background light)) (:foreground "Violetred" :bold t))))
;; '(font-lock-keyword-face ((t (:foreground "gold"))))
;; '(underline ((t nil)) t)
;; '(gnus-cite-face-7 ((((class color) (background light)) (:foreground "yellow"))))
;; '(font-lock-builtin-face ((((class color) (background dark)) (:foreground "red"))))
;; '(custom-saved-face ((t (:underline t :foreground "orange"))))
;; '(font-lock-constant-face ((t (:foreground "blue" :bold t))))
;; '(font-lock-type-face ((t (:foreground "#886fff" :bold t))))
;; '(message-cited-text ((t (:foreground "green"))))
;; '(bold ((t (:bold t))) t)
;; '(bold-italic ((t (:foreground "" :family "courier" :bold nil :italic t))) t)
;; '(font-latex-italic-face ((nil (:bold nil :italic nil))))
;; '(font-latex-math-face ((t (:bold nil :foreground "green3"))))
;; '(mutt-multiply-quoted-text-face ((((class color) (background dark)) (:foreground "gold" :italic nil))))
;; '(mutt-quoted-text-face ((((class color) (background dark)) (:foreground "green" :italic nil))))
;; '(custom-button-face ((t (:bold t :foreground "#3fdfcf"))))
;; '(message-separator-face ((((class color) (background light)) (:foreground "red"))))
;; '(list-mode-item-selected ((t (:foreground "green"))) t)
;; '(custom-variable-button-face ((t (:bold t :underline t :foreground "white"))))
;; '(message-header-contents ((t (:italic nil))))
;; '(hyper-apropos-documentation ((((class color) (background light)) (:foreground "red"))))
;; '(italic ((t (:family "courier" :italic t))) t)
;; '(font-wikipedia-bold-face (((:bold t))))
;; '(font-lock-comment-face ((t (:foreground "orange3"))))
;; '(font-latex-sedate-face ((((class color) (background light)) (:foreground "gold"))))
;; '(x-face ((t (:foreground "gold" :background "black"))))
;; '(font-lock-function-name-face ((t (:foreground "blue" :bold t))))
;; '(font-latex-title-3-face ((((class color) (background dark)) (:foreground "yellow" :family "helvetica" :bold t))))
;; '(isearch ((t (:foreground "red" :background "white"))) t)
;; '(highlight ((t (:foreground "red3" :background "white"))) t)
;; '(message-url ((t (:foreground "orange" :bold t))))
;; '(diff-context-face ((((class color) (background light)) (:foreground "yellow"))))
;; '(zmacs-region ((t (:background "RoyalBlue"))) t)
;; '(dired-face-boring ((((type x pm mswindows) (class color grayscale) (background light)) (:foreground "red"))))
;; '(font-latex-title-2-face ((((class color) (background dark)) (:foreground "yellow" :family "helvetica" :bold t))))
;; '(message-highlighted-header-contents ((t (:bold t)))))
