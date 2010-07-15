(custom-set-variables
 '(TeX-default-mode 'LaTeX-mode)
 '(abbrev-all-caps t)
 '(abbrev-mode t)
 '(auto-save-list-file-prefix nil t)
 '(bar-cursor 2)
 '(buffers-menu-grouping-function 'group-buffers-menu-by-mode-then-alphabetically)
 '(buffers-menu-sort-function 'sort-buffers-menu-by-mode-then-alphabetically)
 '(buffers-menu-submenus-for-groups-p nil)
 '(buffers-tab-max-size 10)
 '(case-replace t)
 '(column-number-mode t)
 '(complex-buffers-menu-p nil)
 '(cperl-comment-column 30)
 '(cperl-continued-statement-offset 3)
 '(cperl-electric-keywords t)
 '(cperl-hairy nil)
 '(cperl-invalid-face nil)
 '(cperl-label-offset -4)
 '(cperl-min-label-indent 2)
 '(cperl-under-as-char t)
 '(debug-on-error nil)
 '(debug-on-quit nil)
 '(default-toolbar-position 'top)
 '(delete-key-deletes-forward t)
 '(font-lock-auto-fontify t)
 '(font-lock-maximum-decoration t)
 '(font-lock-maximum-size 2560000)
 '(font-lock-mode-disable-list nil)
 '(font-lock-mode-enable-list t)
 '(font-lock-use-colors t)
 '(font-lock-use-fonts nil)
 '(font-menu-ignore-scaled-fonts nil t)
 '(font-menu-this-frame-only-p nil t)
 '(get-frame-for-buffer-default-instance-limit nil)
 '(highlight-headers-highlight-citation-too t)
 '(hm--html-automatic-changed-comment nil)
 '(hm--html-automatic-create-modified-line t)
 '(hm--html-automatic-created-comment nil)
 '(hm--html-automatic-update-modified-line t)
 '(hm--html-log-date-format "%B %d, %Y")
 '(hm--html-modified-end-tag "")
 '(hm--html-modified-prefix "Updated: ")
 '(hm--html-modified-start-tag "")
 '(hm--html-title-date-format "%B %d, %Y")
 '(kill-whole-line t)
 '(line-number-mode t)
 '(load-home-init-file t t)
 '(longlines-show-hard-newlines nil)
 '(make-backup-files nil)
 '(mas-exclude-file-regexp-list '("^/tmp" "^*.shadow" "^.*/history" "^.*/places")
)
 '(mas-file-history-save-filename "~/.xemacs/.mas-file-history")
 '(matlab-comment-region-s "%")
 '(mouse-track-rectangle-p nil)
 '(mouse-yank-at-point t)
 '(next-line-add-newlines nil)
 '(overwrite-mode nil)
 '(pc-select-meta-moves-sexps t t)
 '(pc-select-selection-keys-only t t)
 '(pc-selection-mode t t)
 '(ps-paper-type 'letter)
 '(ps-print-color-p t)
 '(query-user-mail-address nil)
 '(recent-files-permanent-submenu t)
 '(require-final-newline nil)
 '(save-abbrevs t)
 '(save-place-limit 1000)
 '(save-place-version-control nil)
 '(scroll-step 2)
 '(shadow-files-to-copy nil t)
 '(teach-extended-commands-p t)
 '(teach-extended-commands-timeout 4)
 '(temp-buffer-show-function 'show-temp-buffer-in-current-frame)
 '(toolbar-cut-function 'kill-primary-selection)
 '(toolbar-ispell-function 'toolbar-ispell-internal)
 '(toolbar-mail-reader 'mutt)
 '(toolbar-print-function 'lpr-buffer)
 '(toolbar-visible-p nil)
 '(truncate-lines nil)
 '(user-mail-address "aoleg@math.umn.edu")
 '(version-control t)
 '(xdvi-bin "xdvi")
 '(xdvi-logfile "~/.xemacs/.xdvi-log")
 '(zmacs-regions t))

(custom-set-faces
 '(default ((t (:foreground "white" :background "black" :size "14pt" :family "Luxi Mono"))) t)
 '(bold ((t (:bold t))) t)
 '(bold-italic ((t (:foreground "" :family "courier" :bold nil :italic t))) t)
 '(cperl-array-face ((((type x)) (:foreground "orangered" :bold t))))
 '(cperl-hash-face ((nil (:foreground "Red" :background "black" :bold t :italic nil))))
 '(cperl-nonoverridable-face ((nil (:foreground "orange" :bold t :italic nil))))
 '(custom-button-face ((t (:bold t :foreground "#3fdfcf"))))
 '(custom-face-tag-face ((t (:bold nil :italic nil :underline t))))
 '(custom-group-tag-face ((nil (:underline t :foreground "blue"))))
 '(custom-saved-face ((t (:underline t :foreground "orange"))))
 '(custom-state-face ((nil (:foreground "dark green"))))
 '(custom-variable-button-face ((t (:bold t :underline t :foreground "white"))))
 '(diff-context-face ((((class color) (background light)) (:foreground "yellow"))))
 '(dired-face-boring ((((type x pm mswindows) (class color grayscale) (background light)) (:foreground "red"))))
 '(dired-face-permissions ((t (:foreground "green"))))
 '(flyspell-duplicate-face ((((class color)) (:foreground "OrangeRed"))))
 '(flyspell-incorrect-face ((((class color)) (:foreground "OrangeRed"))))
 '(font-latex-bold-face ((((class color) (background light)) (:bold t))))
 '(font-latex-italic-face ((nil (:bold nil :italic nil))))
 '(font-latex-math-face ((t (:bold nil :foreground "green3"))))
 '(font-latex-sedate-face ((((class color) (background light)) (:foreground "gold"))))
 '(font-latex-title-1-face ((((class color) (background dark)) (:foreground "yellow" :bold t))))
 '(font-latex-title-2-face ((((class color) (background dark)) (:foreground "yellow" :bold t))))
 '(font-latex-title-3-face ((((class color) (background dark)) (:foreground "yellow" :bold t))))
 '(font-lock-builtin-face ((((class color) (background dark)) (:foreground "red"))))
 '(font-lock-comment-face ((t (:foreground "orange3"))))
 '(font-lock-constant-face ((t (:foreground "blue" :bold t))))
 '(font-lock-doc-string-face ((t (:foreground "green3"))))
 '(font-lock-function-name-face ((t (:foreground "blue" :bold t))))
 '(font-lock-keyword-face ((t (:foreground "gold"))))
 '(font-lock-preprocessor-face ((t (:foreground "red" :bold t :italic nil))))
 '(font-lock-reference-face ((t (:foreground "green2"))))
 '(font-lock-string-face ((t (:bold nil :foreground "green3"))))
 '(font-lock-type-face ((t (:foreground "#886fff" :bold t))))
 '(font-lock-variable-name-face ((t (:foreground "yellow" :bold t))))
 '(font-lock-warning-face ((((class color) (background light)) (:foreground "Violetred" :bold t))))
 '(font-wikipedia-bold-face (((:bold t))))
 '(gnus-cite-face-7 ((((class color) (background light)) (:foreground "yellow"))) t)
 '(gnus-header-content-face ((((class color) (background light)) (:foreground "red" :italic t))) t)
 '(green ((t (:foreground "green"))) t)
 '(highlight ((t (:foreground "red3" :background "white"))) t)
 '(hyper-apropos-documentation ((((class color) (background light)) (:foreground "red"))))
 '(info-node ((t (:bold t))))
 '(isearch ((t (:foreground "red" :background "white"))) t)
 '(italic ((t (:family "courier" :italic t))) t)
 '(list-mode-item-selected ((t (:foreground "green"))) t)
 '(message-cited-text ((t (:foreground "green"))))
 '(message-header-contents ((t (:italic nil))))
 '(message-headers ((t (:foreground "blue" :bold t))))
 '(message-highlighted-header-contents ((t (:bold t))))
 '(message-separator-face ((((class color) (background light)) (:foreground "red"))) t)
 '(message-url ((t (:foreground "orange" :bold t))))
 '(mh-show-to-face ((((class color) (background light)) (:foreground "red"))))
 '(modeline ((t (:foreground "dark slate gray" :background "#DBDBDB" :size "12"))) t)
 '(modeline-buffer-id ((t (:foreground "red3" :background "#DBDBDB" :size "20" :family "Luxi Mono" :bold t))) t)
 '(modeline-mousable ((t (:foreground "black" :background "#DBDBDB" :size "14" :family "Luxi Mono" :bold t))) t)
 '(mutt-header-keyword-face ((((class color) (background dark)) (:foreground "cyan" :bold nil))))
 '(mutt-header-value-face ((((class color) (background dark)) (:foreground "indianred1"))))
 '(mutt-multiply-quoted-text-face ((((class color) (background dark)) (:foreground "gold" :italic nil))))
 '(mutt-quoted-text-face ((((class color) (background dark)) (:foreground "green" :italic nil))))
 '(right-margin ((t (:bold nil :italic nil))) t)
 '(secondary-selection ((t (:foreground "white" :background "red"))) t)
 '(text-cursor ((t (:foreground "black" :background "green"))) t)
 '(underline ((t nil)) t)
 '(viper-minibuffer-insert-face ((((class color)) (:foreground "white" :background "black"))))
 '(widget-field-face ((((class grayscale color) (background light)) (:foreground "black" :background "white"))))
 '(x-face ((t (:foreground "gold"))))
 '(zmacs-region ((t (:background "RoyalBlue"))) t))