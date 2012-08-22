(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(aquamacs-additional-fontsets nil t)
 '(aquamacs-customization-version-id 215 t)
 '(aquamacs-tool-bar-user-customization nil t)
 '(blink-cursor-mode nil)
 '(cua-mode t nil (cua-base))
 '(default-frame-alist (quote ((tool-bar-lines . 0) (menu-bar-lines . 1) (cursor-color . "gold") (cursor-type . bar) (vertical-scroll-bars . right))))
 '(gud-gdb-command-name "gdb --annotate=1")
 '(large-file-warning-threshold nil)
 '(ns-tool-bar-display-mode nil t)
 '(ns-tool-bar-size-mode nil t)
 '(scroll-bar-mode (quote right))
 '(visual-line-mode nil t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(Custom-mode-default ((t (:inherit autoface-default :height 140 :family "Menlo"))) t)
 '(autoface-default ((((type ns)) (:inherit default :background "black" :foreground "white" :height 140 :family "Menlo" :width normal))))
 '(bold-italic ((t (:slant italic :weight bold))))
 '(cperl-array-face ((((type x) (class color) (background light dark)) (:foreground "orangered" :bold t))))
 '(cperl-hash-face ((((class color) (background dark)) (:foreground "Red" :background "black" :bold t :italic nil))))
 '(cperl-nonoverridable-face ((((class color) (background dark)) (:foreground "orange" :bold t :italic nil))))
 '(cursor ((default (:width condensed)) (nil nil)))
 '(custom-button ((t (:bold t :foreground "#3fdfcf"))))
 '(custom-face-tag ((t (:bold nil :italic nil :underline t))))
 '(custom-group-tag ((((class color) (background light)) (:underline t :foreground "blue"))))
 '(custom-mode-default ((t (:inherit autoface-default :height 140 :family "Menlo"))) t)
 '(custom-saved ((t (:underline t :foreground "orange"))))
 '(custom-state ((((class color) (background light)) (:foreground "dark green"))))
 '(custom-variable-button ((t (:bold t :underline t :foreground "white"))))
 '(diff-context-face ((((class color) (background light)) (:foreground "yellow"))))
 '(dired-face-boring ((((type x pm mswindows) (class color grayscale) (background light)) (:foreground "red"))))
 '(dired-face-permissions ((t (:foreground "green"))))
 '(flyspell-duplicate-face ((((class color)) (:foreground "OrangeRed"))) t)
 '(flyspell-incorrect-face ((((class color)) (:foreground "OrangeRed"))) t)
 '(font-latex-bold-face ((((class color) (background light)) (:bold t))))
 '(font-latex-italic-face ((nil (:bold nil :italic nil))))
 '(font-latex-math-face ((t (:bold nil :foreground "green3"))))
 '(font-latex-sedate-face ((((class color) (background light)) (:foreground "gold"))))
 '(font-latex-title-1-face ((((class color) (background dark)) (:foreground "yellow" :bold t))))
 '(font-latex-title-2-face ((((class color) (background dark)) (:foreground "yellow" :bold t))))
 '(font-latex-title-3-face ((((class color) (background dark)) (:foreground "yellow" :bold t))))
 '(font-lock-builtin-face ((((class color) (background dark)) (:foreground "red"))))
 '(font-lock-comment-face ((t (:foreground "orange3"))))
 '(font-lock-constant-face ((t (:foreground "green" :weight bold))))
 '(font-lock-doc-string-face ((t (:foreground "green3"))))
 '(font-lock-function-name-face ((t (:foreground "blue" :bold t))))
 '(font-lock-keyword-face ((t (:foreground "gold"))))
 '(font-lock-preprocessor-face ((t (:foreground "red" :slant normal :weight bold))))
 '(font-lock-reference-face ((t (:foreground "green2"))))
 '(font-lock-string-face ((t (:bold nil :foreground "green3"))))
 '(font-lock-type-face ((t (:foreground "#886fff" :bold t))))
 '(font-lock-variable-name-face ((t (:foreground "yellow" :bold t))))
 '(font-lock-warning-face ((((class color) (background light)) (:foreground "Violetred" :bold t))))
 '(font-wikipedia-bold-face (((:bold t))))
 '(gnus-cite-face-7 ((((class color) (background light)) (:foreground "yellow"))))
 '(gnus-header-content-face ((((class color) (background light)) (:foreground "red" :italic t))))
 '(green ((t (:foreground "green"))) t)
 '(highlight ((t (:foreground "red3"))))
 '(hyper-apropos-documentation ((((class color) (background light)) (:foreground "red"))))
 '(info-node ((t (:bold t))))
 '(isearch ((t (:foreground "red"))))
 '(lazy-highlight ((((class color) (min-colors 88) (background light)) (:foreground "red"))))
 '(list-mode-item-selected ((t (:foreground "green"))) t)
 '(message-cited-text ((t (:foreground "green"))))
 '(message-header-contents ((t (:italic nil))))
 '(message-headers ((t (:foreground "blue" :bold t))))
 '(message-highlighted-header-contents ((t (:bold t))))
 '(message-separator-face ((((class color) (background light)) (:foreground "red"))) t)
 '(message-url ((t (:foreground "orange" :bold t))))
 '(mh-show-to-face ((((class color) (background light)) (:foreground "red"))))
 '(minibuffer-prompt ((t (:foreground "royal blue"))))
 '(mutt-header-keyword-face ((((class color) (background dark)) (:foreground "cyan" :bold nil))))
 '(mutt-header-value-face ((((class color) (background dark)) (:foreground "indianred1"))))
 '(mutt-multiply-quoted-text-face ((((class color) (background dark)) (:foreground "gold" :italic nil))))
 '(mutt-quoted-text-face ((((class color) (background dark)) (:foreground "green" :italic nil))))
 '(region ((nil (:background "royalblue"))))
 '(right-margin ((t (:bold nil :italic nil))) t)
 '(secondary-selection ((t (:foreground "white" :background "red"))))
 '(text-cursor ((t (:foreground "black" :background "green"))) t)
 '(underline ((t nil)))
 '(viper-minibuffer-insert-face ((((class color)) (:foreground "white" :background "black"))))
 '(widget-field ((((class grayscale color) (background light)) (:foreground "black" :background "white"))))
 '(x-face ((t (:foreground "gold" :background "black"))))
 '(zmacs-region ((t (:background "RoyalBlue"))) t))

(set-face-background 'show-paren-match-face (face-background 'custom-mode-default))
(set-face-foreground 'show-paren-match-face "red")