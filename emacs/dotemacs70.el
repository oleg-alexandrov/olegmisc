; associate the files with the .tex extension with the LaTeX mode
(setq auto-mode-alist (append '(("\\.tex$"  . LaTeX-mode)) auto-mode-alist))

; load the LaTeX mode
(require 'tex-site)

; Tell XEmacs to load `my-latex.el' when opening LaTeX files
(add-hook 'LaTeX-mode-hook
  '(lambda()
     (load-file "~/.xemacs/my-latex.el")  ; load these LaTeX preferences
     ))

