;;emacs-lisp-my.el



(defun lisp-function ()
  (interactive)
  (insert "(defun  ()\n")
  (indent-for-tab-command)
  (insert "(interactive)\n")
  (indent-for-tab-command)
  (insert ")")
  (indent-for-tab-command)
  (previous-line 2)
  (forward-char 4)
  )

(defun  local-set-abb ()
  (interactive)
  (insert "(local-set-key [()])")
  (backward-char 3)
  )

(defun  global-set-abb ()
  (interactive)
  (insert "(global-set-key [()])")
  (backward-char 3)
  )


(defun smart-space ()
  (interactive)
  (if (not (expand-abbrev))
      (insert " ")
    )
  )

(local-set-key [(return)] 'reindent-then-newline-and-indent)
(local-set-key [(control x) (control l)] 'local-set-abb)
(local-set-key [(space)] 'smart-space)
;(local-set-key [(meta c)] 'comment-region)
