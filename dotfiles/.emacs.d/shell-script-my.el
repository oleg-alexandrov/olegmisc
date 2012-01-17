(defun reindent-then-newline-and-indent2 ()
  (interactive)
  (reindent-then-newline-and-indent)
  (indent-according-to-mode)
  (call-interactively 'indent-for-tab-command)
  )

(local-set-key [(return)] 'reindent-then-newline-and-indent2)
