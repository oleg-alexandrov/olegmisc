(defun shell-space ()
  (interactive)
  (if (expand-abbrev)
      ()
    (insert " ")
    )
  )


(local-set-key [(return)] 'reindent-then-newline-and-indent)
(local-set-key [(space)] 'shell-space)
(turn-on-pending-delete)
