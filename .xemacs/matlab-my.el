;;(load-library "matlab")

(setq matlab-indent-function t)	;; if you want function bodies indented
(setq matlab-verify-on-save-flag nil) ;; turn off auto-verify on save
(setq matlab-indent-level 3)            ;; normal indent
(setq matlab-cont-level 3)              ;; continuation indent
(pending-delete-mode 1)

(auto-fill-mode -1)

(defun matlab-for ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (setq exp_ab (looking-at "[ ]*$")))
  (if (not exp_ab)
      (insert "for ")
    (insert "for ")
    (matlab-indent-line)
    (insert "\nend")
    (matlab-indent-line)
    (previous-line 1)
    (end-of-line)
    ))

(defun matlab-if ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (setq exp_ab (looking-at "[ ]*$")))
  (if (not exp_ab)
      (insert "if ")
    (insert "if ")
    (matlab-indent-line)
    (insert "\nend")
    (matlab-indent-line)
    (search-backward "if")
    (forward-char 3)
    ))

(defun smart-space ()
  (interactive)
  (if (expand-abbrev)
      ()
    (insert " ")
    )
  )

(defun my-comment ()
  (interactive)
  (if (mark)
      (matlab-comment-region (min (point) (mark)) (max (point) (mark)) nil)
    (save-excursion
      (beginning-of-line)
      (insert "%")
     )
    )
  )

(defun my-uncomment ()
  (interactive)
  (if (mark)
      (matlab-comment-region (min (point) (mark)) (max (point) (mark)) -1)
    (save-excursion
      (beginning-of-line)
      (if (looking-at "%")
	  (delete-char 1)
	)
      )
    )
  )

(defun align-comments ()
  (interactive)
  (if (mark)
  (shell-command-on-region (min (point) (mark)) (max (point (mark))) "~/bin/align_comments.pl" nil t)
  ))

(defun do-plot ()
  (interactive)
  (beginning-of-line)
  (search-forward "_")
  (backward-char 1)
  (insert "suff='")
  (end-of-line)
  (re-search-backward "[^ ]")
  (forward-char 1)
  (insert "'; do_plot('g', suff);")
  )

(defun matlab-insert-quotes ()
  (interactive)
  (insert "''")
  (backward-char 1)
  )
(local-set-key [(meta \')] 'matlab-insert-quotes)
(local-set-key [(meta z)] 'redo)
(local-set-key [(return)] 'reindent-then-newline-and-indent)
(local-set-key [(meta c)] 'my-comment)
(local-set-key [(meta u)] 'my-uncomment)
(local-set-key [(space)] 'smart-space)
(local-set-key [(control t)] nil)
(local-set-key [(control j)] 'fill-paragraph)
(local-set-key [(meta \2)] (lambda nil (interactive) (insert "^2")))
(local-set-key [(meta i)] 'do-plot)