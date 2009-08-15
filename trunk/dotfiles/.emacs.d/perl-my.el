;;; perl-my.el --- 

(defun perl-print ()
  (insert "print \"\\n\"\;")
  (backward-char 4)
  )

(defun subst-globally ()
  (interactive)
  (insert "=~ s///g;")
  (backward-char 4)
  )

(defun tilde-subst-globally ()
  (interactive)
  (insert "=~ s///g;")
  (backward-char 4)
  )



(defun perl-if ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (setq exp_ab (looking-at "[\t]*[ ]*$")))
    (if (not exp_ab)
	(insert "if ")
      (insert "if (){")
      (cperl-indent-command)
      (insert "\n")
      (cperl-indent-command)
      (insert "\n}")
      (cperl-indent-command)
      (search-backward ")" nil t)
      ))

(defun perl-else ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (setq exp_ab (looking-at "[\t]*[ ]*}[ ]*$")))
  (if (not exp_ab)
      (insert "else ")
    (insert "else{")
    (cperl-indent-command)
    (insert "\n")
    (cperl-indent-command)
    (insert "\n}")
    (cperl-indent-command)
    (previous-line 1)
    ))


(defun perl-brace ()
  (interactive)
    (insert "{")
    (cperl-indent-command)
    (insert "\n")
    (insert "\n}")
    (cperl-indent-command)
    (previous-line 1)
    (cperl-indent-command)
    )


(defun perl-foreach ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (setq empty (looking-at "[ ]*[\t]*[ ]*$")))
  (if (not empty)
      (insert "foreach ")
    (insert "foreach  (){")
    (cperl-indent-command)
    (insert "\n")
    (cperl-indent-command)
    (insert "\n}")
    (cperl-indent-command)
    (previous-line 2)
    (search-forward "(" nil t)
    (backward-char 2)
    ))

(defun perl-for ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (setq empty (looking-at "[ ]*[\t]*[ ]*$")))
  (if (not empty)
      (insert "for ")
    (insert "for ( ; ; ){")
    (cperl-indent-command)
    (insert "\n")
    (cperl-indent-command)
    (insert "\n}")
    (cperl-indent-command)
    (previous-line 2)
    (search-forward "(" nil t)
    (setq for_flag 1)
    ))
(setq for_flag 0)
(defun new-for ()
  (interactive)
  (if (= for_flag 0)
      (insert "=")
    (beginning-of-line)
    (setq lpoint (point))
    (end-of-line)
    (setq rpoint (point))
    (shell-command-on-region lpoint rpoint "~/.emacs.d/for.pl" nil t)
    (cperl-indent-command)
    (beginning-of-line)
    (search-forward "=" nil t)
    (setq for_flag 2)
  ))

(defun smart-forward ()
  (interactive)
  (if (= for_flag 0)
      (forward-char 1)
    (search-forward "<" nil t)
    (insert " ")
    (setq for_flag 0)
))

(defun smart-space ()
  (interactive)
  (if (not (expand-abbrev))
      (insert " ")
    )
  )

(defun my-uncomment-region  ()
  "the default function does a lousy job"
  (interactive)
  (narrow-to-region (mark) (point))
  (goto-char (min (mark) (point)))
  (while (re-search-forward "^[ \t]*\#" nil t) (replace-match " "))
  (widen)
  (indent-relative)
  (call-interactively 'indent-region)
  (goto-char (mark))
  )

(defun insert-square-brackets ()
  (interactive)
  (insert "[]")
  (backward-char 1)
  )


; (local-set-key [(=)] 'new-for)
; (local-set-key [(\<e)] 'cperl-endl)
; (local-set-key [(control \[)] 'insert-square-brackets)
; (local-set-key [(control i)] 'indent-region)
; (local-set-key [(control t)] nil)
; (local-set-key [(control x) (\4)] 'left-part-cpp)
; (local-set-key [(control x) (\2)] 'right-part-cpp)
; (local-set-key [(control x) (\3)] 'format-fun-dec)
; (local-set-key [(control y)] 'yank)
; ;(local-set-key [(meta \[)] 'perl-brace)
; ;(local-set-key [(meta a)] 'define-mode-abbrev)
; ;(local-set-key [(meta c)] 'comment-region)
; ;(local-set-key [(meta q)] 'kill-this-buffer)
; (local-set-key [(meta u)] 'my-uncomment-region)
; (local-set-key [(right)] 'smart-forward)
; (local-set-key [(space)] 'smart-space)
; (local-set-key [(return)] 'reindent-then-newline-and-indent)
; (local-set-key [(meta \;)] 'insert-semicolon-and-newline)
(local-set-key (kbd "M-;") 'insert-semicolon-and-newline);
;;


