(turn-on-pending-delete); enable pending delete

(setq TeX-auto-local nil)
(auto-fill-mode 0)
(setq fill-column 72)

; usefull in LaTeX. This will not mess up with any paragraph starting with \begin and ending with \end
(defun my-fill-paragraph ()
  (interactive)
  (let ((lastpoint (point)))
	(re-search-backward "\\(\n[ ]*\n\\|\\$\\$\\|end\{\\)" nil t)
	(next-line 1)
	(beginning-of-line)
	(let ((beg (point)))
	  (re-search-forward "\\(\n[ ]*\n\\|\\$\\$\\|begin\{\\)" nil t)
	  (beginning-of-line)
	  (if (looking-at "\\begin\{")
	      (previous-line 1))
	  (fill-region beg (point))
	  (backward-char 3)
	  (if (not (looking-at "\\$\\$"))
	      ()
	    (forward-char 2)
	    (delete-char 1))
	(goto-char lastpoint)
	)))


(defun autosave-directory ()
  "~/.xemacs")

(defun latex-update-labels () 
  (interactive)  
  (let ((opoint (point)))
  (shell-command-on-region (point-min) (point-max) "~/bin/ref.pl" nil t)
  (goto-char (min opoint (point-max)))
  ))

(defun latex-put-matching-parens () ; make \bigg( .....\right) into \bigg(  ... \bigg)
  (interactive)
  (let ((npoint (point)))
    (backward-char 7)
    (let ((opoint (point)))
      (next-line 5)
      (end-of-line)
      (shell-command-on-region opoint (point) "~/bin/match_pars.pl" nil t)
      (goto-char npoint)
      )))

(defun latex-format-table () 
  (interactive)
  (let ((npoint (point)))
    (search-backward "tabular")
    (next-line 1)
    (beginning-of-line)
    (let ((opoint (point)))
    (search-forward "tabular")
    (beginning-of-line)
    (shell-command-on-region opoint (point) "~/bin/format_table.pl" nil t)
    (goto-char npoint)
    )))

(defun latex-insert-dollars ()
  (interactive)
  (insert "$$")
  (backward-char 1)
  )


(defun latex-insert-abs ()
  (interactive)
  (insert "||")
  (backward-char 1)
)


(defun latex-insert-set ()
  (interactive)
  (insert "\\{\\}")
  (backward-char 2)
  )

(defun latex-insert-norm ()
  (interactive)
  (insert "||||")
  (backward-char 2)
)

(defun latex-insert-double-dollars ()
  (interactive)
  (insert "\\begin{equation*}\n  \n\\end{equation*}")
  (previous-line 1)
)


(defun latex-frac ()
  (interactive)
  (insert "\\frac{}{}")
  (backward-char 3)
  )

(defun latex-list ()
  (interactive)
  (insert "\\begin{list}{}\n")
  (insert "\\item \n")
  (insert "\\end{list}\n")
  (previous-line 2)
  (forward-char 6)
  )

(defun latex-itemize ()
  (interactive)
  (insert "\\begin{itemize}{}\n")
  (insert "\\item \n")
  (insert "\\end{itemize}\n")
  (previous-line 2)
  (forward-char 6)
  )

(defun latex-enumerate ()
  (interactive)
  (insert "\\begin{enumerate}\n")
  (insert "\\item \n")
  (insert "\\end{enumerate}\n")
  (previous-line 2)
  (forward-char 6)
  )

(defun latex-cases ()
  (interactive)
  (insert "\\begin{cases}\n")
  (insert " \n")
  (insert "\\end{cases}")
  (previous-line 1)
  )


(defun latex-equation ()
  (interactive)
  (insert "\\begin{equation}\\label{}\n")
  (insert"  \n")
  (insert "\\end{equation}")
  (previous-line 2)
  (search-forward "\\label{")
  )


(defun latex-square ()
  (interactive)
  (insert "^2")
  )

(defun latex-theorem ()
  (interactive)
  (insert "\\begin{theorem}\\label{}\n\n\\end{theorem}")
  (previous-line 2)
  (end-of-line)
  (backward-char 1)
  )


(defun latex-corollary ()
  (interactive)
  (insert "\\begin{corollary}\\label{}\n\n\\end{corollary}")
  (previous-line 2)
  (end-of-line)
  (backward-char 1)
  )

(defun latex-lemma ()
  (interactive)
  (insert "\\begin{lemma}\\label{}\n\n\\end{lemma}")
  (previous-line 2)
  (end-of-line)
  (backward-char 1)
  )

(defun latex-proof ()
  (interactive)
  (insert "\\begin{proof}\n\n\\end{proof}")
  (previous-line 1)
  )

(defun latex-slide ()
  (interactive)
  (insert "\\begin{frame}[fragile]\\frametitle{}\n\n\\end{frame}\n")
  (previous-line 4)
  (search-forward "frametitle{")
  )

(defun latex-frame ()
  (interactive)
  (insert "\\begin{frame}[fragile]\\frametitle{}\n\n\\end{frame}\n")
  (previous-line 4)
  (search-forward "frametitle{")
  )

(defun latex-center ()
  (interactive)
  (insert "\\begin{center}\n\n\\end{center}")
  (previous-line 1)
  )


(defun latex-ref ()
  (interactive)
  (insert "\\eqref{}")
  (backward-char 1)
  )

(defun go-forward ()
  "move the cursor forward in a smart way"
  (interactive)
;  (if (not (looking-at "[A-Za-z]"));; if the next caracter is not a letter, then expand abbrev  
;      (expand-abbrev))
  (if (looking-at "\\(\}^\{\\|\}_\{\\|\\\\,d\\|\}\\\\!\\)") 
      (forward-char 3);; if matches '}^{' or '}_{' or '\,d' or '}\!' then go forward 3 chars
    (if (looking-at "\\(\}\{\\|\}\)\\)")  
	(forward-char 2);; else if matches }{ or })  then go forward 2 char
      (forward-char 1);; else go forward 1 char 
      )
    )
  )
(local-set-key [(right)] 'go-forward)

(defun latex-cite ()
  (interactive)
  (insert "\\cite{}")
  (backward-char 1)
  )

(defun latex-exp ()
  (interactive)
  (insert "^{}")
  (backward-char 1)   
  )

(defun latex-sub ()
  (interactive)
  (insert "_{}")
  (backward-char 1)   
  )

(defun latex-int-lim ()
  (interactive)
  (insert "\\int\\limits_{}^{}\\!\\,d")
  (backward-char 9)   
  )

(defun latex-int ()
  (interactive)
  (insert "\\int_{}^{}\\!\\,d")
  (backward-char 10)
  )

(defun latex-sum-lim ()
  (interactive )
  (insert "\\sum\\limits_{}^{}")
  (backward-char 4)
  )

(defun latex-sum ()
  (interactive )
  (insert "\\sum_{}^{}")
  (backward-char 4)
  )

(defun latex-bigg ()
  (interactive )
  (insert "\\bigg\\bigg")
  (backward-char 5)
  )

(defun latex-bigg-par ()
  (interactive )
  (insert "\\bigg(\\bigg)")
  (backward-char 6)
  )

(defun latex-comment-line ()
  (interactive)
  (let ((opoint (+ 1 (point))))
  (beginning-of-line)
  (insert "%")
  (goto-char (min opoint (point-max)))))

(defun latex-comment ()
  (interactive)
  (if (mark)
    (comment-region (point) (mark))
    (latex-comment-line)
    )
  )

(defun latex-uncomment ()
  (interactive)
  (comment-region (point) (mark) -1)
  )

(defun smart-space ()
  (interactive)
  (if (expand-abbrev)
      ()
    (insert " ")
    ;(do-auto-fill)
    )
  )

(defun pop-prev-yank ()
  (interactive)
  (yank-pop -1)
  )

(defun my-view-dvi ()
  (interactive)
  (TeX-command-menu "xdvi")
  )

(defun my-ispell ()
  (interactive)
  (ispell-region (point) (point-max))
  )

(defun latex-matrix ()
  (interactive)
  (insert "\\left(\n\\begin{array}{}\n\n\\end{array}\n\\right)")
  (previous-line 3)
  (search-forward "}{")
  )

(defun latex-vector ()
  (interactive)
  (insert "\\left[\n\\begin{array}{}\n\n\\end{array}\n\\right]")
  (previous-line 3)
  (search-forward "}{")
  )

(defun insert-ands-and-newlines ()
  "format a \begin{array}...\end{array} construct, by putting the \& and
newline characters where necessary"
  (interactive)
  (save-excursion
    (search-backward "array")
    (next-line 1)
    (beginning-of-line)
    (let ((beg (point)))
      (search-forward "array")
      (let ((end (point)))
	(narrow-to-region beg end)
	(beginning-of-buffer)
	(while (re-search-forward "[ ]*\n" nil t) (replace-match "\\\\\\\\\n"))
	(beginning-of-buffer)
	(while (re-search-forward "[ ]+" nil t) (replace-match " \& "))
	(widen)
	   )
      )
    )
   )


(defun save-and-run-latex-and-xdvi-jump-to-line ()
  "Compile a LaTex file"
  (interactive)
  (save-buffer (current-buffer))
  (TeX-command-menu "LaTeX")
)

(defun my-xdvi-jump-to-line ()
  (interactive)
  (call-interactively 'xdvi-jump-to-line)
  )

(defun insert-backslash ()
  (interactive)
  (insert "\\")
  )

(local-unset-key [(\")])


;;(flyspell-mode -1) ; turn this off

;(defun my-run-dvi ()
;  (interactive)
  
;  (shell-command (concat                   ; concatenate 
;                  "/home/user/myscript "   ; script name
;                  (file-name-nondirectory (buffer-file-name)) ; file name
;                  )  t)
;  )
;(local-set-key [(meta l) 'my-run-dvi])

(local-set-key [(control \4)] 'latex-insert-double-dollars)
(local-set-key [(control \\)] 'latex-insert-norm)
(local-set-key [(meta \\)] 'latex-insert-abs)
(local-set-key [(meta c)]  'TeX-comment-region); comments a selected region of text.
(local-set-key [(control j)]  'my-fill-paragraph)
(local-set-key [(meta \-)]  'latex-sub)
(local-set-key [(meta \2)] ' latex-square)
(local-set-key [(meta \4)]  'latex-insert-dollars)
(local-set-key [(control \[)] 'latex-insert-set)
(local-set-key [(meta \5)]  'latex-comment)
(local-set-key [(meta \6)]  'latex-exp)
(local-set-key [(meta \[)]  'TeX-insert-braces)
(local-set-key [(meta u)]  'latex-uncomment)
(local-set-key [(meta q)] 'kill-this-buffer)
(local-set-key [(meta r)] ' latex-update-labels)
(local-set-key [(meta t)] 'pop-prev-yank)
(local-set-key [(space)] 'smart-space)
(local-set-key [(tab)]  'indent-according-to-mode)
(local-set-key [(control meta a)] 'define-mode-abbrev)
(local-set-key [(control t)] 'my-ispell)
(local-set-key [(control h)] 'xdvi-jump-to-line)
(local-set-key [(control v)] 'yank)
;(local-set-key [(control s)] 'save-and-run-latex-and-xdvi-jump-to-line)
(local-set-key [(control s)] 'save-and-copy)
(local-set-key [(control x) (\4)] 'insert-ands-and-newlines)
(local-set-key [(control x) (u)] 'un-define-mode-abbrev)
(local-set-key [(control x) (p)] 'latex-put-matching-parens)

