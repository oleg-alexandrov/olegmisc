; c++-my.el

; make trailing whitespace at the end of a line visible
(setq show-trailing-whitespace t)
(setq whitespace-style 'trailing)
;(add-hook 'before-save-hook 'whitespace-cleanup)

(flyspell-prog-mode)      ; turn on `flyspell-mode' for comments and strings
(c-toggle-hungry-state 1) ; delete space hungrily
(turn-on-pending-delete)  ; delete selected text when any character is touched
(imenu-add-menubar-index) ; make a menu listing all functions in the given file
(setq imenu-max-items 50)
(setq-default indent-tabs-mode nil)

(defun c++-indent-block ()
  (interactive)
  (search-backward "{")
  (c-indent-line-or-region)
  (next-line 1)
  (c-indent-line-or-region)
  (next-line 1)
  (c-indent-line-or-region)
  (search-backward ")")
  )

(defun c++-if ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (setq exp_ab (looking-at "[\t]*[ ]*$")))
  (if (not exp_ab)
      (insert "if ")
    (insert "if () {")
    (c-indent-line-or-region)
    (insert "\n")
    (c-indent-line-or-region)
    (insert "\n}")
    (c-indent-line-or-region)
    (previous-line 2)
    (search-forward "(")
    ))

(defun c++-else ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (setq exp_ab (looking-at "[\t]*[ ]*}[ ]*$")))
  (if (not exp_ab)
      (insert "else ")
    (insert "else{")
    (c-indent-line-or-region)
    (insert "\n")
    (c-indent-line-or-region)
    (insert "\n}")
    (c-indent-line-or-region)
    (previous-line 1)
    ))

(defun c++-brace ()
  (interactive)
    (insert "{")
    (c-indent-line-or-region)
    (insert "\n")
    (insert "\n}")
    (c-indent-line-or-region)
    (previous-line 1)
    (c-indent-line-or-region)
    )

(setq smart_equal_flag 0)
(setq smart_forward_flag 0)

(defun c++-for ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (setq empty (looking-at "[ ]*[\t]*[ ]*$")))
  (if (not empty)
      (insert "for ")
    (insert "for ( ; ; ) {")
    (c-indent-line-or-region)
    (insert "\n")
    (c-indent-line-or-region)
    (insert "\n}")
    (c-indent-line-or-region)
    (previous-line 2)
    (search-forward "(" nil t)
    (setq smart_equal_flag 1)
    ))

(defun smart-equal ()
  (interactive)
  (if (= smart_equal_flag 0)
      (insert "=")
    (insert " =")
    (forward-char 1)
    (save-excursion
      (backward-char 2)
      (backward-word 1)
      (if (looking-at "\\(\\w+\\)")
	  (let ((w (match-string 1) ))
	    (search-forward "; ")
	    (insert w)
	    (insert " < ")
	    (search-forward "; ")
	    (insert w)
	    (insert "++")
	    )
	)
      )
  ))

(defun smart-forward ()
  (interactive)
  (if (and (= smart_equal_flag 0) (= smart_equal_flag 0))
      (forward-char 1)
    )

  (if (not (= smart_equal_flag 1))
      ()
    (search-forward "< " nil t)
    (setq smart_equal_flag 0)
    )

  (if (not (= smart_forward_flag 1))
      ()
    (forward-char 1)
    (insert "<<  ")
    (backward-char 1)
    (setq smart_flag 0)
    )

  (setq smart_equal_flag 0)
  (setq smart_forward_flag 0)
  )

(defun c++-include ()
  (interactive)
  (insert "#include <>")
  (backward-char 1)
  )

(defun c++-include1 ()
  (interactive)
  (insert "#include \"\"")
  (backward-char 1)
  )

(defun c++-endl ()
  (interactive)
  (insert " << endl\;\n")
  )
(defun c++-smart-space ()
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
  (while (re-search-forward "^[ \t]*//" nil t)
    (progn (replace-match " ")
	   (c-indent-line-or-region)
	   ))
  (widen)
  ;(c-indent-line-or-region)
  ;(indent-relative)
  (call-interactively 'indent-region)
  (goto-char (mark))
  )

(defun insert-square-brackets ()
  (interactive)
  (insert "[]")
  (backward-char 1)
  )
(defun left-part-cpp ()
  (interactive)
  (end-of-line)
  (search-backward "(" nil t)
  (beginning-of-line)
  (setq lpoint (point))
  (search-forward ")" nil t)
  (end-of-line)
  (setq rpoint (point))
  (shell-command-on-region lpoint rpoint "~/.xemacs/left_part_cpp.pl" nil t)
  (c-indent-line-or-region)
  )

(defun right-part-cpp ()
  (interactive)
  (end-of-line)
  (search-backward "(" nil t)
  (beginning-of-line)
  (setq lpoint (point))
  (search-forward ")" nil t)
  (end-of-line)
  (setq rpoint (point))
  (shell-command-on-region lpoint rpoint "~/.xemacs/right_part_cpp.pl" nil t)
  (c-indent-line-or-region)
  )


(defun format-fun-dec ()
  (interactive)
  (end-of-line)
  (search-backward "(" nil t)
  (beginning-of-line)
  (setq lpoint (point))
  (search-forward ")" nil t)
  (end-of-line)
  (setq rpoint (point))
  (shell-command-on-region lpoint rpoint "~/.xemacs/format_fun_dec.pl" nil t)
  (c-indent-line-or-region)
  )

(defun comment-line ()
  "Comments the current line and goes to the next one"
  (interactive)
  (condition-case nil
      (comment-region (point-at-bol) (point-at-eol)) (error nil))
  (next-line 1))

(defun my-comment-region ()
  (interactive)
  (if (mark)
      (comment-region (point) (mark))
    (comment-line)
    )
  )

;; indent from Alex's style to my own style
(defun change-indent-style ()
  (interactive)
  (shell-command-on-region (point-min) (point-max) "~/bin/indent_style.pl" nil t)
  (indent-region (point-min) (point-max) nil)
  )

(defun matlab_C++_reorder_args ()
  (interactive)
  (shell-command-on-region (min (point) (mark)) (max (point (mark))) "~/.xemacs/matlab_C++_reorder_args.pl" nil t)
  )

(defun c++_to_matlab ()
  (interactive)
  (shell-command-on-region (min (point) (mark)) (max (point (mark))) "~/.xemacs/c++_to_matlab.pl" nil t)
  )

(defun mathematica-to-c ()
  (interactive)
  (shell-command-on-region (min (point) (mark)) (max (point (mark))) "mt2c.pl" nil t)
  (indent-region (point-min) (point-max) nil)
  )

(defun smart-down ()
  (interactive)
  (setq smart_equal_flag 0)
  (setq smart_forward_flag 0)
  (next-line 1)
  )


(defun swap-cc-h-aux (filename)
  (interactive)
  (cond (
	 (string-match "\\.cc" filename)
	 (setq filename (replace-match ".h" t t filename))
	 ) ; end of matching .cc

	(
	 (string-match "\\.h" filename)
	 (setq filename (replace-match ".cc" t t filename))
	 ); endp of matching .h

	); end cond

  filename ; return
  )


(defun swap-cc-h (file-to-swap)
  "If the currently open file ends in .cc, open instead the
corresponding .h file, and vice-versa. If the corresponding file does not
exist, try replacing 'src' with 'include' and vice-versa"
  (interactive)
  (if (string-match "\\(\\.cc\\|\\.h\\)$" file-to-swap)
      (let ((swapped-file (swap-cc-h-aux file-to-swap ) ))
	swapped-file ; attempt succeeded, return current file
	) ; end let
    file-to-swap ; return the input if failed
    ); end if
  ) ; end function

(defun toggle-cc-h ()
  (interactive)
  (find-file (swap-cc-h (buffer-file-name)))
  )

(defun update-header-file ()

  (interactive)

  (let ((cur-file (buffer-file-name) ))

    (if (string-match "\\.cpp$" cur-file)
	(let ((command (concat "~/bin/python/update_header.py " cur-file " "
			       (swap-cpp-h cur-file)) ))
	  (shell-command command)
	  )
      )
    )
  )

(defun c++-break-line ()
  (interactive)
  (insert "\" << \"")
  (backward-char 5)
  (newline-and-indent)
  )

(defun c++-delete ()
  (interactive)
  (if (mark)
      (backward-or-forward-delete-char 1)
    (c-hungry-delete-forward)
    )
  )

(local-set-key [(control x) (l)] 'duplicate-line)
(local-set-key [(=)] 'smart-equal)
;(local-set-key [(\<e)] 'c++-endl)
;(local-set-key [(control \[)] 'insert-square-brackets)
(local-set-key [(control i)] 'indent-region)
(local-set-key [(control t)] nil)
(local-set-key [(control x) (\4)] 'left-part-cpp)
(local-set-key [(control x) (\2)] 'right-part-cpp)
;(local-set-key [(control x) (\3)] 'format-fun-dec)
(local-set-key [(control y)] 'yank)

(local-set-key [(meta a)] 'define-mode-abbrev)
(local-set-key [(meta c)] 'my-comment-region)
(local-set-key [(meta q)] 'kill-this-buffer)
(local-set-key [(meta u)] 'my-uncomment-region)
(local-set-key [(right)] 'smart-forward)
(local-set-key [(super return)] 'smart-forward)
(local-set-key [(down)] 'smart-down)
(local-set-key [(space)] 'c++-smart-space)
(local-set-key [(return)] 'reindent-then-newline-and-indent)
(local-set-key [(meta j)] 'open-spq)
(local-set-key [(control s)] 'save-and-copy)
;(local-set-key [(meta \[)] 'c++-brace)
;(local-set-key "\e[7~" 'beginning-of-line)
;(local-set-key "\e[8~" 'end-of-line)
(local-set-key [(meta t)] 'toggle-cc-h)
(local-set-key [(meta h)] 'update-header-file)
(local-set-key [(control return)] 'c++-break-line)
(local-set-key [(meta \[)] 'goto-match-paren)
(local-set-key [(control meta a)] 'beginning-of-defun)
(local-set-key [(control meta e)] 'end-of-defun)
(local-set-key [(delete)] 'c++-delete)
(local-set-key [(meta \')] 'smart-forward)
