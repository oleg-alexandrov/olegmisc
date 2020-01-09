; this needs to be pasted in the directory .xemacs as my-latex.el

; First let us make the space key expand the abbreviations
(defun smart-space () ; make the space key behave in a smarter way
  (interactive)
  (if (not (expand-abbrev)) ; test if the current word is an abbrev. If yes, expand it.
      (insert " ")          ; If not, insert a plain space
    )                       ; Now you know how to use 'if' in emacs lisp
  )
(local-set-key [(space)] 'smart-space) ; bind the 'smart-space' function to the 'space' key



; Let us boldly define lisp functions to make XEmacs do our will

(defun my-latex-frac ()  ; define a function to insert \frac{}{} and move the cursor back 3 chars
  (interactive)          ; every emacs lisp function must have this line
  (insert "\\frac{}{}")  ; do the job, insert a piece of text
  (backward-char 3)      ; move back 3 chars, that is, inside the first pair of braces
  )


(defun my-latex-equation ()      ; a function to insert the equation environment
  (interactive)                 
  (insert "\\begin{equation}\\label{}\n") ; \n is the newline character
  (insert "  \n")                         ; an empty line (with two spaces on it)
  (insert "\\end{equation}")
  (previous-line 3)                       ; move back to the line containing \begin...
  (forward-char 24)                       ; move inside the curly brackets
  )


; Below are some other functions I find useful. To each of them there is
; a corresponding line in  my-abbreviations.el  calling that function

(defun my-latex-list ()
  (interactive)
  (insert "\\begin{list}{}\n")
  (insert "\\item \n")
  (insert "\\end{list}\n")
  (previous-line 2)
  (forward-char 6)
  )

(defun my-latex-enumerate ()
  (interactive)
  (insert "\\begin{enumerate}\n")
  (insert "\\item \n")
  (insert "\\end{enumerate}\n")
  (previous-line 2)
  (forward-char 6)
  )


(defun my-latex-theorem ()
  (interactive)
  (insert "\\begin{theorem}\\label{}\n\n\\end{theorem}")
  (previous-line 2)
  (end-of-line)
  (backward-char 1)
  )


(defun my-latex-corollary ()
  (interactive)
  (insert "\\begin{corollary}\\label{}\n\n\\end{corollary}")
  (previous-line 2)
  (end-of-line)
  (backward-char 1)
  )

(defun my-latex-lemma ()
  (interactive)
  (insert "\\begin{lemma}\\label{}\n\n\\end{lemma}")
  (previous-line 2)
  (end-of-line)
  (backward-char 1)
  )

(defun my-latex-proof ()
  (interactive)
  (insert "\\begin{proof}\n\n\\end{proof}")
  (previous-line 1)
  )


(defun my-latex-center ()
  (interactive)
  (insert "\\begin{center}\n\n\\end{center}")
  (previous-line 1)
  )

(defun my-latex-ref ()
  (interactive)
  (insert "(\\ref{})")
  (backward-char 2)
  )

(defun my-latex-int-lim ()
  (interactive)
  (insert "\\int\\limits_{}^{}\\!\\,d")
  (backward-char 9)  
  )

(defun my-latex-int ()
  (interactive)
  (insert "\\int_{}^{}\\!\\,d")
  (backward-char 10)
  )

(defun my-latex-sum-lim ()
  (interactive )
  (insert "\\sum\\limits_{}^{}")
  (backward-char 4)
  )

(defun my-latex-sum ()
  (interactive )
  (insert "\\sum_{}^{}")
  (backward-char 4)
  )

; Lastly, let us define two more useful shortcuts (which were described in the LaTeX tip)

(local-set-key [(meta a)] 'define-mode-abbrev) ; define abbrevs on the fly with Alt-a
(local-set-key [(meta space)] 'dabbrev-expand) ; expand dinamic abbreviations with Alt-space