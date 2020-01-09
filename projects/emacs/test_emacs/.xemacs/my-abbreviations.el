; my-abbreviations.el - a file to store abbreviations tables. So far
; it has only a latex-mode abbreviation table, but it can contain
; tables for many other languages (html, C++, etc).  Note that XEmacs
; sometimes overwrites this file and then the nice order as now is
; lost.


; Define the table of abbreviations in latex-mode.  Note that we have
; to use '\\' to stand for backslash, since, just like in LaTeX, in
; emacs lisp the backslash character is special.  It would have been
; logical for this table to be called latex-mode-abbrev-table, but for
; some reason that does not work.
(define-abbrev-table 'text-mode-abbrev-table '(

  ; abbreviations to fix typos (i.e, to correct things like 'teh' into 'the')
    ("teh"  "the "  nil 0)
    ("hte"  "the "  nil 0)
    ("htat" "that " nil 0)

   ; abbreviations to long words
    ("abb"      "abbreviation "    nil 0)
    ("approx"   "approximation "   nil 0)
    ("contr"    "contradiction "   nil 0)
    ("cont"     "continuously "    nil 0)
    ("neib"     "neighborhood "    nil 0)
    ("\\lra"    "\\leftrightarrow" nil 0)
   
   ; abbreviations to long letters in the Green alphabet
    ("\\a" "\\alpha" nil 0)
    ("\\b" "\\beta" nil 0)
    ("\\g" "\\gamma" nil 0)
    ("\\d" "\\delta" nil 0)
    ("\\e" "\\epsilon" nil 0)
    ("\\ve" "\\varepsilon" nil 0)
    ("\\t" "\\theta" nil 0)
    ("\\vt" "\\vartheta" nil 0)
    ("\\k" "\\kappa" nil 0)
    ("\\l" "\\lambda" nil 0)
    ("\\vp" "\\varphi" nil 0)
    ("\\vs" "\\varpsi" nil 0)
    ("\\s" "\\sigma" nil 0)
    ("\\o" "\\omega" nil 0)

   ; abbreviations to mathbb R, etc
    ("\\c" "\\mathbb C" nil 0)
    ("\\q" "\\mathbb Q" nil 0)
    ("\\r" "\\mathbb R" nil 0)
    ("\\z" "\\mathbb Z" nil 0)

  ; abbreviations involving going back one character
    ("\\bb" "{\\bf }" backward-char 0)
    ("\\ee" "\\emph{}" backward-char 0)
    ("\\ii" "{\\it }" backward-char 0)
    ("\\ll" "\\label{}" backward-char 0)
    ("\\mm" "{\\mathbf }" backward-char 0)
    ("\\ss" "\\section{}" backward-char 0)
   ; ------------------------------------
    ("\\bi" "\\bibitem{}" backward-char 0)
    ("\\ci" "\\cite{}" backward-char 0)
    ("\\lm" "\\lim\\limits_{}" backward-char 0)
    ("\\sq" "\\sqrt{}" backward-char 0)
    ("\\sb" "\\subsection{}" backward-char 0)
    ("\\ssb" "\\subsubsection{}" backward-char 0)

   ; abbreviations calling functions (which are defined in my-latex.el)
    ("\\co" "" my-latex-corollary 0)
    ("\\lm" "" my-latex-lemma 0)
    ("\\pr" "" my-latex-proof 0)
    ("\\th" "" my-latex-theorem 0)

    ("\\eq" "" my-latex-equation 0)
    ("\\ct" "" my-latex-center 0)
    ("\\en" "" my-latex-enumerate 0)
    ("\\ls" "" my-latex-list 0)

    ("\\su" "" my-latex-sum 0)
    ("\\in"  "" my-latex-int 0)

    ("\\il" "" my-latex-int-lim 0)
    ("\\sl" "" my-latex-sum-lim 0)
   
    ("\\fr" "" my-latex-frac 0)
    ("\\rf" "" my-latex-ref 0)
  ))