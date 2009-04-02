(define-abbrev-table 'tcl-mode-abbrev-table '(
    ))

(define-abbrev-table 'temp-buffer-mode-abbrev-table '(
    ))

(define-abbrev-table 'makefile-mode-abbrev-table '(
    ))

(define-abbrev-table 'xml-mode-abbrev-table '(
    ))

(define-abbrev-table 'sgml-mode-abbrev-table '(
    ))

(define-abbrev-table 'mutt-mode-abbrev-table '(
    ("existance" "existence" nil 2)
    ("htis" "this " nil 0)
    ("hte" "the " nil 7)
    ("continous" "continuous " nil 0)
    ("riguros" "rigoros" nil 0)
    ("taht" "that " nil 14)
    ("wiht" "with " nil 0)
    ))

(define-abbrev-table 'c++-mode-abbrev-table '(
    ("\\cs" ".c_str()" nil 11)
    ("\\x" "exit(0);" nil 5)
    ("\\vi" "vector<int> " nil 2)
    ("\\ubf" "Util_AssertBool(false);" nil 56)
    ("\\dos" "double* " nil 2)
    ("cin" "cin >> " nil 8)
    ("\\vd" "vector<double> " nil 63)
    ("\\cl" ".close();" nil 12)
    ("\\us" "Util_AssertString(\"\");" (lambda nil (interactive) (search-backward "\"")) 1)
    ("\\p" ".push_back();" (lambda nil (interactive) (search-backward ")")) 0)
    ("\\cd" "complex<double>" nil 8)
    (",." " << endl;" nil 185)
    ("\\ui" "unsigned int " nil 9)
    (",," " << ' ' << " nil 233)
    ("\\f" ".front()" nil 0)
    ("\\e" "endl;" nil 3)
    ("\\c" ".clear();" nil 12)
    ("paramter" "parameter" nil 3)
    ("for" "" c++-for 651)
    ("\\ub" "Util_AssertBool(\"\");" (lambda nil (interactive) (search-backward "\"")) 0)
    ("nii" "new int []" backward-char 4)
    ("teh" "the " nil 32)
    ("uns" "using namespace std;" nil 10)
    ("while" "while" c-electric-continued-statement 4)
    ("else" "else" c-electric-continued-statement 99)
    ("couts" "cout << \"\" << endl;" (lambda nil (interactive) (setq smart_forward_flag 1) (search-backward "\"") (c-indent-command)) 959)
    ("cout" "cout << \"\" << endl;" (lambda nil (interactive) (setq smart_forward_flag 1) (search-backward "\"") (c-indent-command)) 808)
    ("catch" "catch" c-electric-continued-statement 4)
    ("\\pb" ".push_back();" (lambda nil (interactive) (search-backward ")")) 9)
    ("\\sti" "static_cast<int>" nil 27)
    ("if" "" c++-if 787)
    ("\\incl" "#include \"\"" backward-char 29)
    ("ints" "int*" nil 2)
    ("\\do" "double " nil 757)
    ("\\si" ".size()" nil 59)
    ("\\std" "static_cast<double>" nil 13)
    ("\\inc" "#include <>" backward-char 17)
    ("\\rs" ".resize()" backward-char 13)
    ("coute" "cout << endl;" nil 10)
    ("cerr" "cerr << \"\" << endl;" (lambda nil (interactive) (setq smart_forward_flag 1) (search-backward "\"") (c-indent-command)) 814)
    ))

(define-abbrev-table 'cperl-mode-abbrev-table '(
    (";u" "undef $/;" nil 0)
    ("sg" "" subst-globally 297)
    ("ssg" "" tilde-subst-globally 42)
    ("\\m" "=~ //" (lambda nil (interacive) (backward-char 1)) 0)
    ("\\cf" "close(FILE);
" cperl-indent-command 87)
    ("opf" "open (FILE, \":utf8\", \"\");" (lambda nil (interactive) (backward-char 3)) 1)
    ("\\f" "<FILE>" nil 16)
    ("for" "" perl-for 111)
    ("\\ud" "undef $/; # undefines the separator. Can read one whole file in one scalar." nil 11)
    ("\\ar" "$ARGV[]" (lambda nil (interactive) (forward-char -1)) 2)
    ("else" "" perl-else 119)
    ("\\pf" "print FILE \"\\n\";" (lambda nil (interactive) (forward-char -4)) 48)
    ("\\ofw" "open(FILE, \">\");" (lambda nil (interactive) (search-backward "\"")) 24)
    ("xxx" "exit(0);" nil 152)
    ("\\ag" "$ARGV[];" (lambda nil (interactive) (forward-char -2)) 1)
    ("\\spn" "split(\"\\n\", );" (lambda nil (interactive) (search-backward ")")) 1)
    ("\\ofr" "open(FILE, \"<\");" (lambda nil (interactive) (search-backward "\"")) 20)
    ("if" "" perl-if 334)
    (";x" "exit(0);" nil 0)
    ("foreach" "" perl-foreach 171)
    ("\\of" "open (FILE, \":utf8\", \"\");" (lambda nil (interactive) (backward-char 3)) 54)
    ("print" "" perl-print 600)
    ))

(define-abbrev-table 'c-mode-abbrev-table '(
    ("while" "while" c-electric-continued-statement 0)
    ("else" "else" c-electric-continued-statement 0)
    ))

(define-abbrev-table 'fortran-mode-abbrev-table '(
    ))

(define-abbrev-table 'matlab-mode-abbrev-table '(
    ("dsp" "disp(sprintf('%'));" (lambda nil (indent-according-to-mode) (search-backward "%")) 202)
    (";dl" "dlmread" nil 0)
    (";p" "plot(" nil 0)
    ("dsi" "display(sprintf('%d'))" nil 1)
    ("\\ch" "clf; hold on; axis equal; axis off;" nil 0)
    ("\\fig" "figure()" backward-char 11)
    ("\\disp" "disp(sprintf('%d', ));" nil 1)
    (";d" "dlmread" nil 0)
    ("cho" "clf; hold on;" nil 1)
    ("sgf" "saveas(gcf, '')" nil 24)
    ("for" "" matlab-for 293)
    ("teh" "the " nil 18)
    ("cha" "clf; hold on; axis equal; axis off;" nil 5)
    ("\\zz" "\\xxxx" nil 0)
    ("\\dis" "disp(sprintf('%0.8g', ))" nil 6)
    ("xxx" "break;" nil 3)
    ("if" "" matlab-if 251)
    ("fch" "figure(); clf; hold on;" (lambda nil (interactive) (search-backward ")")) 1)
    ("chaa" "clf; hold on; axis equal; axis off" nil 10)
    ("aeo" "axis equal; axis off;" nil 1)
    ("dlr" "dlmread('');" nil 1)
    ("sag" "saveas(gcf, '')" nil 2)
    ("dsr" "display(sprintf('%0.9g'))" nil 1)
    ))

(define-abbrev-table 'html-mode-abbrev-table '(
    ("hh" "<hr />" nil 7)
    ("\\s" "&nbsp;" nil 1)
    (",," "<br>" nil 2)
    ("htat" "that " nil 2)
    ("teh" "the " nil 28)
    ("hte" "the " nil 9)
    ("\\hr" "<hr />" nil 2)
    ("pp" "<p /> " nil 17)
    (".." "&nbsp;" nil 1)
    ("bb" "<br />" nil 19)
    ("taht" "that " nil 7)
    ("wiht" "with " nil 1)
    ("\\ect" "</center>" nil 5)
    ("\\bct" "<center>" nil 1)
    ("\\ct" "<center></center>" (lambda nil (interactive) (search-backward "</")) 7)
    ))

(define-abbrev-table 'hm--html-mode-abbrev-table '(
    ("hh" "<hr />" nil 9)
    ("\\s" "&nbsp;" nil 1)
    (",," "<br>" nil 2)
    ("htat" "that " nil 2)
    ("\\prob" "<p /> <b>Problem .</b>" nil 2)
    ("teh" "the " nil 2)
    ("incl" "<!--#include virtual=\"\"-->" nil 1)
    ("\\hr" "<hr />" nil 2)
    ("pp" "<p /> " nil 33)
    (".." "&nbsp;" nil 7)
    ("bb" "<br />" nil 23)
    ("taht" "that " nil 7)
    ("wiht" "with " nil 1)
    ("\\ect" "</center>" nil 5)
    ("\\bct" "<center>" nil 1)
    ("\\ct" "<center></center>" (lambda nil (interactive) (search-backward "</")) 7)
    ))

(define-abbrev-table 'f90-mode-abbrev-table '(
    ("`eli" "else if" nil 0)
    ("`ey" "entry" nil 0)
    ("`it" "intent" nil 0)
    ("`mo" "module" nil 0)
    ("`r" "real" nil 0)
    ("`ex" "external" nil 0)
    ("`tr" ".true." nil 0)
    ("`pu" "public" nil 0)
    ("`im" "implicit none" nil 0)
    ("write" "write (*, *) " nil 3)
    ("`pr" "print" nil 0)
    ("`eq" "equivalence" nil 0)
    ("`fa" ".false." nil 0)
    ("`i" "integer" nil 0)
    ("`bd" "block data" nil 0)
    ("`po" "pointer" nil 0)
    ("`as" "assignment" nil 0)
    ("`pm" "program" nil 0)
    ("`el" "else" nil 0)
    ("`ba" "backspace" nil 0)
    ("`wr" "write" nil 0)
    ("`if" "interface" nil 0)
    ("`su" "subroutine" nil 0)
    ("`c" "character" nil 0)
    ("`lo" "logical" nil 0)
    ("`pi" "private" nil 0)
    ("`dw" "do while" nil 0)
    ("`al" "allocate" nil 0)
    ("`sq" "sequence" nil 0)
    ("`in " "include" nil 0)
    ("`ta" "target" nil 0)
    ("`wh" "where" nil 0)
    ("`pa" "parameter" nil 0)
    ("`op" "optional" nil 0)
    ("`ab" "allocatable" nil 0)
    ("`rw" "rewind" nil 0)
    ("`cy" "cycle" nil 0)
    ("`se" "select" nil 0)
    ("`di" "dimension" nil 0)
    ("`rt" "return" nil 0)
    ("`cx" "complex" nil 0)
    ("`df" "define" nil 0)
    ("`de" "deallocate" nil 0)
    ("`nu" "nullify" nil 0)
    ("`cn" "contains" nil 0)
    ("`cm" "common" nil 0)
    ("`elw" "elsewhere" nil 0)
    ("`cl" "close" nil 0)
    ("`fu" "function" nil 0)
    ("`rc" "recursive" nil 0)
    ("`fo" "format" nil 0)
    ("`fl" "forall" nil 0)
    ("`na" "namelist" nil 0)
    ("print" "print *, \"\"" (lambda nil (interactive) (search-backward "\"")) 39)
    ("`t" "type" nil 0)
    ))

(define-abbrev-table 'matlab-shell-help-mode-abbrev-table '(
    ))

(define-abbrev-table 'matlab-shell-topic-mode-abbrev-table '(
    ))

(define-abbrev-table 'winmgr-mode-abbrev-table '(
    ))

(define-abbrev-table 'objc-mode-abbrev-table '(
    ("while" "while" c-electric-continued-statement 0)
    ("else" "else" c-electric-continued-statement 0)
    ))

(define-abbrev-table 'java-mode-abbrev-table '(
    ("finally" "finally" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)
    ("else" "else" c-electric-continued-statement 0)
    ("catch" "catch" c-electric-continued-statement 0)
    ))

(define-abbrev-table 'idl-mode-abbrev-table '(
    ))

(define-abbrev-table 'pike-mode-abbrev-table '(
    ("while" "while" c-electric-continued-statement 0)
    ("else" "else" c-electric-continued-statement 0)
    ))

(define-abbrev-table 'awk-mode-abbrev-table '(
    ("while" "while" c-electric-continued-statement 0)
    ("else" "else" c-electric-continued-statement 0)
    ))

(define-abbrev-table 'xrdb-mode-abbrev-table '(
    ))

(define-abbrev-table 'javascript-mode-abbrev-table '(
    ))

(define-abbrev-table 'doctex-mode-abbrev-table '(
    ))

(define-abbrev-table 'sh-mode-abbrev-table '(
    ))

(define-abbrev-table 'text-mode-abbrev-table '(
    ("\\ro" "\\rho" nil 5)
    ("neib" "neighborhood " nil 14)
    ("\\cs" "" latex-cases 39)
    ("\\rn" "\\mathbb R^n" nil 3)
    ("\\r3" "\\mathbb R^3" nil 0)
    ("\\w" "\\wedge " nil 40)
    ("lin" "linearly " nil 0)
    ("\\r2" "\\mathbb R^2" nil 2)
    ("\\eco" "\\end{corollary}" nil 1)
    ("ily" "I love you!" nil 4)
    ("\\bco" "\\begin{corollary}" nil 1)
    ("\\t" "\\theta" nil 58)
    ("\\sol" "\\medskip\\noindent{\\bf Solution.} " nil 37)
    ("\\ve" "\\varepsilon" nil 138)
    ("\\s" "\\sigma" nil 2)
    ("cin" "cinnps" nil 95)
    ("contr" "contradiction" nil 0)
    ("\\r" "\\mathbb R" nil 82)
    ("\\itm" "" latex-itemize 10)
    ("\\q" "\\mathbb Q" nil 2)
    ("\\cim" "\\cimmps" nil 95)
    ("\\rem" "\\medskip\\noindent{\\bf Remark.} " nil 1)
    ("\\p" "\\partial " nil 88)
    ("\\dist" "\\mbox{dist}" nil 3)
    ("oth" "on the other hand" nil 1)
    ("coeff" "coefficient " nil 1)
    ("\\rf" "" latex-ref 422)
    ("\\o" "\\omega" nil 32)
    ("\\ft" "\\footnote{}" backward-char 1)
    ("\\re" "\\mathfrak{Re}" nil 7)
    ("\\eqs" "" (lambda nil (interactive) (insert "\\begin{equation*}
  
\\end{equation*}") (previous-line 1)) 1)
    ("\\n" "\\noindent" nil 9)
    ("\\ci" "" latex-cite 97)
    ("\\efr" "\\end{flushright}" nil 1)
    ("htis" "this " nil 3)
    ("\\up" "\\usepackage{}" backward-char 6)
    ("\\ni" "\\noindent " nil 47)
    ("enviroment" "environment" nil 0)
    ("rhs" "right-hand side " nil 0)
    ("\\bfr" "\\begin{flushright}" nil 1)
    ("\\prr" "\\medskip\\noindent{\\bf Proof.} " nil 1)
    ("\\fr" "" latex-frac 337)
    ("\\l" "\\lambda" nil 314)
    ("\\bv" "\\begin{verbatim}" nil 14)
    ("\\k" "\\kappa" nil 0)
    ("lhs" "left-hand side " nil 0)
    ("\\tit" "\\begin{center}\\bf \\end{center}" (lambda nil (interactive) (beginning-of-line) (search-forward "bf ")) 2)
    ("\\ne" "\\not=" nil 10)
    ("\\i" "\\infty" nil 127)
    ("\\bt" "\\begin{tabular}" nil 1)
    ("\\efl" "\\end{flushleft}" nil 2)
    ("\\eca" "\\end{cases}" nil 3)
    ("\\bs" "\\begin{subequations}" nil 13)
    ("\\g" "\\gamma" nil 26)
    ("rigoros" "rigorous " nil 0)
    ("\\bfl" "\\begin{flushleft}" nil 2)
    ("\\bca" "\\begin{cases}" nil 3)
    ("\\br" "\\begin{reference}" nil 1)
    ("\\emp" "\\emph{}" backward-char 68)
    ("hwat" "what " nil 1)
    ("\\fl" "\\foilhead{}" (lambda nil (interactive) (backward-char 1)) 8)
    ("\\e" "\\epsilon" nil 7)
    ("htat" "that " nil 5)
    ("funs" "functions " nil 0)
    ("indep" "independent " nil 0)
    ("\\bp" "" latex-bigg-par 7)
    ("\\na" "\\nabla" nil 4)
    ("parameterization" "parametrization " nil 1)
    ("\\d" "\\delta" nil 18)
    ("\\bfi" "\\begin{figure}" nil 0)
    ("\\ans" "\\medskip\\noindent{\\bf Answer.} 
" nil 6)
    ("polyn" "polynomial " nil 1)
    ("\\c" "\\mathbb C" nil 23)
    ("\\efg" "\\end{figure}" nil 7)
    ("homogenous" "homogeneous " nil 1)
    ("\\b" "\\beta" nil 19)
    ("\\bfg" "\\begin{figure}[h]\\label{}" backward-char 6)
    ("\\tt" "\\tt " nil 31)
    ("\\mn" "\\medskip\\noindent " nil 107)
    ("\\bm" "\\begin{multline*}" nil 5)
    ("\\fh" "\\foilhead{}" backward-char 7)
    ("\\eml" "\\end{multline}" nil 3)
    ("\\bml" "\\begin{multline}" nil 3)
    ("approx" "approx " nil 13)
    ("\\bl" "\\begin{list}{}" backward-char 3)
    ("\\prob" "\\medskip\\noindent{\\bf Problem .}" (lambda nil (interactive) (backward-char 2)) 18)
    ("\\a" "\\alpha" nil 126)
    ("\\cor" "" latex-corollary 11)
    ("\\ev" "\\end{verbatim}" nil 13)
    ("\\ker" "\\mbox{Ker}\\," nil 0)
    ("props" "properties " nil 0)
    ("\\transp" "^\\mathsf{T}" nil 0)
    ("\\lab" "\\label{}" backward-char 19)
    ("\\ees" "\\end{equation*}" nil 9)
    ("\\bes" "\\begin{equation*}" nil 6)
    ("\\et" "\\end{tabular}" nil 3)
    ("ack" "acknowledgments" nil 0)
    ("\\bi" "\\begin{itemize}" nil 9)
    ("\\pt" "\\partial" nil 6)
    ("\\es" "\\end{subequations}" nil 5)
    ("\\mi" "\\medskip\\noindent" nil 1)
    ("\\bh" "\\begin{huge}" nil 1)
    ("\\eeq" "\\end{eqnarray}" nil 5)
    ("\\im" "\\mathfrak{Im}" nil 4)
    ("coeffs" "coefficients " nil 1)
    ("\\bg" "" latex-bigg 11)
    ("\\beq" "\\begin{eqnarray}" nil 4)
    ("\\pr" "" latex-proof 16)
    ("\\eq" "" latex-equation 78)
    ("\\il" "" latex-int-lim 83)
    ("teh" "the " nil 184)
    ("\\ep" "\\end{proof}" nil 3)
    ("\\mf" "{\\mathbf }" backward-char 26)
    ("\\be" "\\begin{equation}\\label{}" backward-char 90)
    ("\\een" "\\end{enumerate}" nil 1)
    ("\\pp" "\\Phi" nil 4)
    ("\\els" "\\end{list}" nil 9)
    ("\\bd" "\\begin{definition}" nil 3)
    ("\\ben" "\\begin{enumerate}" nil 3)
    ("weired" "weird " nil 1)
    ("\\lt" "{\\LaTeX} " nil 1)
    ("\\en" "" latex-enumerate 11)
    ("\\ii" "" latex-int 19)
    ("\\md" "\\medskip" nil 40)
    ("\\epm" "\\end{pmatrix}" nil 7)
    ("\\eth" "\\end{theorem}" nil 1)
    ("\\bc" "\\begin{center}" nil 29)
    ("\\bls" "\\begin{list}{}{}" (lambda nil (interactive) (backward-char 3)) 11)
    ("\\wt" "\\widetilde " nil 4)
    ("\\npi" "\\newpage\\item " nil 9)
    ("\\ls" "" latex-list 18)
    ("\\em" "\\end{multline*}" nil 8)
    ("\\mc" "\\mathcal{}" backward-char 2)
    ("oyu" "you " nil 3)
    ("hte" "the " nil 28)
    ("\\bb" "{\\bf }" backward-char 151)
    ("\\bpm" "\\begin{pmatrix}" nil 1)
    ("\\bth" "\\begin{theorem}" nil 2)
    ("\\lr" "\\left(\\right)" (lambda nil (interactive) (search-backward "\\")) 9)
    ("\\p2" "\\frac{\\pi}{2}" nil 12)
    ("\\th" "" latex-theorem 11)
    ("\\el" "\\end{list}" nil 3)
    ("\\mb" "\\mbox{  }" (lambda nil (interactive) (backward-char 2)) 115)
    ("\\ba" "\\begin{array}{}" backward-char 16)
    ("\\lrp" "\\left(\\right)" (lambda nil (interactive) (search-backward "\\")) 2)
    ("thm" "theorem " nil 2)
    ("\\sub" "\\subset " nil 15)
    ("\\dy" "\\Delta y" nil 1)
    ("continous" "continuous " nil 2)
    ("\\ei" "\\end{itemize}" nil 7)
    ("\\elm" "\\end{lemma}" nil 3)
    ("\\eal" "\\end{align}" nil 3)
    ("\\dx" "\\Delta x" nil 1)
    ("\\eh" "\\end{huge}" nil 1)
    ("\\blm" "\\begin{lemma}" nil 2)
    ("\\bal" "\\begin{align}" nil 3)
    ("\\bib" "\\bibitem{}" backward-char 6)
    ("\\transpose" "^\\mathsf{T}" nil 0)
    ("\\st" "\\stackrel{\\to}{}" backward-char 32)
    ("\\lm" "" latex-lemma 15)
    ("\\l2" " L^2(0,\\infty)" nil 1)
    ("\\tc" "\\textcolor{}{}" (lambda nil (interactive) (backward-char 3)) 15)
    ("belive" "believe" nil 8)
    ("\\ss" "\\subsection{}" backward-char 4)
    ("behaviour" "behavior " nil 15)
    ("\\ll" "\\lim\\limits_{}" backward-char 22)
    ("\\esp" "\\end{split}" nil 13)
    ("\\ef" "\\end{figure}" nil 1)
    ("\\ee" "\\end{equation}" nil 70)
    ("\\bsp" "\\begin{split}" nil 12)
    ("\\sq" "\\sqrt{}" backward-char 113)
    ("\\dt" "\\Delta t" nil 2)
    ("\\ed" "\\end{definition}" nil 1)
    ("\\sp" "" (lambda nil (interactive) (insert "\\begin{split}
  
\\end{split}") (previous-line 1)) 11)
    ("\\ds" "\\displaystyle" nil 2)
    ("\\li" "Lipschitz" nil 6)
    ("\\ec" "\\end{center}" nil 29)
    ("\\eea" "\\end{eqnarray}" nil 1)
    ("\\h2" "H^{(2)}_m" nil 4)
    ("\\esl" "\\end{slide}" nil 2)
    ("\\mbf" "\\mathbf{}" backward-char 6)
    ("\\baf" "Burden and Faires" nil 10)
    ("\\bea" "\\begin{eqnarray}" nil 1)
    ("riguros" "rigoros" nil 1)
    ("\\ea" "\\end{array}" nil 15)
    ("\\bsl" "\\begin{slide}" nil 1)
    ("\\edo" "\\end{document}" nil 3)
    ("param" "parametrization " nil 0)
    ("\\sm" "\\smallskip" nil 3)
    ("\\hl" "\\\\ \\hline " newline 19)
    ("\\h1" "H^{(1)}_m" nil 6)
    ("\\pa" "\\paragraph" nil 1)
    ("\\mat" "" latex-matrix 49)
    ("\\bdo" "\\begin{document}" nil 6)
    ("taht" "that " nil 53)
    ("\\sl" "" latex-sum-lim 23)
    ("cyl" "cylindrical" nil 0)
    ("\\incl" "\\includegraphics[clip, width=0.8\\textwidth]{}" backward-char 18)
    ("\\toi" "\\to\\infty" nil 1)
    ("\\eab" "\\end{abstract}" nil 2)
    ("wiht" "with " nil 3)
    ("\\evt" "\\end{verbatim}" nil 4)
    ("\\eaa" "\\end{eqnarray}" nil 0)
    ("\\bab" "\\begin{abstract}" nil 3)
    ("\\hi" "\\phi" nil 155)
    ("\\bvt" "\\begin{verbatim}" nil 4)
    ("\\ab" "(\"\" \"\"  0)" (lambda nil (interactive) (beginning-of-line) (search-forward "\"")) 9)
    ("\\vt" "\\vartheta" nil 16)
    ("\\om" "\\Omega" nil 21)
    ("\\ese" "\\end{subequations}" nil 4)
    ("\\fra" "" latex-frame 24)
    ("\\bse" "\\begin{subequations}" nil 3)
    ("analytical" "analytic " nil 3)
    ("\\sss" "\\subsubsection{}" backward-char 4)
    ("prop" "property " nil 0)
    ("responce" "response " nil 1)
    ("defence" "defense " nil 3)
    ("\\abb" "(\"\" \"\" (lambda () (interactive)) 0)" (lambda nil (interactive) (beginning-of-line) (search-forward "\"")) 9)
    ("\\vp" "\\varphi" nil 35)
    ("\\inc" "\\includegraphics[width=0.8\\textwidth]{}" backward-char 3)
    ("\\di" "\\displaystyle" nil 3)
    ("\\trans" "^\\mathsf{T}" nil 3)
    ("\\ny" "\\nyice" nil 96)
    ("\\vec" "" latex-vector 35)
    ("disp" "displaystyle" nil 14)
    ("\\rr" "\\ref{}" backward-char 73)
    ("\\sli" "" latex-frame 25)
    ("\\sec" "\\section{}" backward-char 14)
    ("\\ect" "\\end{center}" nil 7)
    ("\\grad" "\\nabla" nil 17)
    ("\\z" "\\mathbb Z" nil 19)
    ("\\bct" "\\begin{center}" nil 5)
    ("\\ct" "" latex-center 28)
    ("\\dd" "\\Delta" nil 1)
    ))

(define-abbrev-table 'lisp-mode-abbrev-table '(
    ("lc" "" local-set-abb 15)
    ("dd" "" lisp-function 19)
    ("gg" "" global-set-abb 8)
    ("\\li" "(lambda () (interacive) )" backward-char 1)
    ("\\ab" "(\"\" \"\"  0)" (lambda nil (interactive) (beginning-of-line) (search-forward "\"")) 0)
    ))

(define-abbrev-table 'help-mode-abbrev-table '(
    ))

(define-abbrev-table 'completion-list-mode-abbrev-table '(
    ))

(define-abbrev-table 'fundamental-mode-abbrev-table '(
    ("\\trk" "set shared::job::dbgTrackKaroList                      {0 }      ;# UD" nil 0)
    ))

(define-abbrev-table 'global-abbrev-table '(
    ))

