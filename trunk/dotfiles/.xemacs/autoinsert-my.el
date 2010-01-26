(defvar auto-insert-alist
  '((("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . "C / C++ header")
     (upcase (concat (file-name-nondirectory
		      (substring buffer-file-name 0 (match-beginning 0)))
		     "_"
		     (substring buffer-file-name (1+ (match-beginning 0)))))
     "#ifndef " str \n
     "#define " str "\n\n"
     _ "\n\n#endif")

;    (("\\.\\([Cc]\\|cc\\|cpp\\)\\'" . "C / C++ program")
;     nil
;     "#include \""
;     ;; nop without latest cc-mode
;     (and (fboundp 'c-companion-file)
;					;(file-readable-p (c-companion-file 'name))
;	  (file-name-nondirectory (c-companion-file 'name))) & ?\"
;     | -10)

    ("[Mm]akefile\\'" . "makefile.inc")


    (plain-tex-mode . "tex-insert.tex")
    (bibtex-mode . "tex-insert.tex")
    (latex-mode
     ;; should try to offer completing read for these
     "options, RET: "
     "\\documentstyle[" str & ?\] | -1
     ?{ (read-string "class: ") "}\n"
     ("package, %s: "
      "\\usepackage[" (read-string "options, RET: ") & ?\] | -1 ?{ str "}\n")
     _ "\n\\begin{document}\n" _
     "\n\\end{document}")

    (("/bin/.*[^/]\\'" . "Shell-Script mode magic number")
     lambda ()
     (if (eq major-mode default-major-mode)
	 (sh-mode)))
    
    (ada-mode . ada-header)


    (("\\.f\\'" . "Fortran mode")
     (upcase (concat (file-name-nondirectory
		      (substring buffer-file-name 0 (match-beginning 0)))
		     "_"
		     (substring buffer-file-name (1+ (match-beginning 0)))))
     "      "
     (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
     " ! Author: " (user-full-name)
;'(if (search-backward "&" (save-excursion (beginning-of-line 1) (point)) t)
;     (replace-match (capitalize (user-login-name)) t t))
;'(end-of-line 1) " <" (user-login-name) ?@ (system-name) ">,  "
;(substring (current-time-string) -20)
     "
      implicit none


      end
")

    (("\\.cpp\\'" . "cpp mode")
     (upcase (concat (file-name-nondirectory
		      (substring buffer-file-name 0 (match-beginning 0)))
		     "_"
		     (substring buffer-file-name (1+ (match-beginning 0)))))
"#include <iostream>
#include <fstream>
#include <cmath>
#include <algorithm>
using namespace std;
"

     "void "
     (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
     "(){


}"
;     " ! Author: " (user-full-name)
;'(if (search-backward "&" (save-excursion (beginning-of-line 1) (point)) t)
;     (replace-match (capitalize (user-login-name)) t t))
;'(end-of-line 1) " <" (user-login-name) ?@ (system-name) ">,  "
;(substring (current-time-string) -20)
     )

    
    ))

(define-auto-insert "\\.tex\\'" "header.tex")
(define-auto-insert "\\.pl\\'" "header.pl")
(define-auto-insert "\\.sh\\'" "header.sh")
(define-auto-insert "\\.py\\'" "header.py")
;;(define-auto-insert "\\.f\\'" "header.f")
;;(define-auto-insert "\\.cpp\\'" "header.cpp")
;(define-auto-insert "\\.el\\'" "header.el")

