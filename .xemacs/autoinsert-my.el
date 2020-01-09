(setq auto-insert-alist
      '(
        
        (("\\.h\\'" . "C / C++ header") 
         . (lambda () 
             (let ((header-ident 
                    (concat ""
                            (upcase (concat
				     (file-name-nondirectory
				      (substring buffer-file-name 0
						 (match-beginning 0)))
				     "_H")))))
               (insert (concat "#ifndef " header-ident "\n"
			       "#define " header-ident "\n"
			       "\n\n\n#endif\n"
			       )))))
        
        ))

(define-auto-insert "\\.tex\\'" "header.tex")
(define-auto-insert "\\.pl\\'" "header.pl")
(define-auto-insert "\\.sh\\'" "header.sh")
(define-auto-insert "\\.py\\'" "header.py")
(define-auto-insert "\\.f\\'" "header.f")
(define-auto-insert "\\.cpp\\'" "header.cpp")
(define-auto-insert "\\.el\\'" "header.el")

