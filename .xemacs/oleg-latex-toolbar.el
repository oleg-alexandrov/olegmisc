;; - Put the files in some directory, e.g. ~/elisp/latex-toolbar
;; - Put the following lines into your .emacs
;;      (add-to-list 'load-path "~/elisp/latex-toolbar")

(require 'latex)

;;The directory where the icons are installed
(defvar latex-toolbar-icon-directory "~/.xemacs/icons")
;(defvar latex-toolbar-icon-directory
;  (concat
;   (cond ((locate-library "latex-toolbar")
;	  (concat (file-name-directory (locate-library "latex-toolbar"))))
;	 (t
;	  (expand-file-name ".")))
;   "icons")
;  )

;;The printers one could use to print TeX
(setq TeX-printer-list 
      '(
	("cone")
	("mani")
	("cube")
	))
(setq TeX-printer-default "cone")

;; Add some commands to `TeX-command-list'
(add-to-list 'TeX-command-list
'("xdvi" "xdvi -geometry 1100x990+10+0 -expert -s 5 +1 %s.dvi"
TeX-run-silent nil nil))
      
(add-to-list 'TeX-command-list
'("gv" "dvips -Ppdf %s; gv -geometry 1000x990+10+0 -scale 2  %s.ps"
TeX-run-silent nil nil))
      
(add-to-list 'TeX-command-list
'("Acroread" "xterm -e pdflatex %s.tex; acroread -geometry 1200x990+0+0 %s.pdf"
TeX-run-silent nil nil))
      

(add-to-list 'TeX-command-list
'("MyLaTeX" "~/.xemacs/run_latex %s"
TeX-run-silent nil nil))


(defun save-and-run-latex ()
  "Compile a LaTex file"
  (interactive)
  (save-buffer (current-buffer))
  (TeX-command-menu "LaTeX")
)


(defun latex-toolbar-insert (str cnt)
  (insert str)
  (forward-char cnt))

(defun latex-toolbar-make-button (file)
  (if (not (file-name-extension file))
      (setq file (concat file ".xpm")))
  (setq file (expand-file-name file latex-toolbar-icon-directory))
  (if (file-readable-p file)
      (toolbar-make-button-list file)
    (error "cannot find pixmap %s" file)))

(defun latex-toolbar-print ()
  (interactive)
  "Run the Print command from the toolbar. Popups a printer
selection dialog box. Automagicly inhibits the confirmation
garbage."
  (let* ((printer-list (mapcar (function car) TeX-printer-list))
	 (dialog-entry
	  (append (list "Select printer!")
		  (mapcar
		   (function
		    (lambda (pr)
		      (vector pr (list 'latex-toolbar-print1 pr) t)))
		   printer-list)
		  (list nil
			["Cancel" ignore t]))))
    (popup-dialog-box dialog-entry)))

(defun latex-toolbar-print1 (printer)
  (let* ((entry (copy-sequence (assoc "Print" TeX-command-list)))
	 TeX-command-list)
    (rplaca (nthcdr 3 entry) nil)
    (setq TeX-command-list (list entry))
    (TeX-command-menu-print printer TeX-print-command "Print")))

(defun latex-toolbar-print-default ()
  "Run the Print command from the toolbar on the default printer"
  (interactive)
  (latex-toolbar-print1 TeX-printer-default))



(defconst latex-toolbar-latex-icon
  (latex-toolbar-make-button "latex.xpm"))

(defconst latex-toolbar-xdvi-icon
  (latex-toolbar-make-button "xdvi.xpm"))

(defconst latex-toolbar-gv-icon
  (latex-toolbar-make-button "gv.xpm"))

(defconst latex-toolbar-acroread-icon
  (latex-toolbar-make-button "acroread.xpm"))

(defconst latex-toolbar-print-icon   
(latex-toolbar-make-button "print-cap-up.xpm"))


(defconst latex-toolbar '(
    	[toolbar-file-icon		toolbar-open			t	"Open a file"]
	;[toolbar-folder-icon		toolbar-dired			t	"Edit a directory"]
	[toolbar-disk-icon		toolbar-save			t	"Save buffer"]
	[toolbar-printer-icon		toolbar-print			t	"Print text"]
	[toolbar-cut-icon		toolbar-cut			t	"Kill region"]
	[toolbar-copy-icon		toolbar-copy			t	"Copy region"]
	[toolbar-paste-icon		toolbar-paste			t	"Paste from clipboard"]
	[toolbar-undo-icon		toolbar-undo			t	"Undo edit"]
	[toolbar-redo-icon		toolbar-redo			t	"Redo edit"]
	[toolbar-spell-icon		toolbar-ispell			t	"Check spelling"]
	[toolbar-search-icon		toolbar-search			t	"Search"]
	[toolbar-replace-icon		toolbar-replace			t	"Search & Replace"]
	[toolbar-mail-icon		toolbar-mail			t	"Read mail"]
	
	[latex-toolbar-latex-icon	save-and-run-latex 		t	"Run LaTeX"]
	[latex-toolbar-xdvi-icon	(TeX-command-menu "xdvi")	t	"View dvi"]
	[latex-toolbar-gv-icon		(TeX-command-menu "gv")		t	"View ps"]
	[latex-toolbar-acroread-icon	(TeX-command-menu "Acroread")	t	"View pdf"]
	[latex-toolbar-print-icon	latex-toolbar-print		t	"Print ps"]
    ))

;;launch the toolbar
;(set-specifier default-toolbar (cons (current-buffer) latex-toolbar))
;(setq locale 'global)
(set-specifier top-toolbar (cons (current-buffer) latex-toolbar))