;;; mas-file-history.el 
;; A menu buffer to select and open files which you have opened before.
;; Position of the cursor is restored when you open the file again.

;; Copyright (C) 2001 Masahiro Kawata.

;; Author: Masahiro Kawata <mas@hompo.co.jp>
;; Filename: mas-file-history.el
;; URL: http://www.hompo.co.jp/~mas/
;; Keywords: history
;; Version: 1.0.8
;; Description: A menu buffer select and open files which you have opened before.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; code

;; definition variables
(defgroup file-history nil
  "A menu buffer to select and open files which you have opened before."
  :group 'files)
  
(defcustom mas-file-history-num 1000
  "Number of files saved in history file."
  :type 'integer
  :group 'file-history)

(defvar mas-list-file-history-start-point 49)

(defcustom mas-file-history-save-filename "~/.mas-file-history"
  "Name of history file."
  :type 'string
  :group 'file-history)

(defcustom mas-exclude-file-regexp-list '(".*semantic\.cache" "^/tmp")
  "List of regular expressions to exclude."
  :type  '(repeat regexp)
  :group 'file-history)

(defvar mas-file-history ()
  "Assosiation list of file history.
The format of the assosiation list is
\(FILENAME1 FILENAME2 ...\)

where each FILENAME is of the form

\(point cursor-point\)
\(attr  attribute\)
\(visited file-visited-num)"
  )
(defcustom mas-file-history-emacs-start-allfile-open nil
  "t is Open all files when emacs is started."
  :type 'boolean
  :group 'file-history)

(defcustom mas-file-history-emacs-start-autoload-registered-files nil
  "t is Open all registered files when emacs is started."
  :type 'boolean
  :group 'file-history)

(defcustom mas-file-history-autoload-load-as-normal-buffer nil
  "t is Open all auto-loaded files as normal buffer."
  :type 'boolean
  :group 'file-history)

(defcustom mas-file-history-delete-not-exist-file-when-kill-emacs nil
  "t is Delete not-exist-file when kill emacs."
  :type 'boolean
  :group 'file-history)
(defcustom mas-file-history-mask-file-regexp ""
  "regular expression to mask history-file"
  :type 'regexp
  :group 'file-history)

(defvar mas-file-history-mode-map nil "")

(defvar mas-file-history-sort-compare-func
  '((lambda (a b) (if (< (or (cdr(assq 'visited (cadr a))) 0)
						(or (cdr(assq 'visited (cadr b))) 0)) t nil))
	(lambda (a b) (if (> (or (cdr(assq 'visited (cadr a))) 0)
						(or (cdr(assq 'visited (cadr b))) 0)) t nil))
	(lambda (a b) (string< (car a) (car b)))
	(lambda (a b) (string< (car b) (car a)))
	))

(defvar mas-file-history-menu-title "File History")
(defvar mas-file-history-max-menu-item 10)
(defvar mas-file-history-menu-path (if (featurep 'xemacs)
				       '("File")
				     '("files")))
(defvar mas-file-history-menu-before (if (featurep 'xemacs)
				"Open..."
			      "open-file"))

(if (file-exists-p mas-file-history-save-filename)
	(load-file mas-file-history-save-filename))
 
(if mas-file-history-mode-map
	()
  (setq mas-file-history-mode-map (make-keymap))
  (suppress-keymap mas-file-history-mode-map t)
  (define-key mas-file-history-mode-map "\C-m" 'mas-open-file-history)
  (define-key mas-file-history-mode-map "q" 'quit-window)
  (define-key mas-file-history-mode-map "1"
	(lambda () (interactive) (mas-open-file-history 0)))
  (define-key mas-file-history-mode-map "2"
	(lambda () (interactive) (mas-open-file-history 1)))
  (define-key mas-file-history-mode-map "3"
	(lambda () (interactive) (mas-open-file-history 2)))
  (define-key mas-file-history-mode-map "4"
	(lambda () (interactive) (mas-open-file-history 3)))
  (define-key mas-file-history-mode-map "5"
	(lambda () (interactive) (mas-open-file-history 4)))
  (define-key mas-file-history-mode-map "6"
	(lambda () (interactive) (mas-open-file-history 5)))
  (define-key mas-file-history-mode-map "7"
	(lambda () (interactive) (mas-open-file-history 6)))
  (define-key mas-file-history-mode-map "8"
	(lambda () (interactive) (mas-open-file-history 7)))
  (define-key mas-file-history-mode-map "9"
	(lambda () (interactive) (mas-open-file-history 8)))
  (define-key mas-file-history-mode-map "s" 'isearch-forward)
  (define-key mas-file-history-mode-map "r" 'isearch-backward)
  (define-key mas-file-history-mode-map "p" 'previous-line)
  (define-key mas-file-history-mode-map "n" 'next-line)
  (define-key mas-file-history-mode-map "k" 'mas-mark-delete-file-history)
  (define-key mas-file-history-mode-map "d" 'mas-mark-delete-file-history)
  (define-key mas-file-history-mode-map "o" 'mas-mark-open-file-history)
  (define-key mas-file-history-mode-map "I" 'mas-mark-remove-remember-file-history)
  (define-key mas-file-history-mode-map "i" 'mas-mark-remember-file-history)
  (define-key mas-file-history-mode-map "u" 'mas-mark-file-history)
  (define-key mas-file-history-mode-map "x" 'mas-exec-file-history)
  (define-key mas-file-history-mode-map "?" 'describe-mode)
  (define-key mas-file-history-mode-map "m" 'mas-mask-file-history)
)
(put 'mas-file-history-mode 'mode-class 'special)

(defun mas-mark-delete-file-history ()
  (interactive)
  (mas-mark-file-history ?D))

(defun mas-mark-remember-file-history ()
  (interactive)
  (mas-mark-file-history ?R))

(defun mas-mark-open-file-history ()
  (interactive)
  (mas-mark-file-history ?O))

(defun mas-mark-remove-remember-file-history ()
  (interactive)
  (mas-mark-file-history ?E))

(defun mas-file-history-mode ()
  "Major mode for open-file-history.
\\<mas-file-history-mode-map>
\\[mas-open-file-history] Open file which is pointed by cursor
1~9  Open 1 ~ 9th file from top of the menu.
k Put a delete-mark on a file which is pointed by cursor
d Same as k key.
\\[mas-mark-open-file-history] Put a open-mark on a file which is pointed by cursor.
\\[mas-mark-remember-file-history] Put a remember-mark on a file which is pointed by cursor.
\\[mas-mark-remove-remember-file-history] Put a remove-remember-mark on a file which is pointed by cursor.
\\[mas-mark-file-history] delete delete-mark of a file
\\[mas-exec-file-history] Execute all marked commands.
\\[next-line] Go to next line
\\[previous-line] Go to previous line
\\[isearch-forward] Execute incremental search
\\[isearch-backward] Execute reverse incremental search
\\[mas-delete-not-exist-file] Delete entries if it's file  does not exist on disk.
\\[mas-sort-visited-asc] Sort by visited count by ascending-order. 
\\[mas-sort-visited-desc] Sort by visited count by descending-order. 
\\[mas-sort-filename-asc] Sort by filename by ascending-order. 
\\[mas-sort-filename-desc] Sort by filename by descending-order.
\\[mas-mask-file-history] Mask file by regular expression string.
\\[quit-window] Exit from menu
"
  (kill-all-local-variables)
  (use-local-map mas-file-history-mode-map)
  (setq major-mode 'mas-file-history-mode)
  (setq mode-name "mas-file-history")
  (setq buffer-read-only t)
  (run-hooks 'mas-file-history-mode-hook)
  )

(defun mas-file-history-make-menu-spec (name history n)
  (let ((ret (list name)))
    (while (and (> n 0) history)
      (setq ret (cons (vector (caar history) (list 'find-file (caar history))) ret))
      (setq history (cdr history))
      (setq n (1- n)))
    (nreverse ret)))

(defun mas-file-history-add-menu ()
  (easy-menu-define
   mas-file-history-menu
   mas-file-history-mode-map
   ""
   (mas-file-history-make-menu-spec mas-file-history-menu-title
                                    mas-file-history
                                    mas-file-history-max-menu-item))

  (easy-menu-add-item
   nil
   mas-file-history-menu-path
   mas-file-history-menu
   mas-file-history-menu-before))

(defun mas-delete-file-history ( buffilename)
  "Delete a desired entry from history."
  (let ((list-fl mas-file-history)
		(prev-list mas-file-history))
	(while list-fl
	  (let* ((fl (car list-fl))
			 (name (car fl))
			 (history-point (cdr fl)))
		(if (not (equal buffilename name))
			(progn
			  (setq prev-list list-fl)
			  (setq list-fl (cdr list-fl)))
		  ;; delete exist same list
		  (if (eq list-fl mas-file-history)
			  ;; first
			  (setq mas-file-history (cdr list-fl))
			(setcdr prev-list (cdr list-fl))
			(setq list-fl nil)))
		))))

(defun mas-move-to-top-file-history ( buffilename)
  "Move desired file to list's top."
  (let ((list-fl mas-file-history)
		(prev-list mas-file-history))
	(while list-fl
	  (let* ((fl (car list-fl))
			 (name (car fl))
			 (history-point (cadr fl))
			 (history-attr (car(cddr fl))))
		(if (not (equal buffilename name))
			(progn
			  (setq prev-list list-fl)
			  (setq list-fl (cdr list-fl)))
		  (if (eq list-fl mas-file-history)
			  (setq list-fl nil)
		  (setcdr prev-list (cdr list-fl))
;;		  (setcdr list-fl nil)
		  (setq mas-file-history (cons fl mas-file-history))
		  (setq list-fl nil)
		  ))))))

(defun mas-get-record-file-history (buffilename)
  "get cursor point from alist by buffilename"
  (cadr (assoc buffilename mas-file-history)))

(defun mas-get-point-file-history (buffilename)
  (cdr (assq 'point (mas-get-record-file-history buffilename))))

(defun mas-set-point-file-history (buffilename pos)
  (let ((cell (assq 'point (mas-get-record-file-history buffilename))))
	(and cell
		 (setcdr cell pos))))

(defun mas-get-attr-file-history (buffilename)
  (cdr (assq 'attr (mas-get-record-file-history buffilename))))

(defun mas-set-attr-file-history (buffilename attr)
  (let ((cell (assq 'attr (mas-get-record-file-history buffilename))))
	(and cell
		 (setcdr cell attr))))
(defun mas-get-visited-file-history (buffilename)
	(cdr (assq 'visited (mas-get-record-file-history buffilename))))

(defun mas-set-visited-file-history (buffilename visited)
  (let ((cell (assq 'visited (mas-get-record-file-history buffilename))))
	(if cell
		(setcdr cell visited)
	  ( setcdr (assoc buffilename mas-file-history)
			   (list (append (cadr (assoc buffilename mas-file-history)) (list (cons 'visited 1)))))
		)))

(defun mas-append-file-history (&optional arg)
  "Add desired file to history. Add current buffer when argument is omitted."
  (set-buffer (or arg (buffer-name)))
  (if (not 
       (or
        (null (buffer-file-name))
        ;; do not append to history if exclude-string matches buffer-file-name
        (let ((exclude-list mas-exclude-file-regexp-list)
              (exclude-string)
              (exclude-p nil))
          (while exclude-list
            (setq exclude-string (car exclude-list)) 
            (if (string-match exclude-string buffer-file-name)
                (progn
                  (setq exclude-list nil)
                  (setq exclude-p t))
              (setq exclude-list (cdr exclude-list))))
          exclude-p)))
      (if (mas-get-record-file-history buffer-file-name)
          (progn
            (mas-move-to-top-file-history buffer-file-name)
            (mas-set-point-file-history buffer-file-name (point)))
        ;; if not found then append new list
        (setq mas-file-history
              (cons (list buffer-file-name
                          (list (cons 'point (point)) (cons 'attr nil) (cons 'visited 1)))
                    mas-file-history)))
    ;; check history-num
    (let ((list-fl mas-file-history)
          (prev-list mas-file-history)
          (num 0))
      (while list-fl
        (let* ((fl (car list-fl))
               (attr (cdr (assq 'attr (cdr fl)))))
          (if (null attr )
              (progn
                (setq num (+ num 1))
                (if (> num mas-file-history-num)
                    (setcdr prev-list (cdr list-fl))
                  (setq prev-list list-fl)))
            (setq prev-list list-fl))
          (setq list-fl (cdr list-fl))))))
  (mas-file-history-add-menu))

(defun mas-sort-visited-asc ()
  "Sort by visited count by ascending-order"
  (interactive)
  (mas-sort-file-history 0))
(defun mas-sort-visited-desc ()
  "Sort by visited count by descending-order"
  (interactive)
  (mas-sort-file-history 1))
(defun mas-sort-filename-asc ()
  "Sort by filename by ascending-order"
  (interactive)
  (mas-sort-file-history 2))
(defun mas-sort-filename-desc ()
  "Sort by filename by descending-order"
  (interactive)
  (mas-sort-file-history 3))

(defun mas-sort-file-history ( arg)
  (setq mas-file-history
		(sort mas-file-history
			  (nth arg mas-file-history-sort-compare-func)))
  (mas-list-file-history))

;;  (setq mas-file-history (sort mas-file-history 'aaa)))
(defun mas-mark-file-history ( &optional arg)
  "Put a desired mark to buffer menu. Add space character when argument is omitted."
  (interactive)
  (beginning-of-line)
  (if (looking-at " [-N]")		;header lines
	  (ding)
	(let (( buffer-read-only nil))
	  (delete-char 1)
	  (if arg
		  (insert arg)
		(insert " "))
	  (forward-line 1))))
(defun mas-delete-not-exist-file ()
  "Delete entries if it's file  does not exist on disk."
  (interactive)
  (let ((updateflg nil)
		(list-fl mas-file-history))
	(while list-fl
	  (let* ((fl (car list-fl))
			 (name (car fl)))
		(if (file-exists-p name)
			()
		  (setq updateflg t)
		  (mas-delete-file-history name)))
	  (setq list-fl (cdr list-fl)))
	(if updateflg
		(mas-list-file-history))
  ))
(defun mas-exec-file-history ()
  "Execute all marked commands. "
  (interactive)
  (save-excursion
	(let ((menuupdateflg nil)
		  (openflg nil)
		  (name)
		  (current (current-buffer)))
	  (goto-char (point-min))
	  (forward-line 1)
	  (while (search-forward "\nD" nil t)
		(setq name (mas-get-file-name))
		(if name
			(progn
			  (mas-delete-file-history name)
			  (setq menuupdateflg t))))
	  
	  (goto-char (point-min))
	  (forward-line 1)
	  (while (search-forward "\nO" nil t)
		(mas-open-file-history)
		(set-buffer current)
		(setq openflg t))
	  
	  (goto-char (point-min))
	  (forward-line 1)
	  (while (search-forward "\nR" nil t)
		(setq name (mas-get-file-name))
		(mas-set-attr-file-history name 'R)
		(setq menuupdateflg t))
	  
	  (goto-char (point-min))
	  (forward-line 1)
	  (while (search-forward "\nE" nil t)
		(setq name (mas-get-file-name))
		(mas-set-attr-file-history name nil)
		(setq menuupdateflg t))
	  
	  (if (and menuupdateflg (not openflg))
		  (mas-list-file-history))
	  )))

(defun mas-mask-file-history ()
  "Mask file by regular expression string."
  (interactive)
  (setq mas-file-history-mask-file-regexp (read-from-minibuffer "Mask pattern (Regexp): " ))
  (mas-list-file-history)
  )
(defun mas-list-file-history ()
  "Show history menu."
  (interactive)
  (let ((old-buffer (current-buffer))
		(standard-output standard-output)
		(start-point)
		(cnt))
	(save-excursion
	 (set-buffer (get-buffer-create "*open-file-history*"))
	 (setq buffer-read-only nil)
	 (erase-buffer)
	 (setq standard-output (current-buffer))
	 (princ "\
 Num Point     Visited File
 --- --------- ------- ----
")
	 (setq cnt 1)
	 (setq mas-list-file-history-column 7)
	 (setq start-point (point))
	 (let ((list-fl mas-file-history))
	   (while list-fl
		 (let* ((fl (car list-fl))
			   (name (car fl))
			   (attr (cdr (assq 'attr (cadr fl))))
			   (pos (cdr (assq 'point (cadr fl))))
			   (visited (cdr (assq 'visited (cadr fl))))
			   (name-start)
			   (name-end))
           (if (not (string-match mas-file-history-mask-file-regexp name))
               ()
             (princ " ")
             (if ( < cnt 10)
                 (princ cnt)
               (princ " "))
             (if attr
                 (princ attr))
             (indent-to 5 1)
             (princ pos)
             (indent-to 15 1)
             (if visited
                 (princ visited)
               (princ "0"))
             (indent-to 23 1)
             (setq name-start (point))
             (princ name)
             (setq name-end (point))
             (put-text-property name-start name-end 'mas-history-file-name name)
             (princ "\n")
             (setq cnt (+ cnt 1))
             )
           (setq list-fl (cdr list-fl))
           )))
	 (vc-toggle-read-only)
	 (goto-char start-point)
	 (mas-file-history-mode)
	 (switch-to-buffer (current-buffer))
	 (message "Commands: ? for help.")
	 ))
  )
(defun mas-get-file-name ()
  "Obtain file-name of a file which is pointed by cursor"
  (beginning-of-line)
  (if (or (< (point) mas-list-file-history-start-point)
		  (eobp))
	  (error "invalid line"))
  (goto-char (next-single-property-change (point) 'mas-history-file-name))
  (let ((name (get-text-property (point) 'mas-history-file-name)))
	(if name
		name
	  ())))
  
(defun mas-open-file-history ( &optional arg)
  "Open selected file or a file which is pointed by cursor if argument is
omitted. Argument starts with 1, which stands for line number."
  (interactive)
  (if arg
	  (progn
		(goto-char mas-list-file-history-start-point)
		(if (/= arg 0)
			(next-line arg))))
  (let ((name (mas-get-file-name)))
	  (progn
		(find-file name)
		(bury-buffer "*open-file-history*")))
  )

(defun mas-file-history-find-file-hooks-func ()
  "A hook for find-file. When finding an entry in history,
move point to the place saved in history."
  (let ((pos (mas-get-point-file-history (buffer-file-name))))
	(and pos
		 (goto-char pos)))
  (let ((visited (mas-get-visited-file-history (buffer-file-name))))
	(if visited
		(mas-set-visited-file-history (buffer-file-name) (+ 1 visited))
	  ;; to compatible for ver 1.0.2
	  (if (mas-get-record-file-history( buffer-file-name))
		  (mas-set-visited-file-history (buffer-file-name) 1))))
  (mas-append-file-history))
  
(defun mas-file-history-kill-emacs-hook-func ()
  "A hook for emacs's shutdown. Save history into file when emacs is
shut down."
  (let ((bl (reverse (buffer-list))))
	(while bl
	  (let ((buffer (car bl)))
		(mas-append-file-history (buffer-name buffer))
		(setq bl (cdr bl))
	  )))
  (if mas-file-history-delete-not-exist-file-when-kill-emacs
	  (mas-delete-not-exist-file))
  (set-buffer (generate-new-buffer "*mas-file-history*" ))
  (let ((standard-output (current-buffer)))
	
	(princ "(setq mas-file-history '\n")
	(princ "(\n")

 	(let ((list-fl mas-file-history))
 	  (while list-fl
		
 		(let ((list (car list-fl)))
 		  (princ "  (\"")
		  (princ (car list))
 		  (princ "\"\n  ")
		  (let ((list-member (cdr list)))
			(while list-member
			  (princ (car list-member))
			  (setq list-member (cdr list-member)))
			)
		  (princ ")\n")
		  (setq list-fl (cdr list-fl))
		  )
		)
	  )
	(princ "))\n")
	(write-file mas-file-history-save-filename)
	(save-buffer)
	)
  )
(and mas-file-history-emacs-start-autoload-registered-files
	 (let ((list-fl mas-file-history))
	   (while list-fl
		 (let* ((fl (car list-fl))
			   (name (car fl))
			   (bufname))
		   (princ (mas-get-attr-file-history name))
		   (if (equal 'R (mas-get-attr-file-history name))
			   	 (if mas-file-history-autoload-load-as-normal-buffer
					 (find-file name )
				   (setq bufname (buffer-name (find-file-noselect name t t)))
				   (set-buffer bufname)
				   (set-visited-file-name nil t)
				   (rename-buffer (generate-new-buffer-name(concat "**" (concat bufname "**")))))))
		   (setq list-fl (cdr list-fl)))))

(add-hook 'find-file-hooks 'mas-file-history-find-file-hooks-func)
(add-hook 'kill-buffer-hook 'mas-append-file-history)
(add-hook 'kill-emacs-hook 'mas-file-history-kill-emacs-hook-func)

(and mas-file-history-emacs-start-allfile-open
	 (let ((list-fl mas-file-history))
	   (while list-fl
		 (let* ((fl (car list-fl))
			   (name (car fl))
			   (bufname))
		   (setq bufname (buffer-name (find-file-noselect name t t)))
		   (set-buffer bufname)
		   (set-visited-file-name nil t)
		   (rename-buffer (generate-new-buffer-name(concat "**" (concat bufname "**"))))
		   (setq list-fl (cdr list-fl))))))
(mas-file-history-add-menu)
(provide 'mas-file-history)
