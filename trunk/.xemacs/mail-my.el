;;mail-my.el

(defun mail-smart-space ()
  (interactive)
  (if (expand-abbrev)
      ()
    (insert " ")
    (do-auto-fill)
    )
  )

(defun my-exit-mail ()
  (interactive)
  (save-buffer)
  (gnuserv-edit)
  )



(defun my-delete-tail ()
  (interactive)
  (let ((beg (point)))
    (goto-char (point-max))
    (delete-region beg (point)))
  (beginning-of-buffer)
  (goto-line 1)
  )


 (defun start-message ()
   (interactive)
   (flyspell-mode 1) ;; underline spelling mistakes

   (auto-fill-mode 1)
   (setq fill-column 72)

   ; sometimes I want to replace the default Mutt "Hi somebody" with something more customized
   (goto-char (point-min))
   (if (search-forward "," nil t nil)
        (backward-char 1)
      )
    (expand-abbrev)
   

;; the case of double comma
    (if (looking-at ",,") (delete-char 1)) 

   )

; ; ;(local-set-key [(control d)] 'date)
; (local-set-key [(space)] 'mail-smart-space)
; (local-set-key [(control j)] 'fill-paragraph)
; (local-set-key [(control return)] 'my-exit-mail)
; (local-set-key [(control \')] 'my-delete-tail)
; (local-set-key [(shift down)] 'pc-select-mark-line-down)
; (local-set-key [(meta \')] 'mutt-delete-old-citations)

(start-message)