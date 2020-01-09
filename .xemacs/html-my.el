;;html-my.el
(hm--html-mode)

(local-set-key [(control x) (b)] (lambda () (interactive) (insert "<b></b>")
				   (search-backward "<"))) 

(local-set-key [(control x) (i)] (lambda () (interactive) (insert "<i></i>")
				   (search-backward "<"))) 

(local-set-key [(control x) (t)] (lambda () (interactive) (insert "<tt></tt>")
				   (search-backward "<"))) 

(local-set-key [(control x) (p)] (lambda () (interactive) (insert "<pre></pre>")
				   (search-backward "<"))) 

(local-set-key [(control x) (h)] (lambda () (interactive) (insert "<head></head>")
				   (search-backward "<"))) 

(local-set-key [(control x) (l)] (lambda () (interactive) (insert "<a href=\"\"></a>")
				   (search-backward "\">")))

(local-set-key [(control x) (c)] (lambda () (interactive) (insert "<center></center>")
				   (search-backward "<"))) 

(local-set-key [(control x) (j)] (lambda () (interactive) (insert "<img src=\"\" border=\"0\" />")
				   (search-backward "\" border")))

(local-set-key [(control x) (s)] (lambda () (interactive) (insert "&nbsp;")))

(defun smart-space ()
  (interactive)
  (if (expand-abbrev)
      ()
    (insert " ")
    )
  )

(local-set-key [(control b)] 'byte-compile-and-load-file)
(local-set-key [(control backspace)] 'backward-kill-line)
(local-set-key [(control delete)] 'kill-line)
(local-set-key [(control j)]  'fill-paragraph)
(local-set-key [(control l)] 'load-file)
(local-set-key [(control o)] 'find-file)
(local-set-key [(control r)]  'query-replace)
(local-set-key [(control s)] 'save-buffer)
(local-set-key [(control t)] 'ispell-buffer)
(local-set-key [(control u)] 'yank)
(local-set-key [(meta \9)]  'my-insert-parantheses)
(local-set-key [(meta c)] 'comment-region)
(local-set-key [(meta q)] 'kill-this-buffer)
(local-set-key [(meta space)] 'dabbrev-expand)
(local-set-key [(meta u)] 'un-comment-region)
(local-set-key [(meta z)] 'redo)
(local-set-key [(space)] 'smart-space)

