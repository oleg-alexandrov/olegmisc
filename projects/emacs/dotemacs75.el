(setq-default abbrev-mode t)                            ; enable abbreviations
(setq save-abbrevs t)                                   ; save abbreviations upon exiting xemacs
(setq abbrev-file-name "~/.xemacs/my-abbreviations.el") ; the file storing the abbreviations
(if (file-readable-p abbrev-file-name)                  ; read the abbreviations every
  (read-abbrev-file abbrev-file-name)                   ; time xemacs is started
  )

