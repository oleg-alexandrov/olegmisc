(load-file "~/.xemacs/mas-file-history.el") ; creates a menu with a list of recently opened files
(setq mas-file-history-menu-path nil)       ; put the menu at the top of the XEmacs window
(setq mas-file-history-menu-title "History"); the name of the menu
(with-temp-buffer)                          ; this hack seems to be necessary

