(provide 'my-custom-commands)

(defun backward-kill-word-or-region (&optional arg)
  "Calls `kill-region' when a region is active and `backward-kill-word' otherwise."
  (interactive "p")
  (if (use-region-p)
      (call-interactively 'kill-region)
    (backward-kill-word arg)))

(defun reload-theme ()
  "Reloads the current theme to reflect any changes made since it was loaded."
  (interactive)
  (if (null custom-enabled-themes)
      (message "No theme is currently enabled")
    (let ((current-theme (car custom-enabled-themes)))
      (message "Reloading theme: %s" current-theme)
      (disable-theme current-theme)
      (load-theme current-theme t))))

;; (global-set-key (kbd "<f5>") #'reload-theme)
