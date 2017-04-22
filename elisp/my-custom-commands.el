(provide 'my-custom-commands)

(defun backward-kill-word-or-region (&optional arg)
  "Calls `kill-region' when a region is active and `backward-kill-word' otherwise."
  (interactive "p")
  (if (use-region-p)
      (call-interactively 'kill-region)
    (backward-kill-word arg)))
