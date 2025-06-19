(provide 'my-custom-commands)

(defun backward-kill-word-or-region (&optional arg)
  "Calls `kill-region' when a region is active and `backward-kill-word' otherwise."
  (interactive "p")
  (if (use-region-p)
      (call-interactively 'kill-region)
    (backward-kill-word arg)))

;; Source: https://www.emacswiki.org/emacs/misc-cmds.el
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))
