;;; bluegreen-theme.el --- bluegreen theme

;;; Code:

(deftheme bluegreen
  "bluegreen theme")

(custom-theme-set-faces
 'bluegreen

 ;; Core

 '(default           ((t (:background "#042328" :foreground "#A8A88D"))))
 '(cursor            ((t (:background "#8ADF90"))))
 '(region            ((t (:background "#000FFF"))))
 '(mouse             ((t (:background "Grey"))))
 '(fringe            ((t (:background "#1e1e2e"))))

 ;; '(highlight                  ((t (:background "#313244"))))
 ;; '(hl-line                    ((t (:background "#181825"))))

 ;; Font lock

 '(font-lock-keyword-face       ((t (:foreground "#B3C4C5"))))
 '(font-lock-function-name-face ((t (:foreground "#A8A88D"))))
 '(font-lock-variable-name-face ((t (:foreground "#A8A88D"))))
 '(font-lock-constant-face      ((t (:foreground "#73AAA9"))))
 '(font-lock-type-face          ((t (:foreground "#6C9585"))))
 '(font-lock-string-face        ((t (:foreground "#2DA697"))))
 '(font-lock-comment-face       ((t (:foreground "#54ce62"))))

 ;; TODO: font-lock-warning-face

 ;; Mode line

 '(mode-line                  ((t (:foreground "#091405" :background "#D6B58A"))))
 '(mode-line-inactive         ((t (:foreground "#091405" :background "#7f6b52"))))

 ;; Search

 '(isearch                    ((t (:foreground "#1e1e2e" :background "#f9e2af"))))
 '(lazy-highlight             ((t (:foreground "#1e1e2e" :background "#fab387"))))

 ;; Line numbers

 '(line-number                ((t (:foreground "#45475a"))))
 '(line-number-current-line   ((t (:foreground "#cba6f7" :bold t))))

 ;; Parens

 '(show-paren-match           ((t (:foreground "#1e1e2e" :background "#cba6f7" :weight bold))))
)

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'bluegreen)

;;; bluegreen-theme.el ends here
