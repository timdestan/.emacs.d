;; TODO: This causes issues if there is another Emacs running.
(server-start)

(setq
 inhibit-startup-screen t
 create-lockfiles nil
 make-backup-files nil
 column-number-mode t
 scroll-error-top-bottom t
 show-paren-delay 0.5
 use-package-always-ensure t
 sentence-end-double-space nil

 c-basic-indent 2
 tab-width 2
 tab-stop-list (number-sequence 2 200 2)
 indent-line-function 'insert-tab

 ;; Mac-specific options.
 mac-option-key-is-meta t
 mac-command-key-is-meta nil)

(if window-system
    (add-to-list 'default-frame-alist '(fullscreen . maximized)))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(set-keyboard-coding-system nil)

(set-face-attribute 'default nil :height 160)

;; Free up screen real estate.
(if (functionp 'menu-bar-mode)
    (menu-bar-mode -1))
(if (functionp 'tool-bar-mode)
    (tool-bar-mode -1))

;; Pretty good, until I find something better.
(ido-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wombat)))
 '(indent-tabs-mode nil)
 '(package-selected-packages
   (quote
    (haskell-mode rainbow-delimiters git-gutter exec-path-from-shell use-package)))
 '(standard-indent 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defalias 'qrr 'query-replace-regexp)

;; the package manager
(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; On OSX, windowed Emacs does not get same environment variables as
;; shell Emacs. This ensures that the $PATH variable is set correctly.
(when (memq window-system '(mac ns))
  (use-package "exec-path-from-shell")
  (exec-path-from-shell-initialize))

(use-package "git-gutter")
(global-git-gutter-mode +1)

(use-package "rainbow-delimiters")
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Load tuareg site file. Add opam emacs directory to the load-path
;; TODO: This should probably be conditional. Opam/Merlin may not be installed.

(setq opam-share
      (substring
       (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
(load (concat opam-share "/emacs/site-lisp/tuareg-site-file"))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))

;; Load merlin-mode
(require 'merlin)
;; Start merlin on ocaml files
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'caml-mode-hook 'merlin-mode t)

;; Enable auto-complete
(setq merlin-use-auto-complete-mode 'easy)
;; Use opam switch to lookup ocamlmerlin binary
(setq merlin-command 'opam)

;; Enable OCaml indenting.
(require 'ocp-indent)
