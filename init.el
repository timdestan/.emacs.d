;; TODO: This causes issues if there is another Emacs running.
(server-start)

(add-to-list 'load-path "~/.emacs.d/elisp/")

(require 'my-custom-commands)

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
 rust-indent-offset 2
 tab-width 2
 tab-stop-list (number-sequence 2 200 2)
 indent-line-function 'insert-tab

 ;; This works fine in some modes, but causes Emacs to insert a tab character
 ;; after each return for any file type where it doesn't understand the proper
 ;; syntax.
 electric-indent-mode nil

 ;; Mac-specific options.
 mac-option-key-is-meta t
 mac-command-key-is-meta nil)

(if window-system
    (add-to-list 'default-frame-alist '(fullscreen . maximized)))

;; Not sure why this is useful.
(set-keyboard-coding-system nil)

(set-face-attribute 'default nil :height 160)

(setq ring-bell-function 'ignore)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wombat)))
 '(indent-tabs-mode nil)
 '(package-selected-packages
   (quote
    (flycheck lsp-mode mwim markdown-mode rust-mode haskell-mode rainbow-delimiters git-gutter exec-path-from-shell use-package)))
 '(standard-indent 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Branched from better-defaults.el
(progn
  (ido-mode t)
  (setq ido-enable-flex-matching t)

  (menu-bar-mode -1)
  (if window-system
      (menu-bar-mode 1))
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

  (autoload 'zap-up-to-char "misc"
    "Kill up to, but not including ARGth occurrence of CHAR." t)

  (require 'uniquify)
  (setq uniquify-buffer-name-style 'forward)

  (require 'saveplace)
  (setq-default save-place t)

  (global-set-key (kbd "M-/") 'hippie-expand)
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (global-set-key (kbd "M-z") 'zap-up-to-char)

  (global-set-key (kbd "C-s") 'isearch-forward-regexp)
  (global-set-key (kbd "C-r") 'isearch-backward-regexp)
  (global-set-key (kbd "C-M-s") 'isearch-forward)
  (global-set-key (kbd "C-M-r") 'isearch-backward)

  (show-paren-mode 1)
  (setq-default indent-tabs-mode nil)
  (setq x-select-enable-clipboard t
	x-select-enable-primary t
	save-interprogram-paste-before-kill t
	apropos-do-all t
	mouse-yank-at-point t
	require-final-newline t
	visible-bell t
	load-prefer-newer t
	ediff-window-setup-function 'ediff-setup-windows-plain
	save-place-file (concat user-emacs-directory "places")
	backup-directory-alist `(("." . ,(concat user-emacs-directory
						 "backups")))))


(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq-default major-mode 'markdown-mode)

;; Trying out some suggestions from Steve Yegge's effective emacs:
;; https://sites.google.com/site/steveyegge2/effective-emacs

(defalias 'qrr 'query-replace-regexp)

(global-set-key [remap move-beginning-of-line] #'mwim-beginning-of-code-or-line)
(global-set-key [remap move-end-of-line] #'mwim-end-of-code-or-line)

(global-set-key (kbd "C-w") 'backward-kill-word-or-region)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-region)

(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)

(global-set-key (kbd "C-h C-f") 'describe-function)
(global-set-key (kbd "C-h C-v") 'describe-variable)

(global-unset-key (kbd "s-^"))   ;; kill-some-buffers
(global-unset-key (kbd "C-h h")) ;; view-hello-file

;; the package manager
(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))

(package-initialize)
;; TODO: This sometimes causes problems when cached versions are too old. But
;; maybe don't want to slow down startup by checking every time.
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

(eval-when-compile
  (require 'use-package))

;; Conditionally load Google stuff.
(if (file-exists-p "~/.emacs.d/google/init.el")
    (progn
      (add-to-list 'load-path "~/.emacs.d/google/")
      (load-file "~/.emacs.d/google/init.el"))
  (message "skipping google"))

;; Open .v files with Proof General's Coq mode
;;
;; Make it optional, so that it works on a machine
;; that hasn't activated the submodule.
(if (file-directory-p "~/.emacs.d/elisp/PG/generic")
    (load "~/.emacs.d/elisp/PG/generic/proof-site"))

;; On OSX, windowed Emacs does not get same environment variables as
;; shell Emacs. This ensures that the $PATH variable is set correctly.
(when (memq window-system '(mac ns))
  (use-package "exec-path-from-shell")
  (exec-path-from-shell-initialize))

;; According to people on the internet, ligatures may not work except on Mac OS.
;; I have neither confirmed nor denied this myself.
;; Code is from https://github.com/tonsky/FiraCode/wiki/Setting-up-Emacs
(when (memq window-system '(mac ns))
  (if (null (x-list-fonts "Fira Code"))
      (message "Fira Code not found. You may want to install it.")
      (set-default-font "Fira Code"))
  (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
                 (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
                 (36 . ".\\(?:>\\)")
                 (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
                 (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                 (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
                 (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
                 (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
                 (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
                 (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
                 (48 . ".\\(?:x[a-zA-Z]\\)")
                 (58 . ".\\(?:::\\|[:=]\\)")
                 (59 . ".\\(?:;;\\|;\\)")
                 (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
                 (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
                 (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
                 (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
                 (91 . ".\\(?:]\\)")
                 (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
                 (94 . ".\\(?:=\\)")
                 (119 . ".\\(?:ww\\)")
                 (123 . ".\\(?:-\\)")
                 (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
                 (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
                 )
               ))
    (dolist (char-regexp alist)
      (set-char-table-range composition-function-table (car char-regexp)
                            `([,(cdr char-regexp) 0 font-shape-gstring])))))

(use-package "git-gutter"
  :config
  (global-git-gutter-mode +1))

(use-package "rainbow-delimiters"
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package flycheck
  :hook (prog-mode . flycheck-mode))

(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;; Load tuareg site file. Add opam emacs directory to the load-path
;; TODO: Make these conditional on opam being installed and add them back

;; (setq opam-share
;;       (substring
;;        (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
;; (load (concat opam-share "/emacs/site-lisp/tuareg-site-file"))
;; (add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))

;; Load merlin-mode
;; (require 'merlin)
;; Start merlin on ocaml files
;; (add-hook 'tuareg-mode-hook 'merlin-mode t)
;; (add-hook 'caml-mode-hook 'merlin-mode t)

;; Enable auto-complete
;; (setq merlin-use-auto-complete-mode 'easy)
;; Use opam switch to lookup ocamlmerlin binary
;; (setq merlin-command 'opam)

;; Enable OCaml indenting.
;; (require 'ocp-indent)
