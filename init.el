(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(require 'use-package)

(setq use-package-always-ensure t
      use-package-verbose t)

(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "/init.d")))

(load-library "keyboard-navigation.init")
(load-library "window-management.init")
(load-library "shell.init")
(load-library "compilation.init")
(load-library "helm.init")
(load-library "git.init")
(load-library "projectile.init")
(load-library "prodigy.init")
(load-library "programming.init")
(load-library "web.init")
(load-library "lisp.init")
(load-library "clojure.init")
(load-library "docker.init")
(load-library "coffee.init")
(load-library "ruby.init")
(load-library "sql.init")
(load-library "typescript.init")
(load-library "org.init")

(use-package display-line-numbers
  :no-require t
  :ensure nil
  :hook ((prog-mode . display-line-numbers-mode)
         (yaml-mode . display-line-numbers-mode)))

(use-package display-fill-column-indicator
  :no-require t
  :ensure nil
  :hook ((prog-mode . display-fill-column-indicator-mode)
         (yaml-mode . display-fill-column-indicator-mode))
  :config
  (set-face-attribute
   'fill-column-indicator nil :foreground "#3e6a44a85124"))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "CDPATH"))

(use-package site-environment
  :no-require t
  :ensure nil
  :config
  (setenv "PAGER" "/bin/cat"))

(use-package window-management
  :no-require t
  :ensure nil
  :config
  (setq frame-resize-pixelwise t))

(use-package composite
  :no-require t
  :ensure nil
  :config
  (dolist (hook `(ediff-mode-hook
                  mu4e-headers-mode-hook
                  package-menu-mode-hook))
    (add-hook hook (lambda () (setq-local auto-composition-mode nil))))

  ;; support ligatures, some toned down to prevent hang
  (when (version<= "27.0" emacs-version)
    (let ((alist
           '((33 . ".\\(?:\\(==\\|[!=]\\)[!=]?\\)")
             (35 . ".\\(?:\\(###?\\|_(\\|[(:=?[_{]\\)[#(:=?[_{]?\\)")
             (36 . ".\\(?:\\(>\\)>?\\)")
             (37 . ".\\(?:\\(%\\)%?\\)")
             (38 . ".\\(?:\\(&\\)&?\\)")
             (42 . ".\\(?:\\(\\*\\*\\|[*>]\\)[*>]?\\)")
             ;; (42 . ".\\(?:\\(\\*\\*\\|[*/>]\\).?\\)")
             (43 . ".\\(?:\\([>]\\)>?\\)")
             ;; (43 . ".\\(?:\\(\\+\\+\\|[+>]\\).?\\)")
             (45 . ".\\(?:\\(-[->]\\|<<\\|>>\\|[-<>|~]\\)[-<>|~]?\\)")
             (46 . ".\\(?:\\(\\.[.<]\\|[-.=]\\)[-.<=]?\\)")
             (47 . ".\\(?:\\(//\\|==\\|[=>]\\)[/=>]?\\)")
             ;; (47 . ".\\(?:\\(//\\|==\\|[*/=>]\\).?\\)")
             (48 . ".\\(?:\\(x[a-fA-F0-9]\\).?\\)")
             (58 . ".\\(?:\\(::\\|[:<=>]\\)[:<=>]?\\)")
             (59 . ".\\(?:\\(;\\);?\\)")
             (60 . ".\\(?:\\(!--\\|\\$>\\|\\*>\\|\\+>\\|-[-<>|]\\|/>\\|<[-<=]\\|=[<>|]\\|==>?\\||>\\||||?\\|~[>~]\\|[$*+/:<=>|~-]\\)[$*+/:<=>|~-]?\\)")
             (61 . ".\\(?:\\(!=\\|/=\\|:=\\|<<\\|=[=>]\\|>>\\|[=>]\\)[=<>]?\\)")
             (62 . ".\\(?:\\(->\\|=>\\|>[-=>]\\|[-:=>]\\)[-:=>]?\\)")
             (63 . ".\\(?:\\([.:=?]\\)[.:=?]?\\)")
             (91 . ".\\(?:\\(|\\)|?\\)")
             ;; (92 . ".\\(?:\\([\\n]\\)[\\]?\\)")
             (94 . ".\\(?:\\(=\\)=?\\)")
             (95 . ".\\(?:\\(|_\\|[_]\\)_?\\)")
             (119 . ".\\(?:\\(ww\\)w?\\)")
             (123 . ".\\(?:\\(|\\).?\\)")
             (124 . ".\\(?:\\(->\\|=>\\||[-=>]\\||||*>\\|[]=>|}-]\\).?\\)")
             (126 . ".\\(?:\\(~>\\|[-=>@~]\\).?\\)"))))
      (dolist (char-regexp alist)
        (set-char-table-range composition-function-table (car char-regexp)
                              `([,(cdr char-regexp) 0 font-shape-gstring])))))

  (global-auto-composition-mode))

(use-package atom-one-dark-theme
  :config
  (load-theme 'atom-one-dark t))

(use-package site-editor
  :no-require t
  :ensure nil
  :config
  (setq inhibit-startup-screen t)
  (setq kill-ring-max 1000)
  (setq require-final-newline t)
  (setq make-backup-files nil)
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t)))
  (setq create-lockfiles nil)
  (setq scroll-conservatively 1000)

  (setq site-frame-settings '((menu-bar-lines . 0)
                              (tool-bar-lines . 0)
                              (vertical-scroll-bars . nil)
                              (fullscreen . maximized)))
  (setq default-frame-alist site-frame-settings)
  (setq initial-frame-alist site-frame-settings)

  (setq-default indent-tabs-mode nil)
  (setq-default fill-column 100)
  (setq-default line-spacing 2)

  (fringe-mode '(12 . 12))
  (global-hl-line-mode 1)
  (blink-cursor-mode -1)

  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-language-environment 'utf-8)
  (set-selection-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-input-method nil)

  (set-face-attribute
   'default nil :family "Cascadia Code PL" :foreground "#cccccc" :height 95)

  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024))

  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(use-package site-minibuffer
  :no-require t
  :ensure nil
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)
  (add-hook 'minibuffer-setup-hook #'subword-mode))

(use-package add-node-modules-path
  :commands add-node-modules-path
  :hook js2-mode)

;;; Avy

(use-package avy
  :bind (:map isearch-mode-map
              ("C-'" . avy-isearch))
  :config
  (setq avy-style 'at-full)
  (setq avy-timeout-seconds 0.5)
  (setq avy-highlight-first t)
  (setq avy-all-windows nil)

  (avy-setup-default))

;;; Company

(use-package company
  :hook (after-init . global-company-mode)
  :config
  (push 'company-yasnippet company-backends))

;;; Evil

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

(use-package emacs-lisp
  :no-require t
  :ensure nil
  :hook ((emacs-lisp-mode . smartparens-mode)
         (emacs-lisp-mode . eldoc-mode)))

(use-package emmet-mode)

(use-package feature-mode
  :config
  (general-define-key
   :prefix "M-SPC"
   :keymaps 'feature-mode-map

   "cta" '(feature-verify-all-scenarios-in-project
           :whick-key "run scenarios in project")
   "cts" '(feature-verify-scenario-at-pos :which-key "run scenario under cursor")
   "ctt" '(feature-verify-all-scenarios-in-buffer
           :which-key "run scenarios in buffer")))

(use-package flycheck)

(use-package flycheck-jest
  :disabled
  :after flycheck
  :config
  (flycheck-jest-setup))

(use-package highlight-indent-guides
  :hook ((prog-mode . highlight-indent-guides-mode)
         (yaml-mode . highlight-indent-guides-mode))
  :config
  (setq highlight-indent-guides-method 'character))

(use-package indium
  :config
  (setq indium-chrome-executable "/usr/bin/google-chrome"))

(use-package java-mode
  :no-require t
  :ensure nil
  :after general
  :init
  (general-define-key
   :states 'insert
   :keymaps '(java-mode-map)
   ";" 'ior3k-insert-semicolon-at-eol)
  (global-def
    :keymaps 'java-mode-map

    "ci" '(lsp-java-add-import :which-key "code actions"))

  (defun ior3k-java-settings ()
    (setq c-basic-offset 4
          fill-column 100
          tab-width 4
          evil-shift-width 4)

    (c-set-offset 'statement-cont '+)
    (c-set-offset 'arglist-intro '+)
    (c-set-offset 'arglist-close 0))

  (add-hook 'java-mode-hook 'smartparens-mode)
  (add-hook 'java-mode-hook 'subword-mode)
  (add-hook 'java-mode-hook 'ior3k-java-settings)
  (add-hook 'java-mode-hook 'auto-revert-mode))

(use-package js2-mode
  :commands js2-mode
  :bind
  (:map js2-mode-map
   ([remap js-find-symbol] . xref-find-definitions))
  :config
  (setq js-indent-level 2)
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  (setq js2-bounce-indent-p t))

(use-package lsp-java
  :after lsp
  :config
  (setq lsp-java-organize-imports nil)
  (setq lsp-java-save-action-organize-imports nil)

  (add-hook 'java-mode-hook 'lsp)
  (add-hook 'java-mode-hook 'flycheck-mode))
                                        ;(add-hook 'java-mode-hook 'lsp-ui-mode))

(use-package prettier-js
  :after (js2-mode rjsx-mode)
  :hook ((js2-mode . prettier-js-mode)
         (rjsx-mode . prettier-js-mode)))

(use-package rainbow-mode
  :after rjsx-mode
  :hook rjsx-mode)

(use-package reason-mode
  :after lsp-mode
  :config
  (setq refmt-command 'npm)

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection
                                     "/home/david/.emacs.d/rls-linux/reason-language-server")
                    :major-modes '(reason-mode)
                    :notification-handlers (ht ("client/registerCapability" 'ignore))
                    :priority 1
                    :server-id 'reason-ls))

  (add-hook 'reason-mode-hook (lambda () (add-hook 'before-save-hook #'refmt-before-save)))
  (add-hook 'reason-mode-hook #'lsp)
  (add-hook 'reason-mode-hook (lambda () (sp-pair "`" nil :actions :rem))))

(use-package rjsx-mode
  :mode "\\.js\\'")

(use-package rubocop)

(use-package sh-mode
  :no-require t
  :ensure nil
  :config
  (setq sh-basic-offset 2))

(use-package smartparens
  :after general
  :init (require 'smartparens-config)
  :hook ((smartparens-mode . sp-use-smartparens-bindings)
         (rjsx-mode . (lambda () (require 'smartparens-javascript))))
  :config
  (setq sp-escape-quotes-after-insert nil)
  (setq sp-escape-wrapped-region nil)

  (general-define-key
   :states '(insert normal motion)
   :keymaps 'smartparens-mode-map

   "C-l" 'sp-forward-slurp-sexp
   "C-h" 'sp-forward-barf-sexp)

  (show-smartparens-global-mode 1))

(use-package telephone-line
  :config
  (setq telephone-line-evil-use-short-tag t)
  (setq telephone-line-lhs
        '((evil   . (telephone-line-airline-position-segment))
          (accent . (telephone-line-projectile-segment))
          (nil    . (telephone-line-buffer-modified-segment
                     telephone-line-buffer-name-segment))))
  (setq telephone-line-rhs
        '((nil   .  (telephone-line-flycheck-segment))
          (accent . (telephone-line-process-segment))
          (evil    . (telephone-line-evil-tag-segment))))

  (telephone-line-mode 1))

(use-package terraform-mode)

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package wgrep)

(use-package which-key
  :delight
  :config
  (which-key-mode 1))

(use-package xterm-color)

(use-package yaml-mode)

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package projects
  :no-require t
  :ensure nil
  :config
  (load-file "~/.emacs.d/projects.el"))

;;; Elixir

(use-package elixir-mode
  :after alchemist
  :after (lsp-mode)
  :hook (elixir-mode . lsp)
  :init
  (add-to-list 'exec-path "/home/david/.emacs.d/elixir-ls/server")
  :config
  (add-hook 'elixir-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'elixir-format nil t)
              (global-def
                :keymap 'local
                "pc" '(alchemist-iex-project-run :which-key "run iex")))))

(use-package alchemist)

(defun delete-current-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' removed" filename)))))

(defun er-rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(slim-mode edn org-mode git-gutter-fringe git-gutter tide typescript-mode helm-cider clj-refactor cider-eval-sexp-fu wgrep-helm web-mode coffee-mode sql-indent lispyville composite cider clojure-mode aggressive-indent aggressive-indent-mode alchemist helm-lsp helm-projectile helm-ls-git helm lsp-treemacs elixir-mode elixir-ls elixir-lsp flycheck-elixir reason-mode terraform-mode docker dockerfile-mode rubocop evil-mc indium flycheck yasnippet yaml-mode xterm-color which-key wgrep use-package telephone-line smartparens rjsx-mode rainbow-mode rainbow-delimiters projectile-rails prodigy prettier-js lsp-ui lsp-java json-mode highlight-indent-guides general feature-mode exec-path-from-shell evil-surround evil-matchit evil-magit evil-exchange evil-collection evil-args enh-ruby-mode emmet-mode dap-mode avy atom-one-dark-theme add-node-modules-path))
 '(safe-local-variable-values
   '((eval setq sql-mysql-program
           (string-trim-right
            (shell-command-to-string "asdf which mysql")))
     (eval
      (setq sql-mysql-program
            (string-trim-right
             (shell-command-to-string "asdf which mysql"))))
     (cider-shadow-default-options . ":app")
     (cider-default-cljs-repl . shadow))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(server-start)
