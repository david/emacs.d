(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(require 'use-package)

(setq use-package-always-ensure t
      use-package-verbose t)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "CDPATH"))

(use-package site-environment
  :no-require t
  :ensure nil
  :config
  (setenv "PAGER" "/bin/cat"))

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
  (setq scroll-conservatively 1000)

  (setq site-frame-settings '((menu-bar-lines . 0)
                              (tool-bar-lines . 0)
                              (vertical-scroll-bars . nil)
                              (fullscreen . maximized)
                              (alpha . 95)))
  (setq default-frame-alist site-frame-settings)
  (setq initial-frame-alist site-frame-settings)

  (setq-default indent-tabs-mode nil)
  (setq-default fill-column 80)
  (setq-default line-spacing 2)

  (fringe-mode '(12 . 12))
  (global-hl-line-mode 1)
  (blink-cursor-mode -1)
  (show-paren-mode 1)

  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-language-environment 'utf-8)
  (set-selection-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-input-method nil)

  (set-face-attribute
   'default nil :font "Fira Code" :foreground "#aaaaaa" :height 120)

  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(use-package site-modeline
  :no-require t
  :ensure nil
  :config
  (column-number-mode 1)
  (line-number-mode 1)
  (defalias 'yes-or-no-p 'y-or-n-p))

(use-package site-minibuffer
  :no-require t
  :ensure nil
  :bind
  (:map minibuffer-local-map
   ("C-w" . backward-kill-word)))

(use-package add-node-modules-path
  :commands add-node-modules-path
  :hook js2-mode)

(use-package avy
  :bind ("C-'" . avy-goto-char-timer)
  :config
  (setq avy-style 'at-full)
  (setq avy-timeout-seconds 0.5)
  (setq avy-highlight-first t)
  (setq avy-all-windows nil)

  (avy-setup-default))

(use-package company
  :commands company-mode
  :config
  (company-tng-configure-default))

(use-package company-lsp
  :after company
  :config
  (setq company-lsp-enable-snippet nil
        company-lsp-cache-candidates nil)
  (push 'company-lsp company-backends)
  (push 'java-mode company-global-modes))

(use-package counsel
  :after (ivy)
  :config
  (general-define-key
   :states 'insert
   :keymaps 'counsel-find-file-map
   "C-l" 'counsel-up-directory)

  (counsel-mode 1))

(use-package counsel-projectile
  :after (projectile))

(use-package dimmer
  :config
  (setq dimmer-fraction 0.5)
  (dimmer-mode 1))

(use-package emacs-lisp
  :no-require t
  :ensure nil
  :hook ((emacs-lisp-mode . smartparens-mode)
         (emacs-lisp-mode . turn-on-eldoc-mode)
         (emacs-lisp-mode . company-mode)))

(use-package emmet-mode)

(use-package enh-ruby-mode
  :after (smartparens)
  :mode "\\.rb\\'"
  :config
  (setq enh-ruby-add-encoding-comment-on-save nil)
  (setq enh-ruby-deep-indent-paren nil)
  (setq enh-ruby-deep-indent-construct nil))

(use-package eshell
  :after (xterm-color)
  :config
  (setq eshell-hist-ignoredups t)
  (setq eshell-history-size 65456)
  (setq eshell-prompt-regexp "^[0-9:]+ • .* • ")
  (setq eshell-highlight-prompt nil)
  (setq ior3k-projects-dir (concat (getenv "HOME") "/projects/"))

  (defun ior3k-project-name (dir)
    (when (string-match ior3k-projects-dir dir)
      (car
       (split-string (replace-regexp-in-string ior3k-projects-dir "" dir) "/"))))

  (defun eshell-prompt-func ()
    (let* ((dir (eshell/pwd))
	   (time (format-time-string "%H:%M:%S" (current-time)))
	   (project (ior3k-project-name dir))
	   (child-dir (file-name-nondirectory dir))
	   (no-project-dir (replace-regexp-in-string (getenv "HOME") "~" dir)))
      (atom-one-dark-with-color-variables
        (concat
         time
         " • "
         (propertize
          (if project project no-project-dir)
          'face `(:foreground ,atom-one-dark-orange-1))
         (if (and project (not (equal project child-dir)))
             (concat " [" child-dir "]"))
         (propertize
          " •"
          'face `(:foreground ,atom-one-dark-fg))
         " "))))

  (setq eshell-prompt-function 'eshell-prompt-func)

  (defun eshell-mode-hook-func ()
    (let ((path (mapconcat 'identity exec-path ":")))
      (setq eshell-path-env path)
      (setenv "PATH" path))

    (setq xterm-color-preserve-properties t)

    (add-to-list 'eshell-visual-commands "ssh"))

  (add-hook 'eshell-mode-hook 'eshell-mode-hook-func)
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)

  (setq eshell-output-filter-functions
	(remove 'eshell-handle-ansi-color eshell-output-filter-functions)))

(use-package evil
  :init
  (setq evil-want-integration nil)
  (setq evil-want-C-u-scroll t)
  :config
  (setq-default evil-shift-width 2)

  (evil-mode 1))

(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package evil-exchange
  :config
  (evil-exchange-install))

(use-package evil-magit
  :after (magit))

(use-package evil-matchit
  :after (evil)
  :config
  (global-evil-matchit-mode 1))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package flycheck-flow)

(use-package flow-minor-mode
  :after (add-node-modules-path flycheck-flow)
  :hook ((js2-mode . flow-minor-enable-automatically)
         (js2-mode . flycheck-mode))
  :config
  (flycheck-add-mode 'javascript-flow 'flow-minor-mode)
  (flycheck-add-mode 'javascript-eslint 'flow-minor-mode)
  (flycheck-add-next-checker 'javascript-eslint 'javascript-flow))

(use-package general
  :config
  (general-create-definer ior3k-local-leader-def
    :prefix "C-; m")

  (general-define-key
   "C-w" 'backward-kill-word
   "C-x w" 'kill-region
   "s-k" 'windmove-up
   "s-j" 'windmove-down
   "s-l" 'windmove-right
   "s-h" 'windmove-left)

  (general-define-key
   :states 'normal

   "+" 'universal-argument
   "q" 'quit-window)

  (general-define-key
   :prefix "C-;"

   "C-;" 'counsel-M-x

   "b"  '(:ignore t :which-key "buffers")
   "bb" '(ivy-switch-buffer :which-key "switch to buffer")
   "bd" '(kill-this-buffer :which-key "kill current buffer")
   "bs" '(save-buffer :which-key "save buffer")
   "bw" '(write-file :which-key "write to file")

   "c"  '(:ignore t :which-key "code")
   "ce" '(eval-last-sexp :which-key "eval last code block")
   "ci" '(counsel-imenu :which-key "imenu")
   "cm" '(comment-dwim :which-key "comment dwim")

   "cS" '(:ignore t :which-key "snippets")
   "cSn" '(yas-new-snippet :which-key "new snippet")
   "cSS" '(yas-visit-snippet-file :which-key "find snippet file")
   "cSi" '(yas-insert-snippet :which-key "insert snippet")

   "ct" '(projectile-toggle-between-implementation-and-test
          :which-key "toggle test file")
   "cv" '(minitest-verify :which-key "run tests in file")

   "e"  '(:ignore t :which-key "emacs")
   "em" '(evil-record-macro :which-key "record macro")
   "ep" '(package-list-packages :which-key "list packages")
   "eq" '(save-buffers-kill-terminal :which-key "quit emacs")
   "es" '(prodigy :which-key "servers")

   "f"  '(:ignore t :which-key "files")
   "fd" '(delete-current-file :which-key "delete current file")
   "fe" '(dired :which-key "dired")
   "ff" '(counsel-find-file :which-key "find file")
   "fi" '(insert-file :which-key "insert file contents")
   "fv" '(find-alternate-file :which-key "find alternate file")

   "h"  '(:ignore t :which-key "help")
   "hf" '(counsel-describe-function :which-key "describe function")
   "hk" '(describe-key :which-key "describe key")
   "hm" '(describe-mode :which-key "describe mode")
   "hv" '(counsel-describe-variable :which-key "describe variable")

   "p"  '(:ignore t :which-key "projects")
   "pb" '(counsel-projectile :which-key "switch to buffer of file")
   "pc" '(projectile-rails-console :which-key "run console")
   "pe" '(projectile-dired :which-key "dired in project root")
   "pf" '(counsel-projectile-find-file :which-key "find file in project")
   "ph" '(projectile-run-eshell
          :which-key "shell in project root")
   "pp" '(counsel-projectile-switch-project :which-key "switch project")
   "ps" '(counsel-projectile-ag :which-key "search in project")
   "pv" '(magit-status :which-key "git status")

   "w"  '(:ignore t :which-key "windows")
   "wb" '(split-window-below :which-key "open new window below")
   "wd" '(delete-window :which-key "delete window")
   "wk" '(delete-other-windows :which-key "keep this window only")
   "wr" '(split-window-right :which-key "open new window to the right")))

(use-package ivy
  :config
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "")

  (ivy-mode 1))

(use-package java-mode
  :no-require t
  :ensure nil
  :after general
  :hook ((java-mode . flycheck-mode)
         (java-mode . smartparens-mode)
         (java-mode . subword-mode)
         (java-mode . company-mode)
         (java-mode . auto-revert-mode))
  :init
  (ior3k-local-leader-def
   :states 'normal
   :keymaps 'java-mode-map

   "a" '(lsp-execute-code-action :which-key "code actions"))

  (defun ior3k-insert-semicolon-at-eol ()
    (interactive)
    (end-of-line)
    (self-insert-command 1))

  (general-define-key
   :states 'insert
   :keymaps 'java-mode-map
   ";" 'ior3k-insert-semicolon-at-eol)

  (defun ior3k-java-settings ()
    (setq c-basic-offset 4
          fill-column 100
          tab-width 4
          evil-shift-width 4)

    (c-set-offset 'statement-cont '++)
    (c-set-offset 'arglist-intro '+)
    (c-set-offset 'arglist-close 0))

  (add-hook 'java-mode-hook 'ior3k-java-settings))

(use-package js2-mode
  :mode "\\.js\\'"
  :commands js2-mode
  :bind
  (:map js2-mode-map
   ([remap js-find-symbol] . xref-find-definitions))
  :config
  (setq js-indent-level 2)
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  (setq js2-bounce-indent-p t))

(use-package lsp-mode
  :config
  (setq lsp-inhibit-message t))

(use-package lsp-java
  :hook (java-mode . lsp-java-enable))

(use-package lsp-ui
  :hook (lsp-after-open . lsp-ui-mode))

(use-package magit
  :commands (magit-status)
  :config
  (setq magit-log-arguments '("-n128" "--decorate"))
  (setq magit-rebase-arguments '("--autostash"))
  (setq magit-branch-arguments nil)
  (add-to-list 'same-window-regexps "^magit"))

(use-package prettier-js
  :after (js2-mode)
  :hook (js2-mode . prettier-js-mode))

(use-package prodigy
  :config
  (prodigy-define-tag
    :name 'rails-thin-server
    :command "rails"
    :args '("server" "thin")
    :ready-message "Listening on .*, CTRL\\+C to stop")

  (prodigy-define-tag
    :name 'spring-boot-server
    :command "mvn"
    :args '("spring-boot:run")
    :ready-message ".* Started .* in .* seconds (JVM running for .*)"))

(use-package prog-mode
  :after (company)
  :ensure nil
  :no-require t
  :hook
  ((prog-mode . smartparens-mode)
   (prog-mode . subword-mode)
   (prog-mode . company-mode)))

(use-package projectile
  :config
  (setq projectile-completion-system 'ivy)

  (projectile-mode 1))

(use-package projectile-rails
  :after projectile
  :config
  (projectile-rails-global-mode 1))

(use-package pug-mode
  :config
  (setq pug-tab-width 2))

(use-package rjsx-mode)

(use-package smartparens
  :init (require 'smartparens-config)
  :config (show-smartparens-global-mode 1)
  :hook ((smartparens-mode . sp-use-smartparens-bindings)
         (enh-ruby-mode . (lambda () (require 'smartparens-ruby)))))

(use-package stylus-mode)

(use-package telephone-line
  :config
  (setq telephone-line-evil-use-short-tag t)
  (setq telephone-line-lhs
        '((evil   . (telephone-line-evil-tag-segment))
          (accent . (telephone-line-projectile-segment
                     telephone-line-process-segment))
          (nil    . (telephone-line-buffer-segment))))
  (setq telephone-line-primary-left-separator 'telephone-line-gradient
        telephone-line-secondary-left-separator 'telephone-line-nil
        telephone-line-primary-right-separator 'telephone-line-gradient
        telephone-line-secondary-right-separator 'telephone-line-nil)

  (telephone-line-mode 1))

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (exec-path-from-shell add-node-modules-path counsel-projectile yaml-mode evil-exchange lsp-java company-lsp lsp-ui wgrep projectile-rails evil-surround prodigy company company-mode emmet-mode telephone-line evil-collection atom-one-dark atom-one-dark-theme rjsx-mode evil-magit evil-matchit evil stylus-mode pug-mode prettier-js flow-minor-mode flycheck-flow xterm-color general yasnippet js2-mode enh-ruby-mode smartparens magit counsel ivy projectile avy dimmer which-key site-environment base16-theme use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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
