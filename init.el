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

(use-package dabbrev
  :no-require t
  :ensure nil
  :config
  (setq dabbrev-upcase-means-case-search nil))

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
                              (fullscreen . maximized)))
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
   'default nil :foreground "#cccccc" :height 100)

  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(use-package site-minibuffer
  :no-require t
  :ensure nil
  :config
  (defalias 'yes-or-no-p 'y-or-n-p))

(use-package add-node-modules-path
  :commands add-node-modules-path
  :hook js2-mode)

(use-package avy
  :bind (:map isearch-mode-map
         ("C-'" . avy-isearch))
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
  :after (company yasnippet)
  :config
  (setq company-lsp-async t
        company-lsp-cache-candidates t))

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

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

(use-package dimmer
  :disabled
  :config
  (setq dimmer-fraction 0.5)
  (dimmer-mode 1))

(use-package emacs-lisp
  :no-require t
  :ensure nil
  :hook ((emacs-lisp-mode . smartparens-mode)
         (emacs-lisp-mode . eldoc-mode)
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
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :config
  (setq-default evil-shift-width 2)

  (evil-mode 1))

(use-package evil-args
  :after evil)

(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-setup-minibuffer t)

  (evil-collection-init)

  (evil-collection-define-key 'insert 'ivy-minibuffer-map
    (kbd "C-n") nil
    (kbd "C-p") nil
    (kbd "C-k") 'ivy-previous-line
    (kbd "C-j") 'ivy-next-line
    (kbd "C-d") 'ivy-immediate-done)

(use-package evil-exchange
  :config
  (evil-exchange-install))

(use-package evil-magit
  :after (magit))

(use-package evil-matchit
  :after (evil)
  :config
  (global-evil-matchit-mode 1))

(use-package evil-string-inflection)

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

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

(use-package flycheck-jest
  :disabled
  :after flycheck
  :config
  (flycheck-jest-setup))

(use-package general
  :config
  (general-create-definer ior3k-local-leader-def
    :prefix "M-SPC m")

  (general-define-key
   "C-w" 'backward-kill-word
   "s-k" 'windmove-up
   "s-j" 'windmove-down
   "s-l" 'windmove-right
   "s-h" 'windmove-left)

  (general-define-key
   :states 'normal

   "SPC" 'evil-scroll-page-down
   "+"   'universal-argument
   "q"   'quit-window
   "'"   'avy-goto-char-timer)

  (defun ior3k-insert-semicolon-at-eol ()
    (interactive)
    (end-of-line)
    (self-insert-command 1))

  (defun ior3k-find-project-org-in-project ()
    (interactive)
    (find-file (expand-file-name "project.org" (projectile-project-root))))

  (general-define-key
   :states 'insert
   :keymaps '(java-mode-map rjsx-mode-map)
   ";" 'ior3k-insert-semicolon-at-eol)

  (general-define-key
   :states '(normal motion)

   "L" 'evil-forward-arg
   "H" 'evil-backward-arg)

  (general-define-key
   :keymaps 'evil-inner-text-objects-map

   "a" 'evil-inner-arg)

  (general-define-key
   :keymaps 'evil-outer-text-objects-map

   "a" 'evil-outer-arg)

  (general-define-key
   :states '(normal visual motion insert emacs)
   :prefix "M-SPC"

   "M-SPC" 'counsel-M-x

   "c"  '(:ignore t :which-key "code")
   "cm" '(comment-dwim :which-key "comment dwim")

   "cS" '(:ignore t :which-key "snippets")
   "cSn" '(yas-new-snippet :which-key "new snippet")
   "cSS" '(yas-visit-snippet-file :which-key "find snippet file")
   "cSi" '(yas-insert-snippet :which-key "insert snippet")

   "ct" '(projectile-toggle-between-implementation-and-test
          :which-key "toggle test file")
   "cv" '(minitest-verify :which-key "run tests in file")

   "d"  '(:ignore t :which-key "delete")
   "db" '(kill-this-buffer :which-key "current buffer")
   "de" '(save-buffers-kill-terminal :which-key "emacs")
   "df" '(delete-current-file :which-key "delete current file")
   "dm" '(delete-frame :which-key "current frame")
   "dw" '(delete-window :which-key "this window")
   "dW" '(delete-other-windows :which-key "other windows")

   "g"   '(:ignore t :which-key "go to")
   "ga"  '(counsel-projectile :which-key "buffer or file")
   "gb"  '(ivy-switch-buffer :which-key "buffer")
   "ge"  '(flycheck-next-error :which-key "next error")
   "gE"  '(flycheck-previous-error :which-key "previous error")
   "gi"  '(counsel-imenu :which-key "imenu")
   "gf"  '(counsel-find-file :which-key "in current directory")
   "gF"  '(counsel-projectile-find-file :which-key "in project")
   "gg"  '(counsel-projectile :which-key "buffer or file")
   "go"  '(:ignore t :which-key "ocurrences")
   "gos" '(counsel-projectile-ag :which-key "free search")
   "gv" '(find-alternate-file :which-key "alternate file")

   "i"  '(:ignore t :which-key "insert")
   "if" '(insert-file :which-key "file contents")
   "ii" '(lsp-java-add-import :which-key "import")

   "m"  '(:ignore t :which-key "manage")
   "mf" '(:ignore t :which-key "files")
   "mfr" '(er-rename-file-and-buffer :which-key "rename")
   "mp" '(package-list-packages :which-key "packages")
   "ms" '(prodigy :which-key "servers")

   "n"   '(:ignore t :which-key "new")
   "nc"  '(evil-record-macro :which-key "macro")
   "nm"  '(make-frame-command :which-key "frame")
   "nw"  '(:ignore t :which-key "window")
   "nwj" '(split-window-below :which-key "below")
   "nwl" '(split-window-right :which-key "right")

   "o"   '(:ignore t :which-key "open")
   "oc"  '(projectile-rails-console :which-key "console")
   "od"  '(dired :which-key "dired")
   "oh"  '(:ignore t :which-key "help")
   "ohf" '(counsel-describe-function :which-key "function")
   "ohk" '(describe-key :which-key "key")
   "ohm" '(describe-mode :which-key "mode")
   "ohv" '(counsel-describe-variable :which-key "variable")
   "oj"  '(ior3k-find-project-org-in-project :which-key "project file")
   "op"  '(counsel-projectile-switch-project :which-key "project")
   "oq"  '(sql-connect :which-key "database connection")
   "os"  '(projectile-run-eshell :which-key "shell in project")
   "ov"  '(magit-status :which-key "git status")

   "p"  '(:ignore t :which-key "projects")
   "px" '(bpr-spawn :which-key "run async shell command")

   "s"  '(:ignore t :which-key "save")
   "sb" '(save-buffer :which-key "this buffer")
   "sf" '(write-file :which-key "to file")
   "ss" '(save-buffer :which-key "this buffer")

   "x"  '(:ignore t :which-key "execute")
   "xe" '(eval-last-sexp :which-key "eval last code block")))

(use-package highlight-indent-guides
  :hook ((prog-mode . highlight-indent-guides-mode)
         (yaml-mode . highlight-indent-guides-mode))
  :config
  (setq highlight-indent-guides-method 'character))

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
  :init
  (ior3k-local-leader-def
   :states 'normal
   :keymaps 'java-mode-map

   "a" '(lsp-execute-code-action :which-key "code actions"))

  (defun ior3k-java-settings ()
    (setq c-basic-offset 4
          fill-column 100
          tab-width 4
          evil-shift-width 4)

    (c-set-offset 'statement-cont '+)
    (c-set-offset 'arglist-intro '+)
    (c-set-offset 'arglist-close 0))

  (add-hook 'java-mode-hook
            (lambda ()
              (setq-local company-backends (list 'company-lsp))))
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

(use-package json-mode)

(use-package lsp-mode
  :config
  (setq lsp-inhibit-message t
        lsp-eldoc-render-all nil
        lsp-highlight-symbol-at-point nil
        lsp-prefer-flymake nil))

(use-package lsp-java
  :after lsp
  :config
  (setq lsp-java-enable-file-watch nil)
  (setq lsp-java-organize-imports nil)
  (setq lsp-java-save-action-organize-imports nil)

  (add-hook 'java-mode-hook 'lsp)
  (add-hook 'java-mode-hook 'flycheck-mode)
  (add-hook 'java-mode-hook 'company-mode)
  (add-hook 'java-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :config
  (setq lsp-ui-sideline-update-mode 'point))

(use-package magit
  :commands (magit-status)
  :config
  (setq magit-log-arguments '("-n128" "--decorate"))
  (setq magit-rebase-arguments '("--autostash"))
  (setq magit-branch-arguments nil)
  (add-to-list 'same-window-regexps "^magit"))

(use-package prettier-js
  :after (js2-mode rjsx-mode)
  :hook ((js2-mode . prettier-js-mode)
         (rjsx-mode . prettier-js-mode)))

(use-package prodigy
  :config
  (prodigy-define-tag
    :name 'rails-thin-server
    :command "rails"
    :args '("server" "thin")
    :ready-message "Listening on .*, CTRL\\+C to stop")

  (prodigy-define-tag
    :name 'webpack-server
    :command "webpack-dev-server"
    :ready-message ": Compiled successfully")

  (prodigy-define-tag
    :name 'spring-boot-server
    :command "mvn"
    :args '("spring-boot:run")
    :ready-message ".* Started .* in .* seconds (JVM running for .*)")

  (prodigy-define-tag
    :name 'yarn-server
    :command "yarn"
    :args '("start")
    :ready-message "You can now view .* in the browser.")

  (add-to-list 'same-window-regexps "^\\*prodigy"))

(use-package prog-mode
  :after (company)
  :ensure nil
  :no-require t
  :hook
  ((prog-mode . smartparens-mode)
   (prog-mode . subword-mode)
   (prog-mode . company-mode)
   (prog-mode . flycheck-mode)))

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

(use-package rjsx-mode
  :mode "\\.js\\'")

(use-package smartparens
  :init (require 'smartparens-config)
  :config
  (setq sp-escape-quotes-after-insert nil)
  (show-smartparens-global-mode 1)
  :hook ((smartparens-mode . sp-use-smartparens-bindings)
         (enh-ruby-mode . (lambda () (require 'smartparens-ruby)))
         (rjsx-mode . (lambda () (require 'smartparens-javascript)))))

(use-package stylus-mode)

(use-package telephone-line
  :config
  (setq telephone-line-evil-use-short-tag t)
  (setq telephone-line-lhs
        '((evil   . (telephone-line-airline-position-segment))
          (accent . (telephone-line-process-segment))
          (nil    . (telephone-line-buffer-modified-segment
                     telephone-line-buffer-name-segment))))
  (setq telephone-line-rhs
        '((nil   .  (telephone-line-flycheck-segment))
          (accent . (telephone-line-projectile-segment))
          (evil    . (telephone-line-evil-tag-segment))))

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
  (defun ior3k/downcase-first-char (string)
    "Capitalize only the first character of the input STRING."
    (when (and string (> (length string) 0))
      (let ((first-char (substring string nil 1))
            (rest-str   (substring string 1)))
        (concat (downcase first-char) rest-str))))

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
    (flycheck-jest dap-mode dap-java json-mode bpr feature-mode evil-args highlight-indent-guides evil-string-inflection exec-path-from-shell add-node-modules-path counsel-projectile yaml-mode evil-exchange lsp-java company-lsp lsp-ui wgrep projectile-rails evil-surround prodigy company company-mode emmet-mode telephone-line evil-collection atom-one-dark atom-one-dark-theme rjsx-mode evil-magit evil-matchit evil stylus-mode pug-mode prettier-js xterm-color general yasnippet js2-mode enh-ruby-mode smartparens magit counsel ivy projectile avy dimmer which-key site-environment base16-theme use-package))))
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
