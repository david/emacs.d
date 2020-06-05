(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(require 'use-package)

(setq use-package-always-ensure t
      use-package-verbose t)

(use-package atom-one-dark-theme
  :config
  (load-theme 'atom-one-dark t))

(use-package telephone-line
  :custom
  (telephone-line-evil-use-short-tag t)
  (telephone-line-lhs
   '((evil   . (telephone-line-airline-position-segment))
     (accent . (telephone-line-projectile-segment))
     (nil    . (telephone-line-buffer-modified-segment
                telephone-line-process-segment
                telephone-line-buffer-name-segment))))
  (telephone-line-rhs
   '((nil    . (telephone-line-flycheck-segment))
     (accent . (telephone-line-simple-major-mode-segment))
     (evil   . (telephone-line-evil-tag-segment))))
  :hook (after-init . telephone-line-mode))

(use-package evil
  :custom
  (evil-search-module 'evil-search)
  (evil-want-integration nil)
  (evil-want-keybinding nil)
  (evil-want-C-u-scroll t)
  :config
  (setq-default evil-shift-width 2)

  (evil-mode 1))

(use-package evil-args
  :after evil)

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package general
  :preface
  (general-def :states '(normal motion) "SPC" nil)

  (general-create-definer buffer-command-def
    :states 'normal
    :prefix "SPC")

  (general-create-definer global-command-def
    :states '(normal insert)
    :prefix "M-SPC")

  (general-create-definer motion-def
    :states '(normal motion visual))

  (general-create-definer normal-def
    :states 'normal)

  (global-command-def
    "E" '(:ignore t :which-key "editor")))

(use-package emacs-meta-keybindings
  :no-require t
  :ensure nil
  :preface
  (defun emk/open-init-el ()
    (interactive)
    (find-file (expand-file-name "~/.emacs.d/init.el")))
  :general
  (global-command-def
    "Ec" '(emk/open-init-el :which-key "open init.el")))

(use-package evil-easymotion
  :defer t

  :general
  (motion-def
    "f"   'evilem-motion-find-char-in-line
    "F"   'evilem-motion-find-char-in-line-backward
    "t"   'evilem-motion-find-char-to-in-line
    "T"   'evilem-motion-find-char-to-in-line-backward)
  :config
  (evilem-make-motion
   evilem-motion-find-char-in-line #'evil-repeat-find-char
   :pre-hook (save-excursion
               (setq evil-this-type 'inclusive)
               (call-interactively #'evil-find-char)))

  (evilem-make-motion
   evilem-motion-find-char-in-line-backward #'evil-repeat-find-char
   :pre-hook (save-excursion
               (setq evil-this-type 'inclusive)
               (call-interactively #'evil-find-char-backward)))

  (evilem-make-motion
   evilem-motion-find-char-to-in-line #'evil-repeat-find-char
   :pre-hook (save-excursion
               (setq evil-this-type 'inclusive)
               (call-interactively #'evil-find-char-to)))

  (evilem-make-motion
   evilem-motion-find-char-to-in-line-backward #'evil-repeat-find-char
   :pre-hook (save-excursion
               (setq evil-this-type 'inclusive)
               (call-interactively #'evil-find-char-to-backward))))

(use-package evil-exchange
  :after evil
  :hook (after-init . evil-exchange-install))

(use-package evil-matchit
  :after evil
  :hook (after-init . global-evil-matchit-mode))

(use-package evil-mc
  :general
  (normal-def
   "C-n" 'evil-mc-make-and-goto-next-match)
  :hook (after-init . global-evil-mc-mode))

(use-package evil-surround
  :hook (after-init . global-evil-surround-mode))

(use-package ivy
  :custom
  (ivy-count-format "")
  (ivy-height 20)
  (ivy-initial-inputs-alist nil t)
  (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))

  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

  (ivy-mode 1))

(use-package counsel
  :after ivy
  :general
  (general-define-key
   :keymaps 'counsel-find-file-map

   "C-l" 'counsel-up-directory))

(use-package window-management
  :ensure nil
  :no-require t
  :config
  (setq display-buffer-alist
        '((".*\\*transient\\*.*" . ((display-buffer-in-side-window)))
          (".*magit: transient.*" . ((display-buffer-in-side-window)))
          ("^\\*prodigy-.*\\*$"
           (display-buffer-reuse-window display-buffer-pop-up-frame)
           (reusable-frames . t))
          (""
           (display-buffer-reuse-window display-buffer-same-window)
           (reusable-frames . t)))))

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

  (add-hook 'eshell-mode-hook
            (lambda()
              (setenv "TERM" "xterm-256color")
              (add-to-list 'eshell-visual-commands "ssh")))

  (setq eshell-prompt-function 'eshell-prompt-func)

  (add-hook 'eshell-before-prompt-hook
            (lambda ()
              (setq xterm-color-preserve-properties t)))

  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)

  (setq eshell-output-filter-functions
	(remove 'eshell-handle-ansi-color eshell-output-filter-functions)))

(use-package site-compilation
  :no-require t
  :ensure nil
  :config
  (setq compilation-environment '("TERM=xterm-256color"))
  (setq compilation-scroll-output 'first-error)
  (setq compilation-buffer-name-function
        (lambda (_) (concat "*compilation: " (projectile-project-name) "*")))

  (defun ior3k/advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))

  (advice-add 'compilation-filter :around #'ior3k/advice-compilation-filter))

(use-package git-window-management
  :ensure nil
  :no-require t
  :hook ((git-commit-setup . ior3k/git-commit-setup)))

(use-package magit
  :after general
  :commands magit-status
  :custom
  (magit-commit-show-diff nil)
  (magit-log-arguments '("-n128" "--decorate"))
  (magit-rebase-arguments '("--autostash"))
  (magit-branch-arguments nil)
  (magit-commit-show-diff nil))

(use-package evil-magit
  :after (evil general magit)
  :config

  (general-define-key
   :states '(normal visual motion)
   :keymaps '(magit-mode-map magit-revision-mode-map)

   "C-w" 'delete-frame))

(use-package git-gutter-fringe
  :custom
  (git-gutter:update-interval 2)
  (git-gutter-fr:side 'right-fringe)
  (git-gutter:added-sign "+")
  (git-gutter:deleted-sign "-")
  :config
  (global-git-gutter-mode 1))

(defun ior3k/git-commit-setup ()
  (setq-local fill-column 70)
  (auto-fill-mode 1))

(use-package projectile
  :after (ivy)
  :config
  (setq projectile-completion-system 'ivy)

  (projectile-mode 1))

(use-package counsel-projectile
  :commands (counsel-projectile))

(use-package prodigy
  :after (general)
  :config
  (prodigy-define-tag
    :name 'postgres
    :command "postgres"
    :ready-message ".*database system is ready to accept connections")

  (prodigy-define-tag
    :name 'phoenix
    :command "mix"
    :args '("phx.server")
    :ready-message ".*Access.*\\.Endpoint at http.*")

  (prodigy-define-tag
    :name 'rails-server
    :command "rails"
    :args '("server" "--port" "4000")
    :ready-message ".*Worker.*booted.*")

  (prodigy-define-tag
    :name 'webpack-server
    :command "ruby"
    :args '("bin/webpack-dev-server")
    :ready-message ": Compiled successfully")

  (prodigy-define-tag
    :name 'mariadb
    :command "mysqld"
    :ready-message ".*mysqld: ready for connections.*"
    :stop-signal 'sigterm)

  (prodigy-define-tag
    :name 'redis
    :command "redis-server"
    :ready-message ".*Ready to accept connections")

  (prodigy-define-tag
    :name 'sidekiq
    :command "bundle"
    :args '("exec" "sidekiq")
    :ready-message ".*Starting processing.*")

  (prodigy-define-tag
    :name 'elasticsearch
    :command "elasticsearch"
    :ready-message ".*\\[timbuktu\\] started")

  (prodigy-define-tag
    :name 'fakes3
    :command "bundle"
    :args '("exec" "fakes3" "-r" ".local-s3/" "-p" "4567" "-H" "fakes3")
    :ready-message ".*WEBrick::HTTPServer#start.*")

  (general-define-key
   :states '(normal)
   :keymaps 'prodigy-view-mode-map
   :prefix "g"

   "c" 'prodigy-view-clear-buffer
   "r" 'prodigy-restart
   "S" 'prodigy-stop
   "s" 'prodigy-start))

(use-package aggressive-indent
  :hook ((clojure-mode . aggressive-indent-mode)))

(use-package expand-region)

(use-package lsp-mode
  :config
  (setq lsp-auto-guess-root t
        lsp-eldoc-render-all nil
        lsp-enable-snippet t
        lsp-enable-file-watchers nil
        lsp-highlight-symbol-at-point nil
        lsp-inhibit-message t
        lsp-prefer-capf t
        lsp-prefer-flymake nil))

(use-package lsp-treemacs
  :after (lsp-mode))

(use-package lsp-ui
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-use-childframe nil)
  (setq lsp-ui-sideline-enable nil))

(use-package prog-mode
  :after (company)
  :ensure nil
  :no-require t
  :hook ((prog-mode . smartparens-mode)
         (prog-mode . subword-mode)
         (prog-mode . flycheck-mode)
         (prog-mode . ior3k/add-prog-keybindings)))

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

(defun ior3k/add-prog-keybindings ()
  (general-define-key
   :states 'normal
   :keymaps 'local
   "ge" '(flycheck-next-error :which-key "next error in file")
   "gE" '(flycheck-previous-error :which-key "previous error in file")
   "gy" '(counsel-imenu :which-key "go to symbol in current file")))

(use-package web-mode
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2))

(use-package lispyville
  :after (evil-collection)
  :hook ((emacs-lisp-mode . lispyville-mode)
         (lisp-mode . lispyville-mode)
         (clojure-mode . lispyville-mode))
  :config
  (lispyville-set-key-theme
   '(operators
     c-w
     text-objects
     atom-movement
     (additional-movement normal visual motion)
     slurp/barf-lispy
     additional)))

(use-package edn)

(use-package clojure-mode
  :general
  (buffer-command-def
    :keymaps 'clojure-mode-map

    "e"   '(:ignore t :which-key "eval")
    "ee"  '(cider-eval-last-sexp :which-key "last sexp")
    "ef"  '(cider-eval-defun-at-point :which-key "defun at point")
    "ei"  '(:ignore t :which-key "inspect")
    "eir" '(cider-inspect-last-result :which-key "last result")
    "er"  '(cider-eval-region :which-key "region"))

  :config
  (put-clojure-indent 'match 1))

(use-package cider
  :hook (clojure-mode . cider-mode))

(use-package cider-eval-sexp-fu
  :after cider)

(use-package clj-refactor
  :after (cider clojure-mode yas)
  :config
  (add-hook 'clojure-mode-hook
            (lambda ()
              (clj-refactor-mode 1)
              (yas-minor-mode 1))))

(use-package docker
  :after (general prodigy)
  :config
  (prodigy-define-tag
    :name 'docker
    :ready-message ".*Attaching to .*")

  (prodigy-define-tag
    :name 'docker-compose
    :command "docker-compose"
    :args '("up")))

(use-package dockerfile-mode)

(defun ior3k/prodigy-define-docker-compose-service (project cwd)
  (prodigy-define-service
    :name project
    :cwd cwd
    :tags '(docker docker-compose)))

(defun ior3k/dockerfile-settings ()
  (setq-local tab-width 2))

(use-package coffee-mode)

(use-package enh-ruby-mode
  :mode "\\.rb$"

  :general
  (buffer-command-def
    :keymaps 'enh-ruby-mode-map

    "e"   '(:ignore t :which-key "eval")
    "er"  '(ruby-send-region :which-key "region"))

  :hook (enh-ruby-mode . lsp))

(use-package inf-ruby
  :hook ((inf-ruby . subword-mode)))

(use-package minitest
  :custom
  (minitest-use-rails t)

  :general
  (:keymaps 'minitest-compilation-mode-map
   :states 'normal

   "gr" '(minitest-rerun :which-key "run last"))

  (buffer-command-def
    :keymaps 'enh-ruby-mode-map
    :predicate '(string-match-p "_test\\.rb$" (buffer-file-name))

    "t"  '(:ignore t :which-key "tests")
    "tb" '(minitest-verify :which-key "run in current buffer")
    "tf" '(minitest-verify-single :which-key "run current")
    "tp" '(minitest-verify-all :which-key "run in project")
    "tr" '(minitest-rerun :which-key "run last"))

  (global-command-def
    :predicate '(equal 'rails-test (projectile-project-type))

    "t"  '(:ignore t :which-key "tests")
    "tp" '(minitest-verify-all :which-key "run in project")
    "tr" '(minitest-rerun :which-key "run last"))

  :hook (enh-ruby-mode . minitest-mode))

(use-package web-mode-ruby
  :after (web-mode)
  :ensure nil
  :hook ((web-mode . ior3k/configure-web-pairing))
  :mode ("\\.erb\\'" . web-mode)
  :no-require t)

(defun ior3k/after-switch-to-rails-project ()
  (run-hooks 'ior3k/in-rails-project-hook))

(defun ior3k/configure-web-pairing ()
  (require 'smartparens-ruby)
  (setq web-mode-enable-auto-pairing nil)

  (sp-local-pair 'web-mode "<" ">")
  (sp-local-pair 'web-mode "<%" "%>"))

(defun ior3k/rails-console ()
  (interactive)
  (inf-ruby-console-rails (projectile-project-root)))

(use-package slim-mode)

(defun ior3k/inf-ruby-console-heroku (env)
  "Run console in heroku environment."
  (interactive (list (ior3k/read-heroku-envs)))
  (inf-ruby-console-run (concat "heroku run rails console -r " env)
                        (concat "heroku: " env)))

(defun ior3k/read-heroku-envs ()
  (completing-read "Heroku environment: "
                   '("staging" "production")
                   nil
                   t))

(use-package sql-indent
  :hook (sql-mode . sqlind-minor-mode))

(defun ior3k/configure-mysql-program ()
  (setq sql-mysql-program (string-trim-right (shell-command-to-string "asdf which mysql"))))

(use-package typescript-mode
  :config
  (setq typescript-indent-level 2))

(use-package tide
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package org
  :hook (org-mode . visual-line-mode))

(use-package worf
  :after org
  :hook (org-mode . worf-mode))

(use-package vc-keybindings
  :after general
  :ensure nil
  :no-require t
  :general
  (global-command-def
    "v"   '(:ignore t :which-key "version control")
    "vb"  '(magit-blame-addition :which-key "blame")
    "vc"  '(:ignore t :which-key "conflicts")
    "vca" '(smerge-keep-current :which-key "keep all parts")
    "vcn" '(smerge-next :which-key "go to next conflict")
    "vcN" '(smerge-prev :which-key "go to previous conflict")
    "vcj" '(smerge-keep-lower :which-key "keep lower part")
    "vck" '(smerge-keep-upper :which-key "keep upper part")
    "vcl" '(smerge-keep-current :which-key "keep part under cursor")
    "vh"  '(:ignore t :which-key "hunks")
    "vhn" '(git-gutter:next-hunk :which-key "next hunk")
    "vhp" '(git-gutter:previous-hunk :which-key "previous hunk")
    "vhr" '(git-gutter:revert-hunk :which-key "revert hunk")
    "vhs" '(git-gutter:stage-hunk :which-key "stage hunk")
    "vlf" '(magit-log-buffer-file :which-key "commits related to file")
    "vv"  '(projectile-vc :which-key "git status")))

(use-package general
  :after (evil evil-collection ivy)
  :config
  (motion-def
    "C-p" 'counsel-yank-pop)

  (motion-def
   :prefix "SPC"
   "f" '(evil-avy-goto-char-timer :which-key "goto chars after some time")
   "/" '(evil-search-forward :which-key "regular evil search"))

  (general-define-key
   :states '(emacs insert motion normal visual)

   "M-h" 'windmove-left
   "M-j" 'windmove-down
   "M-k" 'windmove-up
   "M-l" 'windmove-right)

  (general-define-key
   "C-s" '(save-buffer :which-key "save buffer"))

  (general-define-key
   :states 'normal

   "+" 'universal-argument
   "-" 'negative-argument
   "/" 'swiper
   "?" 'swiper
   "q" 'previous-buffer
   "Q" 'delete-other-windows)

  (defun ior3k-insert-semicolon-at-eol ()
    (interactive)
    (end-of-line)
    (self-insert-command 1))

  (defun ior3k-find-agenda-org-in-project ()
    (interactive)
    (find-file (expand-file-name "agenda.org" (projectile-project-root))))

  (general-define-key
   :states 'insert
   :keymaps '(java-mode-map rjsx-mode-map reason-mode)
   ";" 'ior3k-insert-semicolon-at-eol)

  (general-define-key
   :states '(emacs insert motion normal visual)
   :keymaps '(completion-list-mode-map fundamental-mode-map)

   "C-w" 'delete-frame)

  (general-define-key
   :states '(normal visual motion emacs)

   "C-w" 'delete-frame)

  (general-define-key
   :states '(normal motion visual)

   "L" 'evil-forward-arg
   "H" 'evil-backward-arg)

  (general-define-key
   :keymaps 'evil-inner-text-objects-map

   "a" 'evil-inner-arg)

  (general-define-key
   :keymaps 'evil-outer-text-objects-map

   "a" 'evil-outer-arg)

  (buffer-command-def
   "v" '(er/expand-region :which-key "expand region"))

  (global-command-def
    "b"  '(:ignore t :which-key "buffer")
    "bb" '(ivy-switch-buffer :which-key "switch buffer")
    "bd" '(evil-delete-buffer :which-key "delete current buffer")
    "bo" '(read-only-mode :which-key "toggle read only mode")
    "bp" '(counsel-projectile :which-key "open in project")
    "bs" '(save-buffer :which-key "save this buffer")
    "bS" '(write-file :which-key "write to file"))

  (global-command-def
    "c"   '(:ignore t :which-key "code")
    "cc"  '(projectile-compile-project :which-key "compile")
    "cC"  '(recompile :which-key "recompile")
    "ca"  '(lsp-execute-code-action :which-key "action")
    "cet" '(lsp-treemacs-errors-list :which-key "show errors in tree")
    "ch"  '(lsp-describe-thing-at-point :which-key "show symbol documentation")
    "cr"  '(lsp-rename :which-key "rename symbol")
    "cwr" '(lsp-workspace-restart :which-key "restart")
    "cx"  '(:ignore t :which-key "evals"))

  (global-command-def
    "E"  '(:ignore t :which-key "editor")
    "Ed" '(dired :which-key "dired")
    "Ek" '(package-list-packages :which-key "packages")
    "Ep" '(projectile-switch-project :which-key "switch")
    "Eq" '(save-buffers-kill-emacs :which-key "quit")
    "Es" '(prodigy :which-key "servers")
    "Ex"  '(eval-last-sexp :which-key "eval last expression"))

  (global-command-def
    "f"  '(:ignore t :which-key "file")
    "fa" '(find-alternate-file :which-key "reload")
    "fd" '(delete-current-file :which-key "delete current")
    "ff" '(counsel-find-file :which-key "find in current directory")
    "fi" '(insert-file :which-key "insert contents")
    "fr" '(er-rename-file-and-buffer :which-key "rename")
    "fs" '(write-file :which-key "save as")
    "fw" '(pwd :which-key "pwd"))

  (global-command-def
    "h"  '(:ignore t :which-key "help")
    "hf" '(describe-function :which-key "function")
    "hk" '(describe-key :which-key "key")
    "hm" '(describe-mode :which-key "mode")
    "hv" '(describe-variable :which-key "variable"))

  (global-command-def
    "m"  '(:ignore t :which-key "frame")
    "md" '(delete-frame :which-key "delete current")
    "mm" '(make-frame-command :which-key "new"))

  (global-command-def
    "p" '(:ignore t :which-key "project")
    "ph" '(projectile-run-eshell :which-key "shell in project")
    "ps" '(counsel-ack :which-key "search in project")
    "px" '(projectile-run-async-shell-command-in-root :which-key "command in root"))

  (global-command-def
    "q"  '(:ignore t :which-key "sql")
    "qq" '(sql-connect :which-key "connect"))

  (global-command-def
    "r" '(:ignore t :which-key "repl"))

  (global-command-def
    "w"  '(:ignore t :which-key "window")
    "wd" '(delete-window :which-key "delete current")
    "wD" '(delete-other-windows :which-key "delete other windows")
    "wj" '(split-window-below :which-key "open below")
    "wl" '(split-window-right :which-key "open right"))

  (global-command-def
   "M-SPC" 'counsel-M-x))

(use-package elisp-keybindings
  :ensure nil
  :no-require t
  :config
  (global-command-def
   :keymaps 'emacs-lisp-mode-map

   "e"   '(:ignore t :which-key "eval")
   "eb" '(eval-buffer :which-key "buffer")
   "ee" '(eval-last-sexp :which-key "last expression")
   "ef" '(eval-defun :which-key "defun")))

(use-package ivy-keybindings
  :ensure nil
  :no-require t

  :preface
  (defun ivy-keybindings/counsel-projectile ()
    (interactive)
    (ivy-quit-and-run (call-interactively #'counsel-projectile)))

  :config
  (general-define-key
   :states '(insert normal)
   :keymaps 'ivy-switch-buffer-map

   "C-p" 'ivy-keybindings/counsel-projectile)

  (general-define-key
   :states 'insert
   :keymaps 'ivy-minibuffer-map

   "C-j" 'ivy-next-line
   "C-k" 'ivy-previous-line))

(use-package prodigy-keybindings
  :ensure nil
  :no-require t
  :hook (evil-collection-setup . prodigy-keybindings/setup)
  :preface
  (defun prodigy-keybindings/setup (&rest _ignored)
    (general-define-key
     :states 'normal
     :keymaps 'prodigy-mode-map

     "RET" 'prodigy-display-process)))

(use-package ruby-keybindings
  :ensure nil
  :no-require t
  :config
  (general-define-key
   :states 'normal
   :keymaps 'minitest-compilation-mode-map

    "gr" '(minitest-rerun :which-key "run last"))

  (global-command-def
    :predicate '(equal 'rails-test (projectile-project-type))

    "t"  '(:ignore t :which-key "tests")
    "tb" '(minitest-verify :which-key "run in current buffer")
    "tf" '(minitest-verify-single :which-key "run current")
    "tp" '(minitest-verify-all :which-key "run in project")
    "tr" '(minitest-rerun :which-key "run last")))

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
             (42 . ".\\(?:\\(\\*\\*\\|[*/>]\\).?\\)")
             (43 . ".\\(?:\\([>]\\)>?\\)")
             (43 . ".\\(?:\\(\\+\\+\\|[+>]\\).?\\)")
             (45 . ".\\(?:\\(-[->]\\|<<\\|>>\\|[-<>|~]\\)[-<>|~]?\\)")
             (46 . ".\\(?:\\(\\.[.<]\\|[-.=]\\)[-.<=]?\\)")
             (47 . ".\\(?:\\(//\\|==\\|[=>]\\)[/=>]?\\)")
             (47 . ".\\(?:\\(//\\|==\\|[*/=>]\\).?\\)")
             (48 . ".\\(?:\\(x[a-fA-F0-9]\\).?\\)")
             (58 . ".\\(?:\\(::\\|[:<=>]\\)[:<=>]?\\)")
             (59 . ".\\(?:\\(;\\);?\\)")
             (60 . ".\\(?:\\(!--\\|\\$>\\|\\*>\\|\\+>\\|-[-<>|]\\|/>\\|<[-<=]\\|=[<>|]\\|==>?\\||>\\||||?\\|~[>~]\\|[$*+/:<=>|~-]\\)[$*+/:<=>|~-]?\\)")
             (61 . ".\\(?:\\(!=\\|/=\\|:=\\|<<\\|=[=>]\\|>>\\|[=>]\\)[=<>]?\\)")
             (62 . ".\\(?:\\(->\\|=>\\|>[-=>]\\|[-:=>]\\)[-:=>]?\\)")
             (63 . ".\\(?:\\([.:=?]\\)[.:=?]?\\)")
             (91 . ".\\(?:\\(|\\)|?\\)")
             (92 . ".\\(?:\\([\\n]\\)[\\]?\\)")
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
 '(git-gutter-fr:side 'right-fringe)
 '(git-gutter:added-sign "+")
 '(git-gutter:deleted-sign "-")
 '(git-gutter:update-interval 2)
 '(helm-buffer-max-length 40)
 '(helm-completion-style 'emacs)
 '(package-selected-packages
   '(minitest worf expand-region counsel-projectile evil-snipe evil-easymotion slim-mode edn org-mode git-gutter-fringe git-gutter tide typescript-mode helm-cider clj-refactor cider-eval-sexp-fu wgrep-helm web-mode coffee-mode sql-indent lispyville composite cider clojure-mode aggressive-indent aggressive-indent-mode alchemist helm-lsp helm-projectile helm-ls-git helm lsp-treemacs elixir-mode elixir-ls elixir-lsp flycheck-elixir reason-mode terraform-mode docker dockerfile-mode rubocop evil-mc indium flycheck yasnippet yaml-mode xterm-color which-key wgrep use-package telephone-line smartparens rjsx-mode rainbow-mode rainbow-delimiters projectile-rails prodigy prettier-js lsp-ui lsp-java json-mode highlight-indent-guides general feature-mode exec-path-from-shell evil-surround evil-matchit evil-magit evil-exchange evil-collection evil-args enh-ruby-mode emmet-mode dap-mode avy atom-one-dark-theme add-node-modules-path))
 '(safe-local-variable-values
   '((eval ior3k/after-switch-to-rails-project)
     (eval ior3k/configure-mysql-program)
     (eval ior3k/switched-to-rails-project)
     (eval setq sql-mysql-program
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
