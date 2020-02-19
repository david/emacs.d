(use-package enh-ruby-mode
  :after (projectile smartparens)
  :mode (("\\.rb\\'" . enh-ruby-mode)
         ("\\.rake\\'" . enh-ruby-mode))
  :config
  (setq enh-ruby-add-encoding-comment-on-save nil)
  (setq enh-ruby-deep-indent-paren nil)
  (setq enh-ruby-deep-indent-construct nil)

  (add-hook
   'enh-ruby-mode-hook
   (lambda ()
     (require 'smartparens-ruby)
     (sp-pair "<" ">" :actions :rem)))

  (add-hook 'projectile-switch-project-hook 'ior3k/rails-project-add-keybindings))

(use-package inf-ruby
  :config
  (add-to-list 'display-buffer-alist
               '("^\\*rails"
                 (display-buffer-reuse-window display-buffer-same-window)
                 (reusable-frames . t))))

(use-package web-mode-ruby
  :after (web-mode)
  :no-require t
  :ensure nil
  :config
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

  (add-hook
   'web-mode-hook
   (lambda ()
     (require 'smartparens-ruby)
     (setq web-mode-enable-auto-pairing nil)

     (sp-pair "<" ">")
     (sp-pair "%" "%"))))

(defun ior3k/rails-project-add-keybindings ()
  (if (equal 'rails-test (projectile-project-type))
      (global-def
        "rs" '(:ignore t :which-key "server")
        "rss" '(projectile-rails-console :which-key "start"))))
