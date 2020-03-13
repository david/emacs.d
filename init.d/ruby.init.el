(use-package ruby-keybindings
  :no-require t
  :ensure nil
  :hook ((ruby-mode . ior3k/rails-project-add-keybindings)))

(use-package ruby-mode
  :no-require t
  :ensure nil
  :after lsp
  :hook ((ruby-mode . lsp)))

(use-package inf-ruby)

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
  (when (equal 'rails-test (projectile-project-type))
    (global-def
      :keymaps 'local
      "rs" '(ior3k/rails-console :which-key "start"))))

(defun ior3k/rails-console ()
  (interactive)
  (inf-ruby-console-rails (projectile-project-root)))

(use-package slim-mode)
