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
