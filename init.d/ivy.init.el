(use-package ivy
  :custom
  (ivy-count-format "")
  (ivy-height 20)
  (ivy-initial-inputs-alist nil t)
  (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))

  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

  (ivy-mode 1))
