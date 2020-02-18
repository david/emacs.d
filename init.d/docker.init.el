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
