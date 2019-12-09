(use-package prodigy
  :after (general)
  :config
  (prodigy-define-tag
    :name 'spring-boot-server
    :command "mvn")

  (prodigy-define-tag
    :name 'invoker
    :command "invoker"
    :args '("start"))

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
    :name 'docker
    :ready-message ".*Attaching to .*")

  (prodigy-define-tag
    :name 'docker-compose
    :command "docker-compose"
    :args '("up"))

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

  (prodigy-define-tag
    :name 'shadow-cljs
    :command "yarn"
    :args '("shadow-cljs" "watch" "app")
    :ready-message ".*Build completed\\. .*")

  (general-define-key
   :states '(normal)
   :keymaps 'prodigy-view-mode-map
   :prefix "p"

   "c" 'prodigy-view-clear-buffer
   "r" 'prodigy-restart
   "S" 'prodigy-stop
   "s" 'prodigy-start)

  (defun ior3k/prodigy-define-docker-compose-service (project cwd)
    (prodigy-define-service
      :name project
      :cwd cwd
      :tags '(docker docker-compose))))
