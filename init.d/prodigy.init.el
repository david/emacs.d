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
    :name 'rails-thin-server
    :command "rails"
    :args '("server" "thin")
    :ready-message "Listening on .*, CTRL\\+C to stop")

  (prodigy-define-tag
    :name 'rails-server
    :command "rails"
    :args '("server" "--port" "4000")
    :ready-message ".*Use CTRL\\+C to stop")

  (prodigy-define-tag
    :name 'webpack-server
    :command "ruby"
    :args '("bin/webpack-dev-server")
    :ready-message ": Compiled successfully")

  (prodigy-define-tag
    :name 'yarn-server
    :command "yarn"
    :args '("start")
    :ready-message "You can now view .* in the browser.")

  (prodigy-define-tag
    :name 'mariadb
    :command "mysqld_safe"
    :ready-message ".*Starting mysqld daemon with databases from.*")

  (prodigy-define-tag
    :name 'redis
    :command "redis-server"
    :ready-message ".*Ready to accept connections.*")

  (prodigy-define-tag
    :name 'sidekiq
    :command "bundle"
    :args '("exec" "sidekiq")
    :ready-message ".*Starting processing.*")

  (prodigy-define-tag
    :name 'elasticsearch
    :command "elasticsearch"
    :ready-message ".*Starting mysqld daemon with databases from.*")

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
