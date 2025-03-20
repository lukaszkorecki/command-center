;; Make all custom executables work in terminal and GUI emacs
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "~/.emacs.d/etc/bin")
(add-to-list 'exec-path "/opt/homebrew/bin")
(setenv "PATH"
        (concat
         (getenv "PATH")
         ":/usr/local/bin:~/.emacs.d/etc/bin:~/bin:~/bin/node/bin:~/bin/jdk/Contents/Home/bin:/usr/local/opt/openjdk/bin:/opt/homebrew/opt/openjdk/bin:/opt/homebrew/bin"))


(provide 'lk/setup-path)
