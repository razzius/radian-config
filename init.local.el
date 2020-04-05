(setq radian-font-size 180)

(global-set-key (kbd "C-`") 'describe-key)

(radian-local-on-hook after-init
  (radian-with-operating-system macOS
    (setq mac-option-modifier 'super
          mac-command-modifier 'meta))

  (define-key smartparens-mode-map (kbd "M-s") nil)

  (use-package evil
    :demand t
    :config (evil-mode 1))

  (use-package evil-surround
    :demand t
    :config (evil-surround-mode))

  (use-package evil-commentary
    :demand t
    :config (evil-commentary-mode))

  (use-package general
    :demand t)

  (defun razzi-local-restart-emacs ()
    (interactive)
    (let ((radian--restart-in-progress t)) ; :/
      (razzi-restart-emacs)))

  (use-package razzi
    :demand t
    :straight (:host github :repo "razzius/razzi.el")
    :general
    ("M-s" 'razzi-exit-insert-and-save)
    (:states 'normal
             :prefix "SPC"
             "qr" 'razzi-local-restart-emacs))

  (use-package crux
    :demand t
    :general
    (:states 'normal
             :prefix "SPC"
             "TAB" 'crux-switch-to-previous-buffer))


  (general-define-key :states 'visual
                      "s" 'evil-surround-region
                      "S" 'razzi-sort-sexp
                      "$" 'razzi-almost-end-of-line)

  (general-define-key :states 'insert
                      "C-l" 'sp-forward-slurp-sexp
                      "C-h" 'delete-backward-char
                      "s-<backspace>" 'evil-delete-backward-word)

  (defun razzi-sort-sexp ()
    (interactive)
    (split-window-vertically)
    (message (thing-at-point 'sexp 'no-properties)))

  (general-define-key :states 'normal
                      "0" 'evil-first-non-blank
                      "M-w" 'kill-current-buffer
                      "M-s" 'save-buffer
                      "M-/" 'evil-commentary-line)

  (general-define-key :states 'normal :prefix "SPC"
                      "h d f" 'describe-function
                      "b b" 'switch-to-buffer
                      "SPC" 'execute-extended-command
                      "f r" 'recentf-open-files
                      "f f" 'find-file
                      "f i" 'radian-find-init-local-el
                      "w m" 'delete-other-windows
                      "w j" 'windmove-down
                      "w k" 'windmove-up
                      "w h" 'windmove-left
                      "w l" 'windmove-right
                      "w 2" 'split-window-horizontally)

  (use-package eval-sexp-fu
    :demand t
    :config
    ;; The default duration disappears for some forms that take a while
    ;; to evaluate, like use-package
    (setq eval-sexp-fu-flash-duration .3)

    (defun razzi-flash-eval-defun ()
      "Hack to make the thing flash even when on an opening parenthesis."
      (interactive)
      (save-excursion
        (if (looking-at "(")
            (save-excursion (evil-jump-item)
                            (forward-char)
                            (call-interactively 'eval-last-sexp))
          (razzi-eval-current-sexp))))

    (general-define-key "M-RET" 'razzi-flash-eval-defun)

    (general-define-key "M-\\" 'eval-defun))

    (use-package desktop
      :demand t
      :config
      (setq desktop-save t)
      (desktop-save-mode 1))

  (recentf-mode))
