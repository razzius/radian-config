(setq radian-font-size 180)

(radian-local-on-hook after-init
  (radian-with-operating-system macOS
    (setq mac-option-modifier 'super
          mac-command-modifier 'meta
          ns-pop-up-frames nil))

  ;; For whatever reason, M-s is not overwritten by evil normal mode
  ;; keybindings.
  (define-key smartparens-mode-map (kbd "M-s") nil)
  (global-set-key (kbd "C-`") 'describe-key)
  (define-key input-decode-map "\C-i" [C-i])

  (use-package evil
    :demand t
    :config
    (setq
     ;; Makes f and F search beyond the current line.
     evil-cross-lines t

     ;; Makes :s/ substitute all matches on a line, not just the first
     ;; match.
     evil-ex-substitute-global t

     ;; Makes / search literally, making it easier to find characters
     ;; such as / and $.
     evil-regexp-search nil)

    (setq-default
     ;; 2 spaces when indenting using > and <.
     evil-shift-width 2

     ;; Makes * and # commands search using the current symbol,
     ;; which includes characters such as _ and -.
     evil-symbol-word-search t)


    (evil-mode 1))

  (evil-set-initial-state 'helpful-mode 'emacs)

  (use-package evil-surround
    :demand t
    :config (global-evil-surround-mode))

  (use-package evil-commentary
    :demand t
    :config (evil-commentary-mode))

  (use-package evil-magit
    :demand t)

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
    ("M-s" 'razzi-flycheck-and-save-buffer)
    (:states 'normal
             "C-c r" 'web-mode-element-rename
             "<backtab>" 'razzi-previous-useful-buffer
             "[ SPC" 'razzi-insert-newline-before
             "] SPC" 'razzi-insert-newline-after
             "-" 'razzi-transpose-next-line
             "_" 'razzi-transpose-previous-line
             "g s" 'razzi-save-and-magit-status
             "C" 'razzi-change-line
             "D" 'razzi-kill-line-and-whitespace
             "G" 'razzi-almost-end-of-buffer
             "Q" 'razzi-replay-q-macro)
    (:states 'normal
             :prefix "SPC"
             "," 'razzi-append-comma
             "o" 'razzi-put-after
             "i d" 'razzi-put-debugger
             "f r" 'razzi-recentf
             "q r" 'razzi-local-restart-emacs)
    (:states 'visual
             "$" 'razzi-almost-end-of-line
             "il" 'razzi-mark-line-text)
    (:states 'insert
             "C-t" 'razzi-transpose-previous-chars
             "C-c a" 'razzi-abbrev-or-add-global-abbrev
             "M-v" 'razzi-paste))

  (use-package crux
    :demand t
    :general
    (:states 'normal
             :prefix "SPC"
             "TAB" 'crux-switch-to-previous-buffer))

  (general-define-key "M-`" 'vterm-toggle)

  (general-define-key :states 'visual
                      "s" 'evil-surround-region)

  (general-define-key :states 'insert
                      "<tab>" 'yas-expand
                      "<C-i>" 'hippie-expand
                      "C-l" 'sp-forward-slurp-sexp
                      "C-h" 'delete-backward-char
                      "s-<backspace>" 'evil-delete-backward-word
                      "M-l" 'evil-visual-line
                      "M-/" 'evil-commentary-line)

  ;; todo
  (defun razzi-sort-sexp ()
    "Sorts the lines of a sexp, first by indentation,
    then alphabetically, ensuring it ends with a closing parenthesis."
    (interactive)
    (split-window-vertically)
    (message (thing-at-point 'sexp 'no-properties)))

  (use-feature flycheck
    :config
    (defun razzi-find-npm-bin (name)
      (let* ((root (locate-dominating-file (buffer-file-name) "node_modules"))
             (npm-bin (concat root "node_modules/.bin"))
             (executable (expand-file-name name npm-bin)))
        (when (file-executable-p executable)
          executable)))

    (defun razzi-setup-local-eslint ()
      (flycheck-add-mode 'javascript-eslint 'web-mode)
      (flycheck-add-next-checker 'lsp 'javascript-eslint)

      (let ((eslint (razzi-find-npm-bin "eslint")))
        (setq-local flycheck-javascript-eslint-executable eslint)))

    (add-hook 'web-mode-hook #'razzi-setup-local-eslint)

    :general
    (:states 'normal
             :prefix "SPC"
             "e l" 'flycheck-list-errors
             "e n" 'flycheck-next-error
             "e p" 'flycheck-previous-error
             "e v" 'flycheck-verify-setup))

  (use-feature magit
    :general
    (:states 'normal
             "gb" 'magit-blame-addition))

  (general-define-key :states 'normal
                      "g /" 'rg-dwim
                      "0" 'evil-first-non-blank
                      "M-f" 'ctrlf-forward-literal
                      "M-w" 'kill-current-buffer
                      "M-/" 'evil-commentary-line)

  (general-define-key :states 'normal :prefix "SPC"
                      "u" 'universal-argument
                      "ESC" 'kill-this-buffer
                      "/" 'radian-rg
                      "h d f" 'describe-function
                      "h d v" 'describe-variable
                      "b b" 'switch-to-buffer
                      "SPC" 'execute-extended-command
                      "f f" 'find-file
                      "f i" 'radian-find-init-local-el
                      "f o" 'crux-open-with
                      "p" 'projectile-switch-project
                      "w m" 'delete-other-windows
                      "w j" 'windmove-down
                      "w k" 'windmove-up
                      "w h" 'windmove-left
                      "w l" 'windmove-right
                      "w 2" 'split-window-horizontally)

  (evil-define-text-object whole-buffer (count &optional beginning end type)
    (evil-range 0 (point-max)))

  (general-define-key :states 'operator
                      "E" 'forward-symbol
                      "ae" 'whole-buffer
                      "SPC" 'evil-inner-symbol)

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

    (general-define-key "M-RET" 'razzi-flash-eval-defun))

  (use-feature desktop
    :demand t
    :config
    (setq desktop-save t)
    (desktop-save-mode 1))

  (use-package vterm
    :config
    (evil-set-initial-state 'vterm-mode 'emacs)

    (defun razzi-vterm-send-c-w ()
      (interactive)
      (vterm-send-key "w" nil nil t))

    (defun razzi-vterm-send-m-b ()
      (interactive)
      (vterm-send-key "b" nil t nil))

    (defun razzi-vterm-send-m-f ()
      (interactive)
      (vterm-send-key "f" nil t nil))

    ;; These next 2 require fish integration
    (defun razzi-vterm-send-s-up ()
      (interactive)
      (vterm-send-key "<up>" t nil nil))

    (defun razzi-vterm-send-s-down ()
      (interactive)
      (vterm-send-key "<down>" t nil nil))

    (general-define-key :keymaps 'vterm-mode-map
                        "<tab>" #'vterm--self-insert
                        "C-a" #'vterm--self-insert
                        "C-c" #'vterm--self-insert
                        "C-e" #'vterm--self-insert
                        "C-h" #'vterm--self-insert
                        "C-n" #'vterm--self-insert
                        "C-p" #'vterm--self-insert
                        "C-u" #'vterm--self-insert

                        "M-v" #'vterm-yank
                        "M-w" #'kill-this-buffer

                        "<s-backspace>" #'razzi-vterm-send-c-w

                        ;; todo bind other than arrow keys to stay on home row
                        "<s-up>" #'razzi-vterm-send-s-up
                        "<s-down>" #'razzi-vterm-send-s-down

                        ;; These are remapped to c-q andn c-v system-wide
                        "<s-left>" #'razzi-vterm-send-m-b
                        "<s-right>" #'razzi-vterm-send-m-f)

    (general-define-key :keymaps 'vterm-mode-map
                        :prefix "C-SPC"
                        "" nil
                        "c" 'vterm))

  (use-package vterm-toggle)

  (use-package golden-ratio
    :demand t
    :config (golden-ratio-mode +1)
    :blackout t)

  (use-package iedit
    :general
    (:states 'normal :prefix "SPC"
             "ie" 'iedit-mode))

  (use-feature hippie-exp
    :config
    (setq hippie-expand-try-functions-list
          '(try-expand-line try-expand-line-all-buffers)))

  (use-feature ffap
    :demand t)

  (use-feature dumb-jump
    :config
    (setq dumb-jump-confirm-jump-to-modified-file nil)
    :general
    (:states 'normal "g ]" 'dumb-jump-go))

  (defun razzi-mouse-open-file-or-url-on-click ()
    (interactive)
    (let* ((string-at-point (ffap-string-at-point))
          (parts (split-string string-at-point ":"))
          (filename (car parts))
          (line-number (cadr parts)))
      (if (and (not (string-empty-p string-at-point))
              (file-exists-p filename))
          (progn
            (find-file filename)
            (when line-number
              (goto-line line-number)))
        (browse-url-at-point))))

  (global-set-key (kbd "<mouse-1>") 'razzi-mouse-open-file-or-url-on-click)

  (recentf-mode)

  (server-start))
