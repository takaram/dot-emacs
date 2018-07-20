(eval-when-compile (require 'use-package))

(setf (cdr (assoc "melpa" package-archives))
      "http://melpa.org/packages/"
      (cdr (assoc "melpa-stable" package-archives))
      "http://stable.melpa.org/packages/")

;; add ruby-mode
(use-package ruby-mode
  :mode (("\\.rbw?\\'"  . ruby-mode)
         ("Gemfile\\'"  . ruby-mode)
         ("Rakefile\\'" . ruby-mode))
  :interpreter "ruby"
  :commands ruby-mode
  :no-require t)

;; for ideas2
(add-hook 'after-init-hook
          (lambda ()
            (when (string= (user-real-login-name) "ideas2")
              (setq backup-directory-alist
                    '((".*" . "/aptmp/ideas2/aramaki/.emacs_backup"))))))

;; color setup
(set-face-foreground 'font-lock-builtin-face "brightblue")
(set-face-foreground 'font-lock-comment-face "color-132")
(set-face-foreground 'font-lock-function-name-face "color-32")
(set-face-foreground 'font-lock-string-face "color-197")
(set-face-foreground 'minibuffer-prompt "brightblue")
(add-hook 'linum-mode-hook
          (lambda()
            (set-face-foreground 'linum "#999999")))

(use-package term
  :defer t
  :config
  (bind-key "C-c C-j" 'term-toggle-mode term-mode-map)
  (bind-key "C-c C-j" 'term-toggle-mode term-raw-map))

(defun ansi-term-in-right-window ()
  "Open `ansi-term' in new window."
  (interactive)
  (split-window-sensibly)
  (other-window 1)
  (ansi-term "/bin/bash")
  (other-window -1))

(defun goto-or-open-term-window ()
  "Go to term window."
  (interactive)
  (let* ((bufname "*ansi-term*")
         (buffer (get-buffer bufname))
         (window (get-buffer-window bufname)))
    (if window
        (select-window window)
      (if buffer
          (switch-to-buffer-other-window buffer)
        (progn (ansi-term-in-right-window)
               (other-window 1))))))
(bind-key "C-c t" 'goto-or-open-term-window)

(defun term-toggle-mode ()
  "Toggle term between line mode and char mode."
  (interactive)
  (if (term-in-char-mode)
      (term-line-mode)
    (term-char-mode)))

(add-hook 'term-mode-hook
          (lambda () (linum-mode -1)))
