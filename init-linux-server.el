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
