;;;
;;; Package settings
;;;
(require 'package)
(setq package-user-dir (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(use-package bind-key
  :ensure t)

;;;
;;; load init file specific to the environment
;;;
(let ((file (expand-file-name "init-local" user-emacs-directory)))
  (if (or (file-readable-p (concat file ".elc"))
          (file-readable-p (concat file ".el")))
      (load file)))

(setq custom-file (expand-file-name ".custom-file.el" user-emacs-directory))
;; (if (file-readable-p custom-file)
;;     (load-file custom-file))

(unless backup-directory-alist
  (setq backup-directory-alist
	`((".*" . ,(expand-file-name "backup" user-emacs-directory)))))

(setq inhibit-startup-message t)

(global-linum-mode t)
(line-number-mode t)
(column-number-mode t)

(tool-bar-mode 0)
(menu-bar-mode 0)

(ido-mode 1)

(setq frame-title-format (format "%%b - Emacs %s" emacs-version))

(setq-default line-spacing 0.1)

(delete-selection-mode t)

(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

(when window-system
  (global-hl-line-mode t)
  (set-face-background 'hl-line "#ccffff"))
(show-paren-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil)

(setq-default show-trailing-whitespace t)
(dolist (hook (list
               'buffer-menu-mode-hook
               'shell-mode-hook
               'term-mode-hook
               'eshell-mode-hook))
  (add-hook hook
            (lambda ()
              (setq show-trailing-whitespace nil))))

(setq backup-by-copying-when-linked t)

(setq scroll-conservatively 1
      scroll-preserve-screen-position t
      scroll-error-top-bottom t)

(defun add-parent-directory-to-buffer-name (&rest args)
  (let ((file (or buffer-file-name dired-directory)))
    (when file
      (when (not (string= file "~/"))
        (setq file (abbreviate-file-name (directory-file-name file))))
      (let* ((parent (directory-file-name
                      (file-name-directory file)))
             (dir (if (string-match "\\`/[^/]*\\'" parent)
                      parent
                    (file-name-nondirectory parent))))
        (rename-buffer (concat
                        (file-name-as-directory dir)
                        (file-name-nondirectory file)))))))
(add-hook 'find-file-hook 'add-parent-directory-to-buffer-name)

(defun switch-buffer-skipping-special-buffer (next)
  (let ((start-buf (buffer-name))
        (func (if next 'next-buffer 'previous-buffer)))
    (funcall func)
    (while (and (string-match "\\`\\*.+\\*\\'" (buffer-name))
                (not (string= start-buf (buffer-name))))
      (funcall func))))
(defun next-buffer-skipping-special-buffer ()
  (interactive)
  (switch-buffer-skipping-special-buffer t))
(defun previous-buffer-skipping-special-buffer ()
  (interactive)
  (switch-buffer-skipping-special-buffer nil))
(bind-key "C-<right>" 'next-buffer-skipping-special-buffer)
(bind-key "C-<left>" 'previous-buffer-skipping-special-buffer)

(defun revert-buffer-no-confirm (&optional force-reverting)
  "Interactive call to `revert-buffer'.  Ignoring the auto-save
 file and not requesting for confirmation.  When the current buffer
 is modified, the command refuses to revert it, unless you specify
 the optional argument: force-reverting to true."
  (interactive "P")
  (if (or force-reverting (not (buffer-modified-p)))
      (revert-buffer t t t)
    (error "The buffer has been modified")))
(bind-key "<f5>" 'revert-buffer-no-confirm)

(bind-key "C-m" 'reindent-then-newline-and-indent)
(bind-key "C-j" 'newline)
(defun indent-all ()
  (interactive)
  (indent-region (point-min) (point-max)))
(bind-key "C-M-]" 'indent-all)

(bind-key "C-k" 'kill-whole-line)
(bind-key "M-k" 'kill-line)

(bind-key "C-x k" 'kill-this-buffer)
(defun kill-this-buffer-and-window()
  (interactive)
  (kill-this-buffer)
  (delete-window))
(bind-key "C-x 9" 'kill-this-buffer-and-window)

;;(bind-key "C-x C-r" 'replace-string)
(bind-key "C-x C-r" 'replace-regexp)

(bind-key "C-c <left>"  'windmove-left)
(bind-key "C-c <right>" 'windmove-right)
(bind-key "C-c <up>"    'windmove-up)
(bind-key "C-c <down>"  'windmove-down)

;;(require 'dashboard)
;;(dashboard-setup-startup-hook)

(use-package ruby-mode
  :defer t
  :config
  (add-hook 'ruby-mode-hook
            (lambda ()
              (setq ruby-deep-indent-paren-style nil
                    ruby-insert-encoding-magic-comment nil)
              (global-rbenv-mode 1)
              (flycheck-mode 1))))
(use-package rbenv
  :commands (global-rbenv-mode rbenv-use rbenv-use-system rbenv-use-corresponding)
  :ensure t)

(use-package org
  :bind ("C-c C-c" . org-capture)
  :defines org-capture-templates
  :config
  (setq org-return-follows-link t
        org-startup-folded nil)
  (setcdr (assoc 'file org-link-frame-setup) 'find-file))

(defun open-startup-menu ()
  (let ((path-to-memo "~/ownCloud/memo.org")
        (original-directory default-directory))
    (when (file-readable-p path-to-memo)
      (find-file path-to-memo)
      (setq org-capture-templates
            '(("t" "TODO" entry (file+headline path-to-memo "Tasks")
               "* TODO %?\n\n")))
      (cd original-directory))))
(add-hook 'after-init-hook 'open-startup-menu)

(defun dired-find-file-or-alternate-directory ()
  (interactive)
  (if (file-directory-p (dired-get-file-for-visit))
      (dired-find-alternate-file)
    (dired-find-file)))
(add-hook 'dired-load-hook
          (lambda ()
            (setq dired-listing-switches "-alhF"
                  line-spacing nil)
            (put 'dired-find-alternate-file 'disabled nil)
            (bind-keys :map dired-mode-map
                       ("RET" . dired-find-file-or-alternate-directory)
                       ([mouse-2] . dired-find-file-or-alternate-directory))))
(use-package dired-toggle
  :bind ("C-x C-d" . dired-toggle)
  :config
  (setq dired-toggle-window-side 'below
        dired-toggle-window-size 8))

(use-package sh-script
  :config
  (sh-electric-here-document-mode -1))

(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2)
  :ensure t)

(use-package markdown-mode
  :mode ("\\.markdown\\'" "\\.md\\'")
  :no-require t)

(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :no-require t)

(use-package sed-mode
  :mode "\\.sed\\'"
  :interpreter "sed"
  :no-require t)

(use-package slim-mode
  :mode "\\.slim\\'"
  :config
  (bind-key "C-m" 'newline-and-indent slim-mode-map))

(use-package ess-site
  :load-path "/usr/share/emacs24/site-lisp/ess"
  :defer t
  :no-require t)

(use-package multi-term
  :commands multi-term
  :config
  (add-hook 'term-mode-hook
            (lambda ()
              (setq show-trailing-whitespace nil)
              (linum-mode 0)
              (hl-line-mode 0)
              (set-face-background 'hl-line (face-background 'default))))
  :no-require t)

(use-package magit
  :bind ("C-c C-g" . magit-status))

(use-package comment-mode
  :bind ("M-c" . comment-mode)
  ;; load from ~/.emacs.d/lisp
  :load-path "lisp")

(use-package flycheck
  :config
  (global-flycheck-mode)
  :ensure t)

(use-package company
  :config
  (global-company-mode)
  :ensure t)

(use-package coffee-mode
  :mode "\\.coffee\\'"
  :config
  (add-hook 'coffee-mode-hook
            (lambda ()
              (setq coffee-tab-width 4
                    tab-width 4))))

(use-package ng2-mode
  :mode "\\.ts\\'"
  :config
  (add-hook 'ng2-typescript-mode-hook
            (lambda ()
              (setq typescript-indent-level 2)))
  :no-require t)

(defun compile-init-el ()
  (dolist (file (list
                 user-init-file
                 (expand-file-name "init-local.el" user-emacs-directory)))
    (when (and file (file-readable-p file)
               (file-newer-than-file-p file (concat file "c")))
      (byte-compile-file file))))
(add-hook 'after-init-hook 'compile-init-el)
(add-hook 'kill-emacs-hook 'compile-init-el)
