(let ((lisp-dir (expand-file-name "lisp" user-emacs-directory)))
  (setq package-user-dir lisp-dir)
  (add-to-list 'load-path lisp-dir)
  (dolist (child (directory-files lisp-dir t nil t))
    (when (and (file-directory-p child)
               (not (string-match-p "^\\." (file-name-nondirectory child))))
      (add-to-list 'load-path (expand-file-name child lisp-dir)))))
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(use-package bind-key
  :ensure t)

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

(defun add-parent-directory-in-buffer-name (&rest args)
  (let ((file (or buffer-file-name dired-directory)))
    (when file
      (setq file (directory-file-name file))
      (let* ((parent (directory-file-name
                      (file-name-directory file)))
             (dir (if (string= parent "/")
                      "/" (file-name-as-directory
                           (file-name-nondirectory parent)))))
        (rename-buffer (concat dir (file-name-nondirectory file)))))))
(advice-add 'find-file :after 'add-parent-directory-in-buffer-name)

(defun revert-buffer-no-confirm (&optional force-reverting)
  "Interactive call to revert-buffer. Ignoring the auto-save
 file and not requesting for confirmation. When the current buffer
 is modified, the command refuses to revert it, unless you specify
 the optional argument: force-reverting to true."
  (interactive "P")
  (if (or force-reverting (not (buffer-modified-p)))
      (let ((mm (with-current-buffer (current-buffer)
                  major-mode)))
        (revert-buffer :ignore-auto :noconfirm)
        (with-current-buffer (current-buffer) (funcall mm)))
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

;;(require 'dashboard)
;;(dashboard-setup-startup-hook)

(eval-when-compile (require 'ruby-mode nil t))
(add-hook 'ruby-mode-hook
          (lambda ()
            (setq ruby-deep-indent-paren-style nil
                  ruby-insert-encoding-magic-comment nil)))
;; (add-hook 'ruby-mode-hook
;; 	  (lambda () (hs-minor-mode 1)
;;             (bind-key "C-c h" 'hs-toggle-hiding)
;;             (add-to-list 'hs-special-modes-alist
;;                          '(ruby-mode
;;                            "\\(def\\|do\\|{\\)" "\\(end\\|end\\|}\\)" "#"
;;                            (lambda (arg) (ruby-end-of-block)) nil))))

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
  :bind ("C-c C-g" . magit-status)
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
