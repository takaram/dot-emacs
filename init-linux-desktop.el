(setq initial-frame-alist
      (append (list
	       '(width . 91)
	       '(height . 75)
	       '(top . 0)
	       '(left . 1060)
	       )
	      initial-frame-alist))
(setq default-frame-alist initial-frame-alist)

(setq tramp-default-method "ssh")

(set-language-environment "Japanese")
(use-package mozc
  :load-path "/usr/local/share/emacs24/site-lisp/emacs-mozc"
  :commands mozc-mode
  :bind ([zenkaku-hankaku] . toggle-input-method)
  :init
  (setq default-input-method "japanese-mozc")
  (advice-add 'toggle-input-method :before 'mozc-mode)
  :config
  (advice-remove 'toggle-input-method 'mozc-mode))

(set-fontset-font t 'japanese-jisx0208
                  (font-spec :family "TakaoGothic"))

(when (>= emacs-major-version 25)
  (setq load-path
        (append load-path
                '("/usr/share/emacs/site-lisp"
                  "/usr/share/emacs24/site-lisp"
                  "/usr/share/emacs/24.5/lisp"
                  "/usr/share/emacs/24.5/lisp/emacs-lisp"))))
