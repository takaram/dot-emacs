;;; init-windows.el --- supplemental init.el for Windows
;;; Commentary:
;;; Code:
;; setting for tramp
(eval-when-compile (require 'tramp))
(defun get-ip-address ()
  "Win32: get the IP-address of the first network interface."
  (let ((cmd (if (string-match "bash" shell-file-name)
                 "hostname -I" "ipconfig | findstr IPv4")))
    (let ((ipconfig (shell-command-to-string cmd)))
      (string-match "\\(\\([0-9]+.\\)+[0-9]+\\)" ipconfig)
      (match-string 0 ipconfig))))
(with-eval-after-load 'tramp
  (setenv "PATH" (concat "C:\\Program Files (x86)\\PuTTY" ";" (getenv "PATH")))
  (setq tramp-default-method "plink")
  ;; tunneling
  (when (not (string-match "^133\\.103\\.101\\." (get-ip-address)))
    (add-to-list 'tramp-default-proxies-alist
                 '("uv20" nil "/plink:fe1:"))
    (add-to-list 'tramp-default-proxies-alist
                 '("ubuntu" nil "/uv20:"))
    (add-to-list 'tramp-default-method-alist
                 '("uv20\\|ubuntu" nil "ssh"))))

(when (member "Ricty Diminished" (font-family-list))
  (set-frame-font "Ricty Diminished-11"))

(setq initial-frame-alist
      (append (list '(width . 82))
              initial-frame-alist))

(setq shell-file-name "C:/Windows/System32/bash.exe"
      multi-term-program shell-file-name)

;; IME settings
(w32-ime-initialize)
(setq default-input-method "W32-IME")
(setq-default w32-ime-mode-line-format-original "[--]")
(setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))

(provide 'init-windows)
;;; init-windows.el ends here
