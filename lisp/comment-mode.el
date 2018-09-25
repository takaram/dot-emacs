;;; comment-mode.el --- Minor mode commenting out what you type automatically

;; Author: Takuya Aramaki
;; Version: 0.1.0
;; Package-Requeires: ()
;;; Commentary:
;; This program is a free software.

;;; Code:

;;; Compatibility
(eval-and-compile
  (defun comment--line (n)
    "Comment or uncomment current line."
    (interactive "p")
    (if (use-region-p)
        (comment-or-uncomment-region
         (save-excursion
           (goto-char (region-beginning))
           (line-beginning-position))
         (save-excursion
           (goto-char (region-end))
           (line-end-position)))
      (let ((range
             (list (line-beginning-position)
                   (goto-char (line-end-position n)))))
        (comment-or-uncomment-region
         (apply #'min range)
         (apply #'max range))))))

(defgroup comment nil
  "Minor mode to comment out what you type")

;;;###autoload
(define-minor-mode comment-mode
  "Automatically comment out what you type.
This minor mode would be useful when you add documentation to your classes, functions, etc."
  :init-value nil
  :lighter " comment"
  :keymap '([C-m] . comment-indent-new-line)
  :global nil
  :group 'comment

  (comment--line 1))

(provide 'comment-mode)
;;; comment-mode ends here
