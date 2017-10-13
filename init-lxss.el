(bind-keys* ("C-\\" . undo)
	    ("C-x C-n" . set-mark-command))
(use-package nadvice
  :commands advice-add)

;; from subr.el in emacs 25.1
(defun special-form-p (object)
  "Non-nil if and only if OBJECT is a special form."
  (if (and (symbolp object) (fboundp object))
      (setq object (indirect-function object)))
  (and (subrp object) (eq (cdr (subr-arity object)) 'unevalled)))

;; from gv.el in emacs 25.1
;;;###autoload
(defmacro gv-ref (place)
  "Return a reference to PLACE.
This is like the `&' operator of the C language.
Note: this only works reliably with lexical binding mode, except for very
simple PLACEs such as (symbol-function \\='foo) which will also work in dynamic
binding mode."
  (let ((code
         (gv-letplace (getter setter) place
           `(cons (lambda () ,getter)
                  (lambda (gv--val) ,(funcall setter 'gv--val))))))
    (if (or lexical-binding
            ;; If `code' still starts with `cons' then presumably gv-letplace
            ;; did not add any new let-bindings, so the `lambda's don't capture
            ;; any new variables.  As a consequence, the code probably works in
            ;; dynamic binding mode as well.
            (eq (car-safe code) 'cons))
        code
      (macroexp--warn-and-return
       "Use of gv-ref probably requires lexical-binding"
       code))))

(defsubst gv-deref (ref)
  "Dereference REF, returning the referenced value.
This is like the `*' operator of the C language.
REF must have been previously obtained with `gv-ref'."
  (funcall (car ref)))
;; Don't use `declare' because it seems to introduce circularity problems:
;; Warning: Eager macro-expansion skipped due to cycle:
;;  … => (load "gv.el") => (macroexpand-all (defsubst gv-deref …)) => (macroexpand (defun …)) => (load "gv.el")
(gv-define-setter gv-deref (v ref) `(funcall (cdr ,ref) ,v))
