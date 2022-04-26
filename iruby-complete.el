;;; iruby-complete.el --- completion supprot for iRuby buffers

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.

(require 'iruby)

(defconst iruby-ruby-expr-break-chars " \t\n\"\'`><,;|&{(")

(defun iruby-escape-single-quoted (str)
  "Escape single quotes, double quotes and newlines in STR."
  (replace-regexp-in-string "'" "\\\\'"
    (replace-regexp-in-string "\n" "\\\\n"
      (replace-regexp-in-string "\\\\" "\\\\\\\\" str))))

(defun iruby-completions (prefix)
  "Return a list of completions for the Ruby expression starting with EXPR."
  ;; NB for `iruby-completion-at-point' under buffer's
  ;; `completion-at-point-functions'
  (let* ((proc (iruby-proc))
         (line (buffer-substring
                (save-excursion (move-beginning-of-line 1)
                                (point))
                (point)))
         (expr (iruby-completion-expr-at-point))
         (prefix-offset (- (length expr) (length prefix)))
         (previous-filter (process-filter proc))
         (kept "")
         completions)
    (unwind-protect
         (progn
           (set-process-filter proc
                               (lambda (proc string)
                                 (setq kept (concat kept string))
                                 ;; ensure that the string is not displayed:
                                 nil))
           (let ((completion-snippet
                  (format (concat iruby-impl-completion-expr "; nil;\n")
                   (iruby-escape-single-quoted expr)
                   (iruby-escape-single-quoted line))))
             (process-send-string proc completion-snippet)
             (while (and (not (string-match iruby-prompt-pattern kept))
                         (accept-process-output proc 2)))
             (setq completions (butlast (split-string kept "\r?\n") 2))
             ;; Subprocess echoes output on Windows and OS X.
             (when (and completions (string= (concat (car completions) "\n") completion-snippet))
               (setq completions (cdr completions)))))
      ;; ensure:
      (set-process-filter proc previous-filter))

    (mapcar
     (lambda (str)
       (substring str prefix-offset))
     completions)))

(defun iruby-completion-bounds-of-prefix ()
  "Return bounds of expression at point to complete."
  (let ((iruby-ruby-expr-break-chars ;; NB this is the constant's sole usage
         (concat iruby-ruby-expr-break-chars ".")))
    (iruby-completion-bounds-of-expr-at-point)))

(defun iruby-completion-bounds-of-expr-at-point ()
  "Return bounds of expression at point to complete."
  (let ((s (char-syntax (following-char))))
    (save-excursion
      (when (eq s ?.)
        (backward-char))
      (let ((bounds (iruby-bounds-of-thing 'sexp)))
        ;; (warn "Bounds %S" bounds)
        (when (and (eq s ?.) bounds)
          ;; when completing at punctuation
          (let ((max (point-max))
                (end (cdr bounds)))
            (unless (= max end)
              (setf (cdr bounds) (1+ end)))))
        bounds))))

(defun iruby-completion-expr-at-point ()
  "Return expression at point to complete."
  (let ((bounds (iruby-completion-bounds-of-expr-at-point)))
    (and bounds
         (buffer-substring-no-properties (car bounds) (cdr bounds)))))


(defun iruby-completion-at-point ()
  "Retrieve the list of completions and prompt the user.
Returns the selected completion or nil."
  (cond
    (iruby-impl-completion-expr
     (let ((bounds (iruby-completion-bounds-of-prefix)))
       (when bounds
         (list (car bounds) (cdr bounds)
               (if (fboundp 'completion-table-with-cache)
                   (completion-table-with-cache #'iruby-completions)
                 (completion-table-dynamic #'iruby-completions))))))
    (t
     (iruby-warn-once "Completion not configured for implementation %s"
                      iruby-buffer-impl))))

(provide 'iruby-complete)
