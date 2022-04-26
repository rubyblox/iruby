;;; iruby-comp.el --- compilation support from inf-ruby

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

(defvar iruby-breakpoint-pattern "\\(\\[1\\] pry(\\)\\|\\((rdb:1)\\)\\|\\((byebug)\\)"
  "Pattern found when a breakpoint is triggered in a compilation session.
This checks if the current line is a pry or ruby-debug prompt.")

(defvar iruby-orig-compilation-mode nil
  "Original compilation mode before switching to `iruby-mode'.")

(defvar iruby-orig-process-filter nil
  "Original process filter before switching to `iruby-mode'.")

(defvar iruby-orig-error-regexp-alist nil
  "Original `compilation-error-regexp-alist' before switching to `iruby-mode.'")

(defun iruby-switch-from-compilation ()
  "Make the buffer writable and switch to `iruby-mode'.
Recommended for use when the program being executed enters
interactive mode, i.e. hits a debugger breakpoint."
  (interactive)
  (setq buffer-read-only nil)
  (buffer-enable-undo)
  (let ((mode major-mode)
        (arguments compilation-arguments)
        (orig-mode-line-process mode-line-process)
        (orig-error-alist compilation-error-regexp-alist))
    (iruby-mode)
    (make-local-variable 'iruby-orig-compilation-mode)
    (setq iruby-orig-compilation-mode mode)
    (set (make-local-variable 'compilation-arguments) arguments)
    (set (make-local-variable 'iruby-orig-error-regexp-alist)
         orig-error-alist)
    (when orig-mode-line-process
      (setq mode-line-process orig-mode-line-process)))
  (let ((proc (get-buffer-process (current-buffer))))
    (when proc
      (make-local-variable 'iruby-orig-process-filter)
      (setq iruby-orig-process-filter (process-filter proc))
      (set-process-filter proc 'comint-output-filter))
    (when (looking-back iruby-prompt-pattern (line-beginning-position))
      (let ((line (match-string 0)))
        (delete-region (match-beginning 0) (point))
        (comint-output-filter proc line)))))

(defun iruby-maybe-switch-to-compilation ()
  "Switch to compilation mode this buffer was in before
`iruby-switch-from-compilation' was called, if it was.
Otherwise, just toggle read-only status."
  (interactive)
  (if iruby-orig-compilation-mode
      (let ((orig-mode-line-process mode-line-process)
            (proc (get-buffer-process (current-buffer)))
            (arguments compilation-arguments)
            (filter iruby-orig-process-filter)
            (errors iruby-orig-error-regexp-alist))
        (unwind-protect
             (funcall iruby-orig-compilation-mode)
          (setq mode-line-process orig-mode-line-process)
          (set (make-local-variable 'compilation-arguments) arguments)
          (set (make-local-variable 'compilation-error-regexp-alist) errors)
          (when proc
            (set-process-filter proc filter))))
    (toggle-read-only)))


;;;###autoload
(defun iruby-switch-setup ()
  "Modify `rspec-compilation-mode' and `ruby-compilation-mode'
keymaps to bind `iruby-switch-from-compilation' to `С-x C-q'."
  (eval-after-load 'rspec-mode
    '(define-key rspec-compilation-mode-map (kbd "C-x C-q")
       'iruby-switch-from-compilation))
  (eval-after-load 'ruby-compilation
    ;; NB available in the rinari src tree, or separately via melpa/...
    '(define-key ruby-compilation-mode-map (kbd "C-x C-q")
       'iruby-switch-from-compilation))
  (eval-after-load 'projectile-rails
    '(define-key projectile-rails-server-mode-map (kbd "C-x C-q")
       'iruby-switch-from-compilation)))


(defun iruby-in-ruby-compilation-modes (mode)
  "Check if MODE is a Ruby compilation mode."
  (member mode '(rspec-compilation-mode
                 ruby-compilation-mode
                 projectile-rails-server-mode
                 minitest-compilation-mode)))

;;;###autoload
(defun iruby-auto-enter ()
  "Switch to `iruby-mode' if the breakpoint pattern matches the current line."
  (when (and (iruby-in-ruby-compilation-modes major-mode)
             (save-excursion
               (beginning-of-line)
               (re-search-forward iruby-breakpoint-pattern nil t)))
    ;; Exiting excursion before this call to get the prompt fontified.
    (iruby-switch-from-compilation)
    (add-hook 'comint-input-filter-functions 'iruby-auto-exit nil t)))

;;;###autoload
(defun iruby-auto-exit (input)
  "Return to the previous compilation mode if INPUT is a debugger exit command."
  (when (iruby-in-ruby-compilation-modes iruby-orig-compilation-mode)
    (if (member input '("quit\n" "exit\n" ""))
        ;; After the current command completes, otherwise we get a
        ;; marker error.
        (run-with-idle-timer 0 nil #'iruby-maybe-switch-to-compilation))))

(defun iruby-enable-auto-breakpoint ()
  (interactive)
  (add-hook 'compilation-filter-hook 'iruby-auto-enter))

(defun iruby-disable-auto-breakpoint ()
  (interactive)
  (remove-hook 'compilation-filter-hook 'iruby-auto-enter


(provide 'iruby-comp)
