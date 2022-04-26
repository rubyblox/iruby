;;; iruby-console.el --- project support for iruby

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

(eval-when-compile
  (require 'cl-macs))

(require 'iruby)

(defvar iruby-console-patterns-alist
  '(("Gemfile" . default)
    ("*.gemspec" . gem)
    (".zeus.sock" . zeus)
    (iruby-console-rails-p . rails)
    (iruby-console-hanami-p . hanami)
    (iruby-console-script-p . script)
    (iruby-console-racksh-p . racksh))
  "Mapping from predicates (wildcard patterns or functions) to type symbols.
`iruby-console-auto' walks up from the current directory until
one of the predicates matches, then calls `iruby-console-TYPE',
passing it the found directory.")

;;;###autoload
(defun iruby-file-contents-match (file regexp &optional match-group)
  (with-temp-buffer
    (insert-file-contents file)
    (when (re-search-forward regexp nil t)
      (if match-group
          (match-string match-group)
        t))))

(defun iruby-expand-files (path &optional full)
  "expand any glob patterns in PATH, returning a list of file pathnames

PATH should contain a literal pathname expression or a pathname
expression including glob patterns.

If FULL is non-nil, a list of absolute pathnames will be returned. By
default, pathnames will be returned as relative to PATH.

This function will ensure that any autosave files or symbolic links to
noneixstent files will not be present in the returned list of
pathnames.

The syntax for PATH would be that used in `file-expand-wildcards'"
  (cl-remove-if 'auto-save-file-name-p
                (cl-remove-if-not 'file-exists-p
                                  (file-expand-wildcards path))))


(defun iruby-console-match (dir)
  "Find matching console command for DIR, if any."
  (catch 'type
    (dolist (pair iruby-console-patterns-alist)
      (let ((default-directory dir)
            (pred (car pair)))
        (when (if (stringp pred)
                  (iruby-expand-files pred)
                (funcall pred))
          (throw 'type (cdr pair)))))))

;;;###autoload
(defun iruby-console-auto (&optional dir new)
  "Run the appropriate Ruby console command, or the default iRuby

The command and the directory to run it from are detected
automatically.

If an iRuby buffer exists for DIR, that buffer will be selected, else a
new iRuby process will be created.

If no matching project files can be found, this will run the default
iRuby implementation, with `default-directory' set to DIR.

If called interactively, the selected buffer's `default-directory' will
be used as DIR unless called with a prefix argument. If called with a
prefix argument, the user will be asked to select a directory for the
console"
  (interactive
   (list (if current-prefix-arg
             (let ((choice
                    (read-directory-name "Run iRuby console in directory: "
                                         nil nil t)))
               (cond
                 ((zerop (length choice)) default-directory)
                 (t choice)))
           ;; else, just use default-directory
           default-directory)
         current-prefix-arg))
  (let* ((default-directory dir)
         (use-dir
          ;; FIXME not exactly working out
          (locate-dominating-file default-directory #'iruby-console-match))
         ;; FIXME need an iruby-debug option ...
         ;; -debug which dir iruby-console-match has located
         (type (iruby-console-match use-dir))
         (fun (when type (intern (format "iruby-console-%s" type)))))
    (cond
      ((null type)
       (warn "iruby-console-auto: No console available for %s" dir)
       (iruby))
      ((fboundp fun)
       (funcall fun use-dir new))
      (t
       (error "Console function not bound: %S" fun)))))

(defun iruby-console-rails-p ()
  (and (file-exists-p "Gemfile.lock")
       (iruby-file-contents-match "Gemfile.lock" "^ +railties ")
       (file-exists-p "config/application.rb")
       (iruby-file-contents-match "config/application.rb"
                                     "\\_<Rails::Application\\_>")))

(defun iruby-console-read-directory (type)
  (or
   (let ((predicate (car (rassq type iruby-console-patterns-alist))))
     (locate-dominating-file (read-directory-name "" nil nil t)
                             (lambda (dir)
                               (let ((default-directory dir))
                                 (if (stringp predicate)
                                     (iruby-expand-files predicate)
                                   (funcall predicate))))))
   ;; FIXME do not err in this case, just run a normal `iruby'
   ;; ... with warning
   ;;
   ;; this should not be where an assert is buried for known-console-dir-p etc
   (error "No matching directory for %s console found"
          (capitalize (symbol-name type)))))

(defun iruby-find-console-buffer (dir name)
  (let* ((dir-attrs (file-attributes dir 'integer))
         (dir-device (file-attribute-device-number dir-attrs))
         (dir-ino (file-attribute-inode-number dir-attrs)))
    (cl-block search
      (cl-dolist (elt iruby-process-buffers)
        (let ((buff (cdr elt)))
          (when (buffer-live-p buff)
            (with-current-buffer buff
              ;; FIXME stop using the buffer impl name as a project/console name
              (and (equal name (iruby-impl-name iruby-buffer-impl))
                   (let* ((o-attrs (file-attributes default-directory 'integer))
                          (o-device (file-attribute-device-number o-attrs))
                          (o-ino (file-attribute-inode-number o-attrs)))
                     (= o-device dir-device)
                     (= o-ino dir-ino))
                   (cl-return-from search buff)))))))))

(defun iruby-console-activate (impl name &optional new)
  (let ((exists (unless new
                  (iruby-find-console-buffer name default-directory))))
    (run-iruby impl name new)))

(defun iruby-console-wrap (cmd name &optional new)
  (when (stringp cmd)
    (setq cmd (iruby-split-shell-string cmd)))
  (let ((binding (iruby-get-default-interactive-binding)))
    (iruby-console-activate (iruby-wrap-binding cmd binding name)
                            name new)))

;;;###autoload
(defun iruby-console-zeus (dir &optional new)
  "Run Rails console in DIR using Zeus."
  (interactive (list (iruby-console-read-directory 'zeus)
                     current-prefix-arg))
  (let* ((default-directory (file-name-as-directory dir))
         (wrap-cmd  '("zeus" "console"))
         (binding (iruby-get-default-interactive-binding)))
    (unless (executable-find "zeus")
      (setq wrap-cmd (append '("bundle" "exec") wrap-cmd)))
    (iruby-console-wrap wrap-cmd "zeus" new)))

;;;###autoload
(defun iruby-console-rails (dir &optional new)
  "Run Rails console in DIR."
  (interactive (list (iruby-console-read-directory 'rails)
                     interactive-prefix-arg))
  (let* ((default-directory dir)
         (env (iruby-console-rails-env))
         (with-bundler (file-exists-p "Gemfile"))
         (wrap-cmd '("rails" "console" "-e"))
         (binding  (iruby-get-default-interactive-binding)))
    (when with-bundler
      (setq wrap-cmd (append '("bundle" "exec") wrap-cmd)))
    (iruby-console-wrap wrap-cmd"rails" new)))

(defun iruby-console-rails-env ()
  (if (stringp iruby-console-environment)
      iruby-console-environment
    (let ((envs (iruby-console-rails-envs)))
      (completing-read "Rails environment: "
                       envs
                       nil t
                       nil nil (car (member "development" envs))))))

(defun iruby-console-rails-envs ()
  (let ((files (iruby-expand-files "config/environments/*.rb")))
    (if (null files)
        (error "No files in %s" (expand-file-name "config/environments/"))
      (mapcar #'file-name-base files))))

(defun iruby-console-hanami-p ()
  (and (file-exists-p "config.ru")
       (iruby-file-contents-match "config.ru" "\\_<run Hanami.app\\_>")))

(defun iruby-console-hanami (dir &optional new)
  "Run Hanami console in DIR."
  (interactive (list (iruby-console-read-directory 'hanami)
                     current-prefix-arg))
  (let* ((default-directory (file-name-as-directory dir))
         (env (iruby-console-hanami-env))
         (with-bundler (file-exists-p "Gemfile"))
         (wrap-cmd '("hanami" "console")))
    (when with-bundler
      (setq wrap-cmd (append '("bundle" "exec") wrap-cmd)))
    (with-iruby-process-environment ((format "HANAMI_ENV=%s" env))
      (iruby-console-wrap wrap-cmd "hanmai" new))))

(defun iruby-console-hanami-env ()
  (if (stringp iruby-console-environment)
      iruby-console-environment
    (let ((envs '("development" "test" "production")))
      (completing-read "Hanami environment: "
                       envs
                       nil t
                       nil nil (car (member "development" envs))))))

;;;###autoload
(defun iruby-console-gem (dir &optional new)
  "Run IRB console for the gem in DIR.
The main module should be loaded automatically.  If DIR contains a
Gemfile, it should use the `gemspec' instruction."
  (interactive (list (iruby-console-read-directory 'gem)
                     current-prefix-arg))
  (let* ((default-directory (file-name-as-directory dir))
         ;; NB picking the first gemspec file here, if mutiple are available
         (gemspec (car (iruby-expand-files "*.gemspec")))
         (name  (iruby-file-contents-match
                 gemspec "\\.name[ \t]*=[ \t]*['\"]\\([^'\"]+\\)['\"]" 1))
         (args (when (file-directory-p "lib")
                 (list "-I" "lib")))
         (bind (iruby-get-default-interactive-binding))
         wrapper)
    (setq name
          (format "gem(%s)"
                  (cond
                    (name (string-trim name))
                    (t (file-name-sans-extension gemspec))))
          wrapper (iruby-wrap-binding nil bind name))
    (when args
      (setf (iruby:interactive-args wrapper)
            (append (iruby:interactive-args wrapper)
                    args)))
    ;; calling this directly here, due to the args append
    (iruby-console-activate wrapper name new)))

(defun iruby-console-racksh-p ()
  (and (file-exists-p "Gemfile.lock")
       (iruby-file-contents-match "Gemfile.lock" "^ +racksh ")))

(defun iruby-console-racksh (dir &optional new)
  "Run racksh in DIR."
  (interactive (list (iruby-console-read-directory 'racksh)
                     current-prefix-arg))
  (let ((default-directory (file-name-as-directory dir)))
    (iruby-console-wrap '("bundle" "exec" "rackish") "rackish" new)))

(defun iruby-console-script-p ()
  (and (file-exists-p "Gemfile.lock")
       (or
        (file-exists-p "bin/console")
        (file-exists-p "console")
        (file-exists-p "console.rb"))))

;;;###autoload
(defun iruby-console-script (dir &optional new)
  "Run custom bin/console, console or console.rb in DIR."
  ;; FIXME the console cmd may need additional configuration for use
  ;; under iruby
  (interactive (list (iruby-console-read-directory 'script)
                     current-prefix-arg))
  (let ((default-directory (file-name-as-directory dir))
        (wrap-cmd '("bundle" "exec"))
        name)
    (cond
     ((file-exists-p "bin/console")
      (setq wrap-cmd (append wrap-cmd (setq name "bin/console"))))
     ((file-exists-p "console.rb")
      (setq wrap-cmd (append wrap-cmd (list "ruby" (setq name "console.rb")))))
     ((file-exists-p "console")
      (setq wrap-cmd (append wrap-cmd (list (setq name "console"))))))
    (cond
      (name (iruby-console-wrap wrap-cmd name new))
      (t (error "Found no console script in %S" dir)))))

;;;###autoload
(defun iruby-console-default (dir &optional new)
  "Run Pry or the default iRuby as a bundler console in DIR"
  (interactive (list (iruby-console-read-directory 'default)
                     current-prefix-arg))
  (let ((default-directory dir))
    (cond
      ((file-exists-p "Gemfile")
       (let* ((project (file-name-nondirectory (directory-file-name dir)))
              (name (format "console (%s)" project)))
         (iruby-console-wrap '("bundle" "exec") name new)))
      (t
       (error "Unable to run a bundler console in a directory with no Gemfile")))))

(provide 'iruby-console)
