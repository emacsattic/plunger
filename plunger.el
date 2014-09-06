;;; plunger.el --- use Git plumbing commands on the region

;; Copyright (C) 2012-2013  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20120112
;; Version: 0.2.2
;; Status: experimental
;; Homepage: http://github.com/tarsius/plunger
;; Keywords: compile, convenience, lisp

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides function which use the Git plumbing commands
;; `hash-object', `mktree' and `commit-tree' on the active region.

;;; Code:

(require 'cl-lib)
(require 'magit)

(declare-function elx-maintainers 'elx)
(declare-function mm-decompress-buffer 'mm-util)
(declare-function request 'request)

(defun plunger-region-blob (from to &optional filename perms)
  (interactive "r")
  (let ((inhibit-read-only t)
        (inhibit-point-motion-hooks t))
    (goto-char to)
    (when (> (call-process-region from to magit-git-executable
                                  nil t nil
                                  "hash-object" "--stdin" "-w") 0)
      (error "Cannot hash object"))
    (kill-region from to)
    (backward-char)
    (goto-char (line-beginning-position))
    (insert (or perms "100644") " blob ")
    (goto-char (line-end-position))
    (insert "\t" (or filename "region"))
    (buffer-substring (line-beginning-position) (point))))

(defun plunger-region-mktree (from to)
  (interactive "r")
  (let ((inhibit-read-only t)
        (inhibit-point-motion-hooks t))
    (goto-char to)
    (when (> (call-process-region from to magit-git-executable
                                  nil t nil "mktree") 0)
      (error "Cannot make tree"))
    (kill-region from to)
    (backward-char)
    (delete-char 1)
    (buffer-substring (line-beginning-position) (point))))

(defun plunger-region-commit (from to &optional message)
  (interactive "r")
  (let ((inhibit-read-only t)
        (inhibit-point-motion-hooks t))
    (plunger-commit (buffer-substring from to) "update")))

(defvar plunger-pre-commit-hook nil)

(defun plunger-commit (tree message)
  (let ((process-environment (cl-copy-list process-environment))
        commit)
    (run-hook-with-args 'plunger-pre-commit-hook tree)
    (if (equal tree (magit-git-string "rev-parse" "HEAD^{tree}"))
        (progn (message "Refusing to make an empty commit") nil)
      (setq commit
            (apply 'magit-git-string "commit-tree" tree
                   "-m" message
                   (unless (magit-no-commit-p)
                     (list "-p" (magit-git-string "rev-parse" "HEAD")))))
      (if (not commit)
          (error "Cannot commit tree")
        (magit-run-git "update-ref" (magit-get-ref "HEAD") commit)
        (magit-git-string "rev-parse" "HEAD")))))

(defun plunger-pull-file (url filename message)
  "Fetch URL and commit it in the current repository as FILENAME.
If file is compressed first decompress it.  FILENAME must not
contain any directory parts.  If the repository tracks other
files then they are removed.  MESSAGE is used as commit message."
  (require 'mm-util)
  (require 'request)
  (request
   url
   :sync t
   :parser  #'buffer-string
   :success (apply-partially
             (cl-function
              (lambda (url filename message &key response data &allow-other-keys)
                (with-temp-buffer
                  (insert data)
                  (mm-decompress-buffer  (file-name-nondirectory url) t t)
                  (plunger-region-blob   (point-min) (point-max) filename)
                  (plunger-region-mktree (point-min) (point-max))
                  (plunger-region-commit (point-min) (point-max) message))))
             url filename message)
   :error   (cl-function
             (lambda (&key error-thrown &allow-other-keys)
               (signal (car error-thrown)
                       (cdr error-thrown)))))
  (unless (magit-get-boolean "core.bare")
    (magit-run-git "reset" "--hard" "HEAD")))

(defun plunger-pull-elisp-file (url filename message)
  (require 'elx)
  (add-hook
   'plunger-pre-commit-hook
   (lambda (tree)
     (let ((maintainer
            (with-temp-buffer
              (magit-git-insert
               (list "show"
                     (concat tree ":"
                             (car (magit-git-lines
                                   "ls-tree" "--name-only" tree)))))
              (car (elx-maintainers)))))
       (setenv "GIT_AUTHOR_NAME"  (or (car maintainer) "unknown"))
       (setenv "GIT_AUTHOR_EMAIL" (or (cdr maintainer) "unknown"))
       (setenv "GIT_COMMITTER_NAME"  user-full-name)
       (setenv "GIT_COMMITTER_EMAIL" user-mail-address)))
   nil t)
  (plunger-pull-file url filename message))

(provide 'plunger)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; plunger.el ends here
