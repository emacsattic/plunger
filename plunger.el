;;; plunger.el --- create Git objects using plumbing commands

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

;; This package provides some functions wrapping Git plumbing
;; commands for creating blobs, trees and commits.

;;; Code:

(require 'cl-lib)
(require 'magit)

(declare-function url-http-parse-response 'url-http)
(declare-function mm-decompress-buffer 'mm-util)

(defun plunger-region-blob (from to &optional filename permissions destination)
  (let ((buf (plunger--get-buffer " *plunger-blob*"))
        (inhibit-read-only t)
        hash)
    (unless (= 0 (apply 'call-process-region
                        from to magit-git-executable
                        nil (list buf nil) nil
                        (append magit-git-standard-options
                                (list "hash-object" "--stdin")
                                (when destination
                                  (list "-w"))
                                (when filename
                                  (list "--path" filename)))))
      (error "Cannot hash object"))
    (with-current-buffer buf
      (setq hash (magit-trim-line (buffer-string))))
    (kill-buffer buf)
    (when (and filename destination)
      (with-current-buffer destination
        (goto-char (point-max))
        (let ((standard-output (current-buffer))
              (inhibit-read-only t))
          (princ (format "%s blob %s\t%s\n"
                         (or permissions "100644")
                         hash filename)))))
    hash))

(defun plunger-region-mktree (from to)
  (let ((buf (plunger--get-buffer " *plunger-tree*")))
    (unless (= 0 (call-process-region from to magit-git-executable
                                      nil (list buf nil) nil
                                      "mktree"))
      (error "Cannot make tree"))
    (prog1 (with-current-buffer buf
             (magit-trim-line (buffer-string)))
      (kill-buffer buf)
      (kill-buffer (current-buffer)))))

(defvar plunger-pre-commit-hook nil)

(defun plunger-commit (tree message)
  (let ((process-environment (cl-copy-list process-environment))
        commit)
    (run-hook-with-args 'plunger-pre-commit-hook tree)
    (setq commit
          (apply 'magit-git-string "commit-tree" tree
                 "-m" message
                 (unless (magit-no-commit-p)
                   (list "-p" (magit-git-string "rev-parse" "HEAD")))))
    (if (not commit)
        (error "Cannot commit tree")
      (magit-run-git "update-ref" (magit-get-ref "HEAD") commit)
      (magit-git-string "rev-parse" "HEAD"))))

(defun plunger--get-buffer (name)
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer))
      (setq buffer-read-only t))
    buf))

(defun plunger-pull-file (url &optional filename message)
  (require 'mm-util)
  (require 'url-http)
  (let ((http-buf (url-retrieve-synchronously url))
        (tree-buf (plunger--get-buffer " *plunger-mktree*"))
        response tree)
    (with-current-buffer http-buf
      (setq response (url-http-parse-response))
      (when (or (<  response 200)
                (>= response 300))
        (error "Error during download request: %s"
               (buffer-substring-no-properties
                (point) (progn (end-of-line) (point)))))
      (re-search-forward "^$" nil 'move)
      (forward-char)
      (delete-region (point-min) (point))
      (mm-decompress-buffer (file-name-nondirectory url) t t)
      (unless filename
        (setq filename
              (if (magit-no-commit-p)
                  (if (string-match "\\([^/]+\\)$" url)
                      (match-string 1 url)
                    (error "Cannot determine local filename"))
                (car (magit-git-lines "ls-tree" "--name-only" "HEAD")))))
      (plunger-region-blob (point-min) (point-max) filename nil tree-buf))
    (kill-buffer http-buf)
    (with-current-buffer tree-buf
      (plunger-region-mktree (point-min) (point-max)))
    (kill-buffer tree-buf)
    (if (and message
             (or (magit-no-commit-p)
                 (= 1 (magit-git-exit-code "diff-tree" "--quiet" "HEAD" tree))))
        (plunger-commit tree message)
      ;; Indicate that this isn't a commit by returning a list.
      (list tree))))

(provide 'plunger)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; plunger.el ends here
