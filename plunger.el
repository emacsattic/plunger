;;; plunger.el --- create Git objects using plumbing commands

;; Copyright (C) 2012  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20120112
;; Version: 0.2.0
;; Status: beta
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

(require 'magit)

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
  (let ((process-environment (copy-list process-environment))
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

(defun plunger--get-buffer (name &optional noerase)
  (let ((buf (get-buffer-create name))
        (inhibit-read-only t))
    (with-current-buffer buf
      (unless noerase
        (erase-buffer))
      (setq buffer-read-only t))
    buf))

(provide 'plunger)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; plunger.el ends here
