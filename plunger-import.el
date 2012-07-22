;;; plunger-import.el --- create commit from file downloaded from web

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

;; This library provides function `plunger-import' which
;; creates a commit from a file downloaded from the web.

;; The plan was to also directly create commits from tarballs
;; and files whose url first have to be extracted from a webpage.
;; Unfortunatly I lost interested before that was implemented.

;;; Code:

(require 'magit)
(require 'mm-util)
(require 'plunger)
(require 'url-http)

(defun plunger-import (url &optional filename message)
  (let ((buffer (url-retrieve-synchronously url))
        response tree)
    (with-current-buffer buffer
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
      ;; that's the only importer that currently exists
      (setq tree (plunger-import-single url filename)))
    (kill-buffer buffer)
    (if (and message
             (or (magit-no-commit-p)
                 (= 1 (magit-git-exit-code "diff-tree" "--quiet" "HEAD" tree))))
        (plunger-commit tree message)
      ;; Indicate that this isn't a commit by returning a list.
      (list tree))))

(defun plunger-import-single (url filename)
  (unless filename
    (setq filename
	  (if (magit-no-commit-p)
	      (if (string-match "\\([^/]+\\)$" url)
		  (match-string 1 url)
		(error "Cannot determine local filename"))
	    (car (magit-git-lines "ls-tree" "--name-only" "HEAD")))))
  (let ((buf (plunger--get-buffer " *plunger-mktree*")))
    (plunger-region-blob (point-min) (point-max) filename nil buf)
    (with-current-buffer buf
      (plunger-region-mktree (point-min) (point-max)))))

(provide 'plunger-import)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; plunger-import.el ends here
