;;; duck-core.el --- Core function for talking to duckling -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Core function for talking to duckling
;; This file is not a part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'json)

(defvar duck-default-lang)
(defvar duck-default-tz)
(defvar duck-cli-path)

(defvar duck-process nil
  "The cli process")

(defvar duck-process-output nil
  "Placeholder for process output")

(defun duck-quit ()
  (process-send-string duck-process ":quit\n")
  (setq duck-process nil))

(defun duck-start ()
  (setq duck-process
        (make-process :name "duck-cli"
                      :command (list duck-cli-path)
                      :filter (lambda (proc str) (setq duck-process-output str)))))

(defun duck-parse (text &optional lang tz)
  (if (null duck-process)
      (duck-start))
  (process-send-string duck-process
                       (format "%s\n"
                               (json-encode-alist `(("text" . ,text)
                                                    ("lang" . ,(or lang duck-default-lang))
                                                    ("tz" . ,(or tz duck-default-tz))))))
  (accept-process-output duck-process)
  (json-parse-string duck-process-output :object-type 'alist))

(provide 'duck-core)

;;; duck-core.el ends here
