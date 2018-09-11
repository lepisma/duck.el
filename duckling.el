;;; duckling.el --- Minimal wrapper for duckling -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25"))
;; URL: https://github.com/lepisma/duckling.el

;;; Commentary:

;; Minimal wrapper for duckling
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

(defcustom duckling-default-lang "en"
  "Default language for duckling")

(defcustom duckling-default-tz "Asia/Kolkata"
  "Default timezone")

(defcustom duckling-cli-path nil
  "Path to cli binary")

(defvar duckling-process nil
  "The cli process")

(defvar duckling-process-output nil
  "Placeholder for process output")

(defun duckling-quit ()
  (process-send-string duckling-process ":quit\n")
  (setq duckling-process nil))

(defun duckling-start ()
  (setq duckling-process
        (make-process :name "duckling-cli"
                      :command (list duckling-cli-path)
                      :filter (lambda (proc str) (setq duckling-process-output str)))))

(defun duckling-parse (text &optional lang tz)
  (if (null duckling-process)
      (duckling-start))
  (process-send-string duckling-process
                       (format "%s\n"
                               (json-encode-alist `(("text" . ,text)
                                                    ("lang" . ,(or lang duckling-default-lang))
                                                    ("tz" . ,(or tz duckling-default-tz))))))
  (accept-process-output duckling-process)
  (json-parse-string duckling-process-output :object-type 'alist))

(provide 'duckling)

;;; duckling.el ends here
