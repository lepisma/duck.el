;;; duck-time.el --- Time utilities for duck.el -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Time utilities for duck.el
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

(require 'cl-lib)
(require 'duck-core)
(require 'parse-time)

(defun duck-time-entity-p (entity)
  "Tell if ENTITY is of time type."
  (string= "time" (alist-get 'dim entity)))

(defun duck-time-parse-string (duckling-string)
  "Parse time from duckling string."
  (cl-flet ((subs (from to) (string-to-number (substring-no-properties duckling-string from to))))
    `((year . ,(subs 0 4))
      (month . ,(subs 5 7))
      (day . ,(subs 8 10))
      (hour . ,(subs 11 13))
      (minute . ,(subs 14 16))
      (second . ,(subs 17 19)))))

(defun duck-time-get (entity)
  "Return the best match time from the ENTITY."
  (let ((value (alist-get 'value entity)))
    `((range . ,(cons (alist-get 'start entity) (alist-get 'end entity)))
      (grain . ,(intern (alist-get 'grain value)))
      (value . ,(duck-time-parse-string (alist-get 'value value))))))

;;;###autoload
(defun duck-time-parse (&rest args)
  "Parse time from the given duckling arguments."
  (let ((res (apply #'duck-parse args)))
    (let ((entities (cl-remove-if-not #'duck-time-entity-p res)))
      (mapcar #'duck-time-get entities))))

(provide 'duck-time)

;;; duck-time.el ends here
