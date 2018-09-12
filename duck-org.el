;;; duck-org.el --- Org mode stuff for duck.el -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Org mode stuff for duck.el
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
(require 'duck-time)


(defun duck-org-timestring (parsed-time)
  "Return an org mode type timestring without side brackets."
  (let* ((grain (alist-get 'grain parsed-time))
         (value (alist-get 'value parsed-time))
         (prefix (format "%d-%02d-%02d"
                         (alist-get 'year value)
                         (alist-get 'month value)
                         (alist-get 'day value))))
    (if (member grain '(hour minute))
        (format "%s %02d:%02d" prefix
                (alist-get 'hour value)
                (alist-get 'minute value))
      prefix)))

(provide 'duck-org)

;;; duck-org.el ends here
