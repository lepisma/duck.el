;;; duck.el --- Minimal wrapper for duckling -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.2
;; Package-Requires: ((emacs "25"))
;; URL: https://github.com/lepisma/duck.el

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

(require 'duck-core)
(require 'duck-time)
(require 'duck-org)

(defcustom duck-default-lang "en"
  "Default language for duck")

(defcustom duck-default-tz "Asia/Kolkata"
  "Default timezone")

(defcustom duck-cli-path nil
  "Path to duckling cli binary")

(provide 'duck)

;;; duck.el ends here
