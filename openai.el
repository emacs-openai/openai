;;; openai.el --- Elisp library for the OpenAI API  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs090218/openai
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (request "0.3.0") (tblui "0.1.0"))
;; Keywords: comm openai

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Elisp library for the OpenAI API
;;

;;; Code:

(require 'cl-lib)
(require 'let-alist)
(require 'pp)

(require 'request)
(require 'tblui)

(defgroup openai nil
  "Elisp library for the OpenAI API."
  :prefix "openai-"
  :group 'comm
  :link '(url-link :tag "Repository" "https://github.com/jcs090218/openai"))

(defcustom openai-key ""
  "Generated API key."
  :type 'list
  :group 'openai)

(defcustom openai-user ""
  "A unique identifier representing your end-user, which can help OpenAI to
monitor and detect abuse."
  :type 'string
  :group 'openai)

(defmacro openai-request (url &rest body)
  "Wrapper for `request' function."
  (declare (indent 1))
  `(if (string-empty-p openai-key)
       (user-error "[INFO] Invalid API key, please set it to the correct value: %s" openai-key)
     (request ,url ,@body)))

(provide 'openai)
;;; openai.el ends here
