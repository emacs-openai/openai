;;; chatgpt.el --- API for interacting with ChatGPT  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs090218/ChatGPT.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: comm gpt

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
;; API for interacting with ChatGPT
;;

;;; Code:

(require 'request)

(defgroup chatgpt nil
  "Fuzzy matching for `company-mode'."
  :prefix "chatgpt-"
  :group 'comm
  :link '(url-link :tag "Repository" "https://github.com/jcs090218/ChatGPT.el"))

(defcustom chatgpt-key ""
  "Generated API key."
  :type 'list
  :group 'chatgpt)

;;;###autoload
(defun chatgpt-send (query)
  ""
  (request "https://api.openai.com/v1/completions"
    :type "POST"
    :headers `(("Content-Type"  . "application/json")
               ("Authorization" . ,(concat "Bearer " chatgpt-key)))
    :data (json-encode
           `(("model"      . "text-davinci-003")
             ("prompt"     . ,query)
             ("max_tokens" . 4000)
             ("temperature" . 1.0)))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                ))))

(provide 'chatgpt)
;;; chatgpt.el ends here
