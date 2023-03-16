;;; openai-moderation.el --- Classifies if text violates OpenAI's Content Policy  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Shen, Jen-Chieh

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
;; Given a input text, outputs if the model classifies it as violating OpenAI's
;; content policy.
;;
;; See https://platform.openai.com/docs/api-reference/embeddings
;;

;;; Code:

(require 'openai)

;;
;;; API

(defun openai-moderation-create ( input callback
                                  &key
                                  (key openai-key)
                                  (model "text-moderation-latest"))
  "Classifies if text violates OpenAI's Content Policy.

Argument INPUT is the text to classify.

The argument CALLBACK is execuated after request is made."
  (openai-request "https://api.openai.com/v1/embeddings"
    :type "POST"
    :headers `(("Content-Type"  . "application/json")
               ("Authorization" . ,(concat "Bearer " key)))
    :data (json-encode
           `(("model" . ,model)
             ("input" . ,input)))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback data)))))

(provide 'openai-moderation)
;;; openai-moderation.el ends here
