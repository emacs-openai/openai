;;; openai-embedding.el --- Create embeddings with OpenAI API  -*- lexical-binding: t; -*-

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
;; Get a vector representation of a given input that can be easily consumed by
;; machine learning models and algorithms.
;;
;; See https://platform.openai.com/docs/api-reference/embeddings
;;

;;; Code:

(require 'openai)

(defcustom openai-embedding-model "text-embedding-ada-002"
  "ID of the model to use."
  :type 'string
  :group 'openai)

;;
;;; API

(defun openai-embedding-create (input callback)
  "Creates an embedding vector representing the input text.

INPUT text to get embeddings for, encoded as a string or array of tokens.
To get embeddings for multiple inputs in a single request, pass an array of
strings or array of token arrays. Each input must not exceed 8192 tokens in
length.

The argument CALLBACK is execuated after request is made."
  (openai-request "https://api.openai.com/v1/embeddings"
    :type "POST"
    :headers `(("Content-Type"  . "application/json")
               ("Authorization" . ,(concat "Bearer " openai-key)))
    :data (json-encode
           `(("model" . ,openai-embedding-model)
             ("input" . ,input)
             ("user"  . ,openai-user)))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback data)))))

(provide 'openai-embedding)
;;; openai-embedding.el ends here
