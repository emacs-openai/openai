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

;;
;;; API

(cl-defun openai-embedding-create ( input callback
                                    &key
                                    (base-url openai-base-url)
                                    (parameters openai-parameters)
                                    (content-type "application/json")
                                    (key openai-key)
                                    org-id
                                    (model "text-embedding-ada-002")
                                    (user openai-user))
  "Create an embedding vector representing the input text.

INPUT text to get embeddings for, encoded as a string or array of tokens.  To
get embeddings for multiple inputs in a single request, pass an array of
strings or array of token arrays.  Each input must not exceed 8192 tokens in
length.

The argument CALLBACK is executed after the request is made.

Arguments BASE-URL, PARAMETERS, CONTENT-TYPE, KEY, ORG-ID and USER are global
options; however, you can overwrite the value by passing it in.

The rest of the arguments are optional, please see OpenAI API reference page
for more information.  Arguments here refer to MODEL."
  (openai-request (concat base-url "/embeddings")
    :type "POST"
    :params parameters
    :headers (openai--headers content-type key org-id)
    :data (openai--json-encode
           `(("model" . ,model)
             ("input" . ,input)
             ("user"  . ,user)))
    :parser 'json-read
    :complete (cl-function
               (lambda (&key data &allow-other-keys)
                 (funcall callback data)))))

(provide 'openai-embedding)
;;; openai-embedding.el ends here
