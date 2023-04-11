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

(cl-defun openai-moderation-create ( input callback
                                     &key
                                     (base-url openai-base-url)
                                     (parameters openai-parameters)
                                     (content-type "application/json")
                                     (key openai-key)
                                     org-id
                                     (model "text-moderation-latest"))
  "Classifies if text violates OpenAI's Content Policy.

Argument INPUT is the text to classify.

The argument CALLBACK is executed after request is made.

Arguments BASE-URL, PARAMETERS, CONTENT-TYPE, KEY and ORG-ID are global
options; however, you can overwrite the value by passing it in.

The rest of the arguments are optional, please see OpenAI API reference page
for more information.  Arguments here refer to MODEL."
  (openai-request (concat base-url "/embeddings")
    :type "POST"
    :params parameters
    :headers (openai--headers content-type key org-id)
    :data (openai--json-encode
           `(("model" . ,model)
             ("input" . ,input)))
    :parser 'json-read
    :complete (cl-function
               (lambda (&key data &allow-other-keys)
                 (funcall callback data)))))

(provide 'openai-moderation)
;;; openai-moderation.el ends here
