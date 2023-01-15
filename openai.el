;;; openai.el --- Elisp library for the OpenAI API  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs090218/openai
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (request "0.3.0"))
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

(require 'request)

(defgroup openai nil
  "Elisp library for the OpenAI API."
  :prefix "openai-"
  :group 'comm
  :link '(url-link :tag "Repository" "https://github.com/jcs090218/openai"))

(defcustom openai-key ""
  "Generated API key."
  :type 'list
  :group 'openai)

(defcustom openai-model "text-davinci-003"
  "Target trained model server's name."
  :type 'string
  :group 'openai)

(defcustom openai-max-tokens 4000
  "The maximum number of tokens to generate in the completion.

The token count of your prompt plus max_tokens cannot exceed the model's context
length.  Most models have a context length of 2048 tokens (except for the newest
models, which support 4096)."
  :type 'integer
  :group 'openai)

(defcustom openai-temperature 1.0
  "What sampling temperature to use.

Higher values means the model will take more risks.  Try 0.9 for more creative
applications, and 0 (argmax sampling) for ones with a well-defined answer."
  :type 'number
  :group 'openai)

(defmacro openai--with-buffer (buffer-or-name &rest body)
  "Execute BODY within the ChatGPT buffer."
  (declare (indent 1))
  `(with-current-buffer (get-buffer-create buffer-or-name)
     (setq-local buffer-read-only t)
     (let ((inhibit-read-only))
       ,@body)))

(defun chatgpt--pop-to-buffer (buffer-or-name)
  "Show ChatGPT display buffer."
  (pop-to-buffer (get-buffer-create buffer-or-name)
                 `((display-buffer-in-direction)
                   (dedicated . t))))

(defmacro openai-request (url &rest body)
  "Wrapper for `request' function."
  (declare (indent 1))
  `(if (string-empty-p openai-key)
       (user-error "[INFO] Invalid API key, please set it to the correct value: %s" openai-key)
     (request ,url ,@body)))

(defun openai-models (callback)
  "Return models data and execute the CALLBACK."
  (openai-request "https://api.openai.com/v1/models"
    :type "GET"
    :headers `(("Content-Type"  . "application/json")
               ("Authorization" . ,(concat "Bearer " openai-key)))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback data)))))

;;;###autoload
(defun openai-complete (query callback)
  "Query ChatGPT with QUERY.

Argument CALLBACK is a function received one argument which is the JSON data."
  (request "https://api.openai.com/v1/completions"
    :type "POST"
    :headers `(("Content-Type"  . "application/json")
               ("Authorization" . ,(concat "Bearer " openai-key)))
    :data (json-encode
           `(("model"       . ,openai-model)
             ("prompt"      . ,query)
             ("max_tokens"  . ,openai-max-tokens)
             ("temperature" . ,openai-temperature)))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback data)))))

;;;###autoload
(defun openai-list-models ()
  "Lists the currently available models, and provides basic information about
each one such as the owner and availability."
  (interactive)
  (openai-models (lambda (data)
                   (let-alist data
                     (mapc (lambda (model)
                             (message "%s" model))
                           .data)))))

;;;###autoload
(defun openai-retrieve-model (model)
  ""
  (interactive ))

(provide 'openai)
;;; openai.el ends here
