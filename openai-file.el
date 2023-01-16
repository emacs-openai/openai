;;; openai-file.el --- Control files in OpenAI  -*- lexical-binding: t; -*-

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
;; Files are used to upload documents that can be used with features like
;; Fine-tuning.
;;
;; See https://beta.openai.com/docs/api-reference/files
;;

;;; Code:

(require 'openai)

;;
;;; API

(defun openai-file-list (callback)
  "Returns a list of files that belong to the user's organization.

The argument CALLBACK is execuated after request is made."
  (openai-request "https://api.openai.com/v1/files"
    :type "GET"
    :headers `(("Content-Type"  . "application/json")
               ("Authorization" . ,(concat "Bearer " openai-key)))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback data)))))

;;
;;; Application

(defvar openai-file-entries nil
  "Async files entries.")

(tblui-define
 openai-file
 (lambda () openai-model-entries)
 [("ID" 32 nil)
  ("Filename" 20 nil)
  ("Bytes" 6 nil)
  ("Object" 6 nil)
  ("Created at" 10 nil)
  ("Purpose" 40 nil)]
 nil)

;;;###autoload
(defun openai-list-files ()
  "List files that belong to the user's organization."
  (openai-file-list (lambda (data)
                      (let ((id 0))
                        (let-alist data
                          (mapc (lambda (file)
                                  (let-alist file
                                    (push (list (number-to-string id)
                                                (vector .id
                                                        .filename
                                                        .bytes
                                                        .object
                                                        .created_at
                                                        .purpose))
                                          openai-model-entries))
                                  (cl-incf id))
                                .data)))
                      (openai-file-goto-ui))))

(provide 'openai-file)
;;; openai-file.el ends here
