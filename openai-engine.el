;;; openai-engine.el --- Control engines with OpenAI API  -*- lexical-binding: t; -*-

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
;; [DEPRECATED]
;;
;; These endpoints describe and provide access to the various engines available
;; in the API.
;;
;; See https://platform.openai.com/docs/api-reference/engines
;;

;;; Code:

(require 'openai)

;;
;;; API

(defun openai-engine-list (callback)
  "Lists the currently available (non-finetuned) models, and provides basic
information about each one such as the owner and availability.

The argument CALLBACK is execuated after request is made."
  (openai-request "https://api.openai.com/v1/engines"
    :type "GET"
    :headers `(("Content-Type"  . "application/json")
               ("Authorization" . ,(concat "Bearer " openai-key)))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback data)))))

(defun openai-engine-retrieve (engine-id callback)
  "Retrieves a model instance, providing basic information about it such as the
owner and availability.

The argument ENGINE-ID is the engine to use for this request.

The argument CALLBACK is execuated after request is made."
  (openai-request (format "https://api.openai.com/v1/engines/%s" engine-id)
    :type "GET"
    :headers `(("Content-Type"  . "application/json")
               ("Authorization" . ,(concat "Bearer " openai-key)))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback data)))))

;;
;;; Application

(defvar openai-engine-entries nil
  "Async files entries.")

(tblui-define
 openai-engine
 (lambda () openai-engine-entries)
 [("ID" 30 nil)
  ("Object" 8 nil)
  ("Owner" 20 nil)
  ("ready" 8 nil)]
 nil)

;;;###autoload
(defun openai-list-engines ()
  "List currently available (non-finetuned) models."
  (interactive)
  (setq openai-engine-entries nil)  ; reset
  (openai-engine-list (lambda (data)
                        (let ((id 0))
                          (let-alist data
                            (mapc (lambda (engine)
                                    (let-alist engine
                                      (push (list (number-to-string id)
                                                  (vector .id
                                                          .object
                                                          .owner
                                                          (if .ready "true" "false")))
                                            openai-engine-entries))
                                    (cl-incf id))
                                  .data)))
                        (openai-engine-goto-ui))))

(provide 'openai-engine)
;;; openai-engine.el ends here
