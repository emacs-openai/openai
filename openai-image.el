;;; openai-image.el --- Create image with OpenAI  -*- lexical-binding: t; -*-

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
;; Create image with OpenAI.
;;
;; See https://beta.openai.com/docs/api-reference/images/create
;;

;;; Code:

(require 'openai)

;;
;;; API

(cl-defun openai-image ( prompt callback
                         &key
                         (key openai-key)
                         n
                         size
                         response-format
                         (user openai-user))
  "Send create image request.

Argument CALLBACK is function with data pass in."
  (openai-request "https://api.openai.com/v1/images/generations"
    :type "POST"
    :headers `(("Content-Type"  . "application/json")
               ("Authorization" . ,(concat "Bearer " key)))
    :data (openai--json-encode
           `(("prompt"          . ,prompt)
             ("n"               . ,n)
             ("size"            . ,size)
             ("response_format" . ,response-format)
             ("user"            . ,user)))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback data)))))

(cl-defun openai-image-edit ( image prompt callback
                              &key
                              (key openai-key)
                              mask
                              n
                              size
                              response-format
                              (user openai-user))
  "Creates an edited or extended image given an original IMAGE and a PROMPT.

Argument CALLBACK is function with data pass in."
  (openai-request "https://api.openai.com/v1/images/edits"
    :type "POST"
    :headers `(("Authorization" . ,(concat "Bearer " key)))
    :data (openai--json-encode
           `(("image"           . ,image)
             ("prompt"          . ,prompt)
             ("mask"            . ,mask)
             ("n"               . ,n)
             ("size"            . ,size)
             ("response_format" . ,response-format)
             ("user"            . ,user)))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback data)))))

(cl-defun openai-image-variation ( image callback
                                   &key
                                   (key openai-key)
                                   mask
                                   n
                                   size
                                   response-format
                                   (user openai-user))
  "Creates a variation of a given IMAGE.

Argument CALLBACK is function with data pass in, and the argument IMAGE  must be
a valid PNG file, less than 4MB, and square.

If mask is not provided, image must have transparency, which will be used as
the mask."
  (openai-request "https://api.openai.com/v1/images/variations"
    :type "POST"
    :headers `(("Authorization" . ,(concat "Bearer " key)))
    :data (openai--json-encode
           `(("image"           . ,image)
             ("mask"            . ,mask)
             ("n"               . ,n)
             ("size"            . ,size)
             ("response_format" . ,response-format)
             ("user"            . ,user)))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback data)))))

;;
;;; Util

(defun openai--select-png-files (candidate)
  "Return t if CANDIDATE is either directory or a PNG file."
  (or (string-suffix-p ".png" candidate t)  ; only support png
      (file-directory-p candidate)))        ; allow navigation

;;
;;; Application

(defcustom openai-image-n 1
  "The number of images to generate.  Must be between 1 and 10."
  :type 'integer
  :group 'openai)

(defcustom openai-image-size "1024x1024"
  "The size of the generated images.

Must be one of `256x256', `512x512', or `1024x1024'."
  :type 'string
  :group 'openai)

(defcustom openai-image-response-format "url"
  "The format in which the generated images are returned.

Must be one of `url' or `b64_json'."
  :type 'string
  :group 'openai)

(defvar openai-image-entries nil
  "Async images entries.")

(tblui-define
 openai-image
 (lambda () openai-image-entries)
 [("URL" 200 nil)]
 nil)

;;;###autoload
(defun openai-image-prompt (prompt)
  "Use PROMPT to ask for image, and display result in a buffer."
  (interactive (list (read-string "Describe image: ")))
  (setq openai-image-entries nil)
  (openai-image prompt
                (lambda (data)
                  (let ((id 0))
                    (let-alist data
                      (mapc (lambda (images)
                              (dolist (image images)
                                (push (list (number-to-string id)
                                            (vector (cdr image)))
                                      openai-image-entries)
                                (cl-incf id)))
                            .data)))
                  (openai-image-goto-ui)
                  :size openai-image-size
                  :n openai-image-n
                  :response-format openai-image-response-format)))

;;;###autoload
(defun openai-image-edit-prompt (prompt)
  "Use PROMPT to ask for image, and display result in a buffer."
  (interactive (list (read-string "Describe image: ")))
  (setq openai-image-entries nil)
  (openai-image-edit prompt
                     (lambda (data)
                       (let ((id 0))
                         (let-alist data
                           (mapc (lambda (images)
                                   (dolist (image images)
                                     (push (list (number-to-string id)
                                                 (vector (cdr image)))
                                           openai-image-entries)
                                     (cl-incf id)))
                                 .data)))
                       (openai-image-goto-ui))
                     :size openai-image-size
                     :n openai-image-n
                     :response-format openai-image-response-format))

;;;###autoload
(defun openai-image-variation-prompt (image)
  "Prompt to select an IMAGE file, and display result in a buffer."
  (interactive (list (read-file-name "Select image file: " nil nil t nil
                                     #'openai--select-png-files)))
  (setq openai-image-entries nil)
  (openai-image-variation image
                          (lambda (data)
                            (let ((id 0))
                              (let-alist data
                                (mapc (lambda (images)
                                        (dolist (image images)
                                          (push (list (number-to-string id)
                                                      (vector (cdr image)))
                                                openai-image-entries)
                                          (cl-incf id)))
                                      .data)))
                            (openai-image-goto-ui))
                          :size openai-image-size
                          :n openai-image-n
                          :response-format openai-image-response-format))

(provide 'openai-image)
;;; openai-image.el ends here
