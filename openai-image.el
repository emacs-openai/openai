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

(defcustom openai-image-mask nil
  "An additional image whose fully transparent areas (e.g. where alpha is zero)
indicate where image should be edited.

Must be a valid PNG file, less than 4MB, and have the same dimensions as image."
  :type 'string
  :group 'openai)

;;
;;; API

(defun openai-image (query callback)
  "Create image with QUERY.

Argument CALLBACK is function with data pass in."
  (openai-request "https://api.openai.com/v1/images/generations"
    :type "POST"
    :headers `(("Content-Type"  . "application/json")
               ("Authorization" . ,(concat "Bearer " openai-key)))
    :data (json-encode
           `(("prompt"          . ,query)
             ("n"               . ,openai-image-n)
             ("size"            . ,openai-image-size)
             ("response_format" . ,openai-image-response-format)
             ("user"            . ,openai-user)))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback data)))))

(defun openai-image-edit (query callback)
  "Create an edited or extended image given an original image and a QUERY.

Argument CALLBACK is function with data pass in."
  (openai-request "https://api.openai.com/v1/images/edits"
    :type "POST"
    :headers `(("Content-Type"  . "application/json")
               ("Authorization" . ,(concat "Bearer " openai-key)))
    :data (json-encode
           `(("prompt"          . ,query)
             ("mask"            . ,openai-image-mask)
             ("n"               . ,openai-image-n)
             ("size"            . ,openai-image-size)
             ("response_format" . ,openai-image-response-format)
             ("user"            . ,openai-user)))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback data)))))

(defun openai-image-variation (image callback)
  "Create an edited or extended image given an original IMAGE.

Argument CALLBACK is function with data pass in, and the argument IMAGE  must be
a valid PNG file, less than 4MB, and square.

If mask is not provided, image must have transparency, which will be used as
the mask."
  (openai-request "https://api.openai.com/v1/images/variations"
    :type "POST"
    :headers `(("Content-Type"  . "application/json")
               ("Authorization" . ,(concat "Bearer " openai-key)))
    :data (json-encode
           `(("image"           . ,image)
             ("mask"            . ,openai-image-mask)
             ("n"               . ,openai-image-n)
             ("size"            . ,openai-image-size)
             ("response_format" . ,openai-image-response-format)
             ("user"            . ,openai-user)))
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

(defvar openai-image-entries nil
  "Async images entries.")

(tblui-define
 openai-image
 (lambda () openai-image-entries)
 [("URL" 200 nil)]
 nil)

;;;###autoload
(defun openai-image-prompt (query)
  "Prompt to ask for image QUERY, and display result in a buffer."
  (interactive (list (read-string "Describe image: ")))
  (setq openai-image-entries nil)
  (openai-image query
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
                  (openai-image-goto-ui))))

;;;###autoload
(defun openai-image-edit-prompt (query)
  "Prompt to ask for image QUERY, and display result in a buffer."
  (interactive (list (read-string "Describe image: ")))
  (setq openai-image-entries nil)
  (openai-image-edit query
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
                       (openai-image-goto-ui))))

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
                            (openai-image-goto-ui))))

(provide 'openai-image)
;;; openai-image.el ends here
