;;; openai-fine-tune.el --- Control fine-tunes with OpenAI API  -*- lexical-binding: t; -*-

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
;; Manage fine-tuning jobs to tailor a model to your specific training data.
;;
;; See https://platform.openai.com/docs/api-reference/fine-tunes
;;

;;; Code:

(require 'openai)

;;
;;; API

(cl-defun openai-fine-tune-create ( training-file callback
                                    &key
                                    (model "curie")
                                    validation-file
                                    n-epochs
                                    batch-size
                                    learning-rate-multiplier
                                    prompt-loss-weight
                                    compute-classification-metrics
                                    classification-n-classes
                                    classification-positive-class
                                    classification-betas
                                    suffix)
  "Creates a job that fine-tunes a specified model from a given dataset.

Response includes details of the enqueued job including job status and the name
of the fine-tuned models once complete.

The argument TRAINING-FILE is the ID of an uploaded file that contains training
data.

The argument CALLBACK is execuated after request is made.

Arguments KEY is global option; however, you can overwrite the value by passing
it in.

The rest of the arugments are optional, please see OpenAI API reference page
for more information.  Arguments here refer to MODEL, VALIDATION-FILE, N-EPOCHS,
BATCH-SIZE, LEARNING-RATE-MULTIPLIER, PROMPT-LOSS-WEIGHT,
COMPUTE-CLASSIFICATION-METRICS, CLASSIFICATION-N-CLASSES,
CLASSIFICATION-POSITIVE-CLASS, CLASSIFICATION-BETAS, and SUFFIX"
  (openai-request "https://api.openai.com/v1/fine-tunes"
    :type "POST"
    :data (openai--json-encode
           `(("model"                          . ,model)
             ("training_file"                  . ,training-file)
             ("validation_file"                . ,validation-file)
             ("n_epochs"                       . ,n-epochs)
             ("batch_size"                     . ,batch-size)
             ("learning_rate_multiplier"       . ,learning-rate-multiplier)
             ("prompt_loss_weight"             . ,prompt-loss-weight)
             ("compute_classification_metrics" . ,compute-classification-metrics)
             ("classification_n_classes"       . ,classification-n-classes)
             ("classification_positive_class"  . ,classification-positive-class)
             ("classification_betas"           . ,classification-betas)
             ("suffix"                         . ,suffix)))
    :complete (cl-function
               (lambda (&key data &allow-other-keys)
                 (funcall callback data)))))

(cl-defun openai-fine-tune-list (callback)
  "List your organization's fine-tuning jobs.

The argument CALLBACK is execuated after request is made."
  (openai-request "https://api.openai.com/v1/fine-tunes"
    :type "GET"
    :complete (cl-function
               (lambda (&key data &allow-other-keys)
                 (funcall callback data)))))

(cl-defun openai-fine-tune-retrieve (fine-tune-id callback)
  "Gets info about the fine-tune job.

The FINE-TUNE-ID of the fine-tune job.

The argument CALLBACK is execuated after request is made."
  (openai-request (format "https://api.openai.com/v1/fine-tunes/%s" fine-tune-id)
    :type "GET"
    :complete (cl-function
               (lambda (&key data &allow-other-keys)
                 (funcall callback data)))))

(cl-defun openai-fine-tune-cancel (fine-tune-id callback)
  "Immediately cancel a fine-tune job.

The FINE-TUNE-ID of the fine-tune job to cancel.

The argument CALLBACK is execuated after request is made."
  (openai-request (format "https://api.openai.com/v1/fine-tunes/%s/cancel" fine-tune-id)
    :type "POST"
    :complete (cl-function
               (lambda (&key data &allow-other-keys)
                 (funcall callback data)))))

(cl-defun openai-fine-tune-list-events (fine-tune-id callback)
  "Get fine-grained status updates for a fine-tune job.

The FINE-TUNE-ID of the fine-tune job to get events for.

The argument CALLBACK is execuated after request is made."
  (openai-request (format "https://api.openai.com/v1/fine-tunes/%s/events" fine-tune-id)
    :type "GET"
    :complete (cl-function
               (lambda (&key data &allow-other-keys)
                 (funcall callback data)))))

(cl-defun openai-fine-tune-delete (model callback)
  "Delete a fine-tuned model.  You must have the Owner role in your organization.

The MODEL to delete.

The argument CALLBACK is execuated after request is made."
  (openai-request (format "https://api.openai.com/v1/models/%s" model)
    :type "DELETE"
    :complete (cl-function
               (lambda (&key data &allow-other-keys)
                 (funcall callback data)))))

;;
;;; Application

(defvar openai-fine-tune-entries nil
  "Async files entries.")

(tblui-define
 openai-fine-tune
 (lambda () openai-fine-tune-entries)
 [("ID" 15 nil)
  ("Object" 30 nil)
  ("Model" 6 nil)
  ("Created at" 10 nil)
  ("fine_tuned_model" 10 nil)
  ("hyperparams" 40 nil)
  ("organization_id" 40 nil)
  ("status" 10 nil)
  ("validation_files" 10 nil)
  ("training_files" 10 nil)
  ("Updated at" 10 nil)]
 nil)

;;;###autoload
(defun openai-list-fine-tunes ()
  "List fine-tuning jobs."
  (interactive)
  (setq openai-fine-tune-entries nil)  ; reset
  (openai-fine-tune-list
   (lambda (data)
     (let ((id 0))
       (let-alist data
         (mapc (lambda (fine-tune)
                 (let-alist fine-tune
                   (push (list (number-to-string id)
                               (vector .id
                                       .object
                                       .model
                                       .created_at
                                       .fine_tuned_model
                                       .hyperparams
                                       .organization_id
                                       .result_files
                                       .status
                                       .validation_files
                                       .training_files
                                       .updated_at))
                         openai-fine-tune-entries))
                 (cl-incf id))
               .data)))
     (openai-fine-tune-goto-ui))))

(provide 'openai-fine-tune)
;;; openai-fine-tune.el ends here
