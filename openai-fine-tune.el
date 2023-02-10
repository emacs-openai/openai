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

(defcustom openai-fine-tune-model "curie"
  "The name of the base model to fine-tune.

You can select one of \"ada\", \"babbage\", \"curie\", \"davinci\", or a
fine-tuned model created after 2022-04-21.  To learn more about these models,
see the Models documentation."
  :type 'string
  :group 'openai)

(defcustom openai-fine-tune-validation-file ""
  "The ID of an uploaded file that contains validation data."
  :type 'string
  :group 'openai)

(defcustom openai-fine-tune-n-epochs 4
  "The number of epochs to train the model for.

An epoch refers to one full cycle through the training dataset."
  :type 'integer
  :group 'openai)

(defcustom openai-fine-tune-batch-size nil
  "The batch size to use for training.

The batch size is the number of training examples used to train a single forward
and backward pass.

By default, the batch size will be dynamically configured to be ~0.2% of the
number of examples in the training set, capped at 256 - in general, we've found
that larger batch sizes tend to work better for larger datasets."
  :type 'integer
  :group 'openai)

(defcustom openai-fine-tune-learning-rate-multiplier nil
  "The learning rate multiplier to use for training.

The fine-tuning learning rate is the original learning rate used for pretraining
multiplied by this value.

By default, the learning rate multiplier is the 0.05, 0.1, or 0.2 depending on
final `batch_size' (larger learning rates tend to perform better with larger
batch sizes). We recommend experimenting with values in the range 0.02 to 0.2 to
see what produces the best results."
  :type 'integer
  :group 'openai)

(defcustom openai-fine-tune-prompt-loss-weight 0.01
  "The weight to use for loss on the prompt tokens. This controls how much the
model tries to learn to generate the prompt (as compared to the completion which
always has a weight of 1.0), and can add a stabilizing effect to training when
completions are short.

If prompts are extremely long (relative to completions), it may make sense to
reduce this weight so as to avoid over-prioritizing learning the prompt."
  :type 'number
  :group 'openai)

(defcustom openai-fine-tune-compute-classification-metrics 0.01
  "If set, we calculate classification-specific metrics such as accuracy and F-1
score using the validation set at the end of every epoch.

In order to compute classification metrics, you must provide a `validation_file'.
Additionally, you must specify `classification_n_classes' for multiclass
classification or `classification_positive_class' for binary classification."
  :type 'boolean
  :group 'openai)

(defcustom openai-fine-tune-classification-n-classes nil
  "The number of classes in a classification task.

This parameter is required for multiclass classification."
  :type 'integer
  :group 'openai)

(defcustom openai-fine-tune-classification-positive-class nil
  "The positive class in binary classification.

This parameter is needed to generate precision, recall, and F1 metrics when
doing binary classification."
  :type 'string
  :group 'openai)

(defcustom openai-fine-tune-classification-betas nil
  "If this is provided, we calculate F-beta scores at the specified beta values.

The F-beta score is a generalization of F-1 score.  This is only used for binary
classification.

With a beta of 1 (i.e. the F-1 score), precision and recall are given the same
weight.  A larger beta score puts more weight on recall and less on precision.
A smaller beta score puts more weight on precision and less on recall."
  :type 'list
  :group 'openai)

(defcustom openai-fine-tune-suffix nil
  "A string of up to 40 characters that will be added to your fine-tuned model
name.

For example, a suffix of \"custom-model-name\" would produce a model name like
`ada:ft-your-org:custom-model-name-2022-02-15-04-21-04'."
  :type 'string
  :group 'openai)

;;
;;; API

(defun openai-fine-tune-create (training-file callback)
  "Creates a job that fine-tunes a specified model from a given dataset.

Response includes details of the enqueued job including job status and the name
of the fine-tuned models once complete.

The argument TRAINING-FILE is the ID of an uploaded file that contains training
data.

The argument CALLBACK is execuated after request is made."
  (openai-request "https://api.openai.com/v1/fine-tunes"
    :type "POST"
    :headers `(("Content-Type"  . "application/json")
               ("Authorization" . ,(concat "Bearer " openai-key)))
    :data (json-encode
           `(("model"                          . ,openai-fine-tune-model)
             ("training_file"                  . ,training-file)
             ("validation_file"                . ,openai-fine-tune-validation-file)
             ("n_epochs"                       . ,openai-fine-tune-n-epochs)
             ("batch_size"                     . ,openai-fine-tune-batch-size)
             ("learning_rate_multiplier"       . ,openai-fine-tune-learning-rate-multiplier)
             ("prompt_loss_weight"             . ,openai-fine-tune-prompt-loss-weight)
             ("compute_classification_metrics" . ,openai-fine-tune-compute-classification-metrics)
             ("classification_n_classes"       . ,openai-fine-tune-classification-n-classes)
             ("classification_positive_class"  . ,openai-fine-tune-classification-positive-class)
             ("classification_betas"           . ,openai-fine-tune-classification-betas)
             ("suffix"                         . ,openai-fine-tune-suffix)))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback data)))))

(defun openai-fine-tune-list (callback)
  "List your organization's fine-tuning jobs.

The argument CALLBACK is execuated after request is made."
  (openai-request "https://api.openai.com/v1/fine-tunes"
    :type "GET"
    :headers `(("Content-Type"  . "application/json")
               ("Authorization" . ,(concat "Bearer " openai-key)))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback data)))))

(defun openai-fine-tune-retrieve (fine-tune-id callback)
  "Gets info about the fine-tune job.

The FINE-TUNE-ID of the fine-tune job.

The argument CALLBACK is execuated after request is made."
  (openai-request (format "https://api.openai.com/v1/fine-tunes/%s" fine-tune-id)
    :type "GET"
    :headers `(("Content-Type"  . "application/json")
               ("Authorization" . ,(concat "Bearer " openai-key)))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback data)))))

(defun openai-fine-tune-cancel (fine-tune-id callback)
  "Immediately cancel a fine-tune job.

The FINE-TUNE-ID of the fine-tune job to cancel.

The argument CALLBACK is execuated after request is made."
  (openai-request (format "https://api.openai.com/v1/fine-tunes/%s/cancel" fine-tune-id)
    :type "POST"
    :headers `(("Content-Type"  . "application/json")
               ("Authorization" . ,(concat "Bearer " openai-key)))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback data)))))

(defun openai-fine-tune-list-events (fine-tune-id callback)
  "Get fine-grained status updates for a fine-tune job.

The FINE-TUNE-ID of the fine-tune job to get events for.

The argument CALLBACK is execuated after request is made."
  (openai-request (format "https://api.openai.com/v1/fine-tunes/%s/events" fine-tune-id)
    :type "GET"
    :headers `(("Content-Type"  . "application/json")
               ("Authorization" . ,(concat "Bearer " openai-key)))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback data)))))

(defun openai-fine-tune-list-events (model callback)
  "Delete a fine-tuned model.  You must have the Owner role in your organization.

The MODEL to delete.

The argument CALLBACK is execuated after request is made."
  (openai-request (format "https://api.openai.com/v1/models/%s" model)
    :type "DELETE"
    :headers `(("Content-Type"  . "application/json")
               ("Authorization" . ,(concat "Bearer " openai-key)))
    :parser 'json-read
    :success (cl-function
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
  (openai-file-list (lambda (data)
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
