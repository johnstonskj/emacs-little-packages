;;; smithy-mode.el --- major mode for Smithy IDL  -*- lexical-binding: t; -*-

;; Author: simon Johnston <johnstonskj@gmail.com>
;; Keywords: 
;; Version: 0.0.1

;;; --------------------------------------------------------------------------
;;; License:

;; Copyright (c) 2022 Simon Johnston

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; --------------------------------------------------------------------------
;;; Commentary:

;; TBD

;;; --------------------------------------------------------------------------
;;; Code:

(eval-when-compile
  (require 'rx))


;; --------------------------------------------------------------------------
;; Constants

(defconst smithy--kwds-constants
  '("true" "false" "null")
  "Smithy constant keywords.")

(defconst smithy--kwds-defs
  '("namespace" "metadata")
  "Smithy definition keywords.")

(defconst smithy--kwds-simple-types
  '("blob" "boolean" "string" "byte" "short" "integer" "long" "float"
    "double" "bigInteger" "bigDecimal" "timestamp" "document")
  "Smithy simple types.")

(defconst smithy--kwds-aggregate-types
  '("list" "set" "map" "structure" "union")
  "Smithy aggregate types.")

(defconst smithy--kwds-aggregate-members
  '("member" "key" "value")
  "Smithy aggregate type members.")

(defconst smithy--kwds-service-types
  '("service" "operation" "resource")
  "Smithy service types.")

(defconst smithy--kwds-service-members
  '("version" "operations" "resources" "errors" "renames"
    "input" "output"
    "identifiers" "create" "put" "read" "update" "delete" "list" "collectionOperations"
    "resources")
  "Smithy service type members.")

;; --------------------------------------------------------------------------
;; Syntax tables

(defvar smithy-mode-syntax-table
  (let ((table (make-syntax-table)))

    ;; paired delimiters
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)

    ;; - word constituent characters
    (modify-syntax-entry ?_ "w" st)

    table))

;; --------------------------------------------------------------------------
;; Font lock / syntax highlighting

(rx-define smithy-identifier
  (seq
       (or alphabetic ?_)
       (+ (or alphanumeric ?_))))
(message "%s" (rx smithy-identifier))

(rx-define smithy-namespace
  (seq
       smithy-identifier
       (* (seq ?. smithy-identifier))))
(message "%s" (rx smithy-namespace))

(rx-define smithy-root-shape-id
  (seq
       (optional (seq smithy-namespace ?#))
       smithy-identifier))
(message "%s" (rx smithy-root-shape-id))

(rx-define smithy-shape-id
  (seq
       smithy-root-shape-id
       (optional ?$ smithy-identifier)))
(message "%s" (rx smithy-shape-id))

(defun list->font-lock-list (words face &optional delimiter)
  (list
   (if delimiter
       (rx-to-string
        `(seq word-start (| ,@words) word-end (* (syntax whitespace)) ,delimiter))
     (rx-to-string
        `(seq word-start (| ,@words) word-end)))
   0 face))

(defconst smithy--font-lock-groups
  (list
   (list->font-lock-list
    smithy--kwds-constants
    font-lock-constant-face)
   (list->font-lock-list
    smithy--kwds-defs
    font-lock-keyword-face)
   (list->font-lock-list
    (append smithy--kwds-simple-types smithy--kwds-aggregate-types smithy--kwds-service-types)
    font-lock-type-face)
   (list->font-lock-list
    (append smithy--kwds-aggregate-members smithy--kwds-service-members)
    font-lock-builtin-face
    ?:)
   (list (rx (seq ?@ smithy-root-shape-id word-end)) 0 font-lock-function-name-face)
   )
  "All Smithy font lock keywords.")

(defconst smithy-font-lock-defaults
  (list
   smithy--font-lock-groups
   nil ; keywords-only
   nil ; case-fold
   nil))
  
;; --------------------------------------------------------------------------
;; Abreviation table

(defvar smithy-mode-abbrev-table nil
  "Abbreviation table used in `smithy-mode' buffers.")

(define-abbrev-table 'smithy-mode-abbrev-table
  '())

;;;###autoload
(define-derived-mode
  smithy-mode ; variant
  prog-mode ; parent
  "Smithy" ; name
  "Major mode for Smithy IDL files."
  :abbrev-table smithy-mode-abbrev-table
  :syntax-table smithy-mode-syntax-table
  (setq font-lock-defaults smithy-font-lock-defaults)
  (setq-local comment-start "//")
  (setq-local comment-end "")
;;   (setq-local comment-start-skip "#+[\t ]*")
;;   (setq-local indent-line-function #'smithy-indent-line)
  (setq-local indent-tabs-mode t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.smithy" . smithy-mode))


(provide 'smithy-mode)

;;; smithy-mode.el ends here
