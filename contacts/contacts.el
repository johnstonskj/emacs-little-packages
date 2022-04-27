;;; contacts.el -*- lexical-binding: t; -*-

(require 'cl-lib)

;; --------------------------------------------------------------------------
;; Initialize customization
;; --------------------------------------------------------------------------

(defgroup contacts nil
  "Simon's personal environment settings."
  :tag "skj"
  :prefix "skj-")

(defcustom
  contacts-default-country
  t
  "Emit tracing messages during initialization, useful with --debug-init."
  :tag "Trace initialization process"
  :group 'contacts
  :type 'string)

;; --------------------------------------------------------------------------
;; --------------------------------------------------------------------------

(cl-defstruct (phone (:constructor phone--create) (:copier nil))
  number country-code)

(cl-defun phone-create (number &optional country-code)
  (apply #'phone--create number (if country-code country-code (country-code-default))))

(defun phone-print--default (number country-code)
  (format "+%s%s" country-code number))


;; --------------------------------------------------------------------------
;; --------------------------------------------------------------------------

(cl-defstruct (country (:constructor country--create) (:copier nil))
  name phone-prefix phone-validation phone-print address-validation address-print)

(defun country-create (name phone-prefix phone-validation phone-print address-validation address-print)
  (apply #' country--create :name name
            :phone-prefix phone-prefix
            :phone-validation phone-validation
            :phone-print phone-print
            :address-validation address-validation
            :address-print address-print))

(defconst
  country-info-alist
  (list
   (cons "us" (country-create :name "United States"
                              :phone-prefix 1
                              :phone-validation nil
                              :phone-print 'phone-print--default
                              :address-validation nil
                              :address-print nil))))

(defun country-code-default () "us")

(message "%s"
         (country-create "United States"
                         1
                         nil
                         'phone-print--default
                         nil
                         nil))

;; --------------------------------------------------------------------------
;; --------------------------------------------------------------------------

