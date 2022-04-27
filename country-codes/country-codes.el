;;; country-codes.el --- Support for ISO-3166 country data -*- lexical-binding: t; -*-

;; Author: simon Johnston <johnstonskj@gmail.com>
;; Keywords: 
;; Version: 0.0.1

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

;;; Commentary:

;; TBD

;;; Code:

(require 'cl-lib)
(require 'char-categories)

(defconst
  country-code-iso-version
  "2020"
  "The version of the ISO specification used to build the data tables in this
package.")

;; --------------------------------------------------------------------------
;; The Country Code Type Predicates
;; --------------------------------------------------------------------------

(defun country-code-alpha-2-p (cc)
  (and (stringp cc) (= (length cc) 2) (string-letter-uppercase-p cc)))

(defun country-code-alpha-3-p (cc)
  (and (stringp cc) (= (length cc) 3) (string-letter-uppercase-p cc)))

(defun country-code-numeric-p (cc)
  (and (integerp cc) (<= 1 cc 999)))

(defconst country-codes--status
  '(officially-assigned
    formerly-used
    user-assigned
    exceptionally-reserved
    indeterminately-reserved
    transitionally-reserved
    deleted
    unassigned))

(defun country-code-status-p (v)
  (and (symbolp v)       
       (not (null (member v country-codes--status)))))

(defconst country-codes-independent '(yes no))

(defun language-code-alpha-2-p (cc)
  (and (stringp cc) (= (length cc) 2) (string-letter-lowercase-p cc)))

(defun language-tagged-string-p (l)
  (and (listp l)
       (language-code-alpha-2-p (car l))
       (let ((value (cdr l)))
         (or (stringp value)
             (and (listp value)
                  (> (length value) 0)
                  (seq-every-p #'stringp value))))))

(defun language-tagged-list-p (ll)
  (and (listp ll)
       (seq-every-p #'language-code-alpha-2-p ll)))

;; --------------------------------------------------------------------------
;; Country Data (ISO-3166-1)
;; --------------------------------------------------------------------------

(defconst country-names--alist
  '(("AG" . (("en" . "Antigua and Barbuda")
             ("fr" . "Antigua-et-Barbuda")))))

;; --------------------------------------------------------------------------

(defconst country-languages--alist
  '(("AG" . (("en" . t)
             ("fr" . nil)))))

;; --------------------------------------------------------------------------

(defconst country-territories--alist
  '(("AG" . (1019))))

(defconst country-territory-names--alist
  '((1019 . (("en" . "Redonda Island")
             ("fr" . "l'Île Redonda")))))

;; --------------------------------------------------------------------------

(defconst country-subdivision-categories--alist
  '(("AG" . (298 429))))

(defconst country-subdivision-category-names--alist
  '((298 . (("en" . ("dependency" "dependencies"))
            ("fr" . ("dépendance"))))
    (429 . (("en" . ("parish" "parishes"))
            ("fr" . ("paroisse"))))))

;; --------------------------------------------------------------------------

(cl-defstruct (country-subdivision
               (:constructor country-subdivision--create)
               (:copier nil))
  alpha-code category parent)

(defconst country-subdivisions--alist
  `(("AG" . (,(country-subdivision--create :alpha-code "AG-10" :category 298 :parent nil)
             ,(country-subdivision--create :alpha-code "AG-11" :category 298 :parent nil)
             ,(country-subdivision--create :alpha-code "AG-03" :category 429 :parent nil)
             ,(country-subdivision--create :alpha-code "AG-04" :category 429 :parent nil)
             ,(country-subdivision--create :alpha-code "AG-05" :category 429 :parent nil)
             ,(country-subdivision--create :alpha-code "AG-06" :category 429 :parent nil)
             ,(country-subdivision--create :alpha-code "AG-07" :category 429 :parent nil)
             ,(country-subdivision--create :alpha-code "AG-08" :category 429 :parent nil)))))

(defconst country-subdivision-names--alist
  '(("AG-10" . ("en" . "Barbuda"))
    ("AG-11" . ("en" . "Redonda"))
    ("AG-03" . ("en" . "Saint George"))
    ("AG-04" . ("en" . "Saint John"))
    ("AG-05" . ("en" . "Saint Mary"))
    ("AG-06" . ("en" . "Saint Paul"))
    ("AG-07" . ("en" . "Saint Peter"))
    ("AG-08" . ("en" . "Saint Philip"))))

;; --------------------------------------------------------------------------

(cl-defstruct (country-code
               (:constructor country-code--create)
               (:copier nil))
  alpha-2 alpha-3 numeric independent? status short-name)

(defconst country-code-records--alist
  `(("AG" . ,(country-code--create
              :alpha-2 "AG"
              :alpha-3 "ATG"
              :numeric 028
              :independent? 'yes
              :status 'officially-assigned
              :short-name "Antigua and Barbuda"))))

;; --------------------------------------------------------------------------
;; Accessor functions
;; --------------------------------------------------------------------------

(defun country-code-record (code)
  "Return a `country-code' record for the provided country CODE. 

The code should be an ISO 2-character identifier corresponding to the 
predicate `country-code-alpha-2-p'. If no country exists with the provided 
code this function returns `nil'."
  (when (country-code-alpha-2-p code)
    (let ((country-record-pair (assoc code country-code-records--alist)))
      (when country-record-pair
        (cdr country-record-pair)))))

(defmacro additional-accessor (accessor-name alist-name property-name type-doc)
  `(defun ,accessor-name (country-or-code)
     ,(concat "Return the \""
              property-name
              "\" property for the provided country COUNTRY-OR-CODE. 

The code must be either an ISO 2-character identifier corresponding 
to the predicate `country-code-alpha-2-p' or a record corresponding
to the predicate `country-code-p'. On success this function will 
return a " type-doc ",
else it returns `nil'.")
     (cond
      ((country-code-alpha-2-p country-or-code)
       (let ((record-pair (assoc country-or-code ,alist-name)))
         (when record-pair (cdr record-pair))))
      ((country-code-p country-or-code)
       (,accessor-name (country-code-alpha-2 country-or-code))))))

(additional-accessor country-code-names
                     country-names--alist
                     "localized name"
                     "`language-tagged-list-p' value")

(additional-accessor country-code-languages
                     country-languages--alist
                     "spoken languages"
                     "a list of pairs (where `car' is a `language-code-alpha-2-p'
and the `cdr' is either t for administrative languages or nil)")

(additional-accessor country-code-territories
                     country-territories--alist
                     "territory identifiers"
                     "list of territory identifiers (`integerp')")

(additional-accessor country-code-subdivision-categories
                     country-subdivision-categories--alist
                     "subdivision categories"
                     "`language-tagged-list-p' value")

(additional-accessor country-code-subdivisions
                     country-subdivisions--alist
                     "subdivisions"
                     "list of `country-subdivision-p' values")

;; --------------------------------------------------------------------------
;; Unit Tests
;; --------------------------------------------------------------------------

(defun country-code-default ()
  "US")

;; --------------------------------------------------------------------------
;; Unit Tests
;; --------------------------------------------------------------------------

(ert-deftest country-code-alpha-2-p-test ()
  "Test the predicate for country-code 2-character alpha identifiers."
  (should (eq (country-code-alpha-2-p nil) nil))
  (should (eq (country-code-alpha-2-p "1") nil))
  (should (eq (country-code-alpha-2-p "A") nil))
  (should (eq (country-code-alpha-2-p "A1") nil))
  (should (eq (country-code-alpha-2-p "AA") t))
  (should (eq (country-code-alpha-2-p "aa") nil))
  (should (eq (country-code-alpha-2-p "AAA") nil)))

(ert-deftest country-code-status-p-test ()
  "Test the predicate for country-code-status values."
  (should (eq (country-code-status-p nil) nil))
  (should (eq (country-code-status-p 'not-sure) nil))
  (should (eq (country-code-status-p 'deleted) t)))

(ert-deftest country-code-record-test ()
  "Test the predicate for country-code records."
  (should (eq (country-code-p nil) nil))
  (let ((example-record (country-code--create
                         :alpha-2 "AG"
                         :alpha-3 "ATG"
                         :numeric 028
                         :independent? 'yes
                         :status 'officially-assigned
                         :short-name "Antigua and Barbuda")))
    (should (eq (country-code-p example-record) t))))

(ert-deftest country-code-record-lookup-test ()
  "Test the lookup of country-code-records by code."
  (should (eq (country-code-record nil) nil))
  (let ((expected-value (country-code--create
                         :alpha-2 "AG"
                         :alpha-3 "ATG"
                         :numeric 028
                         :independent? 'yes
                         :status 'officially-assigned
                         :short-name "Antigua and Barbuda")))
    (should (equal (country-code-record "AG") expected-value))))

(ert-deftest country-code-names-lookup-test ()
  "Test the lookup of country-code-names by code, or by record."
  (should (eq (country-code-names nil) nil))
  (let ((expected-value '(("en" . "Antigua and Barbuda") ("fr" . "Antigua-et-Barbuda"))))
    (should (equal (country-code-names "AG") expected-value))
    (should (equal (country-code-names (country-code-record "AG")) expected-value))))

(ert-deftest country-code-languages-lookup-test ()
  "Test the lookup of country-code-languages by code, or by recordq."
  (should (eq (country-code-languages nil) nil))
  (let ((expected-value '(("en" . t) ("fr" . nil))))
    (should (equal (country-code-languages "AG") expected-value))
    (should (equal (country-code-languages (country-code-record "AG")) expected-value))))

(ert-deftest country-code-territories-lookup-test ()
  "Test the lookup of country-code-territories by code, or by recordq."
  (should (eq (country-code-territories nil) nil))
  (let ((expected-value '(1019)))
    (should (equal (country-code-territories "AG") expected-value))
    (should (equal (country-code-territories (country-code-record "AG")) expected-value))))

(ert-deftest country-code-subdivision-categories-lookup-test ()
  "Test the lookup of country-code-subdivision-categories by code, or by recordq."
  (should (eq (country-code-territories nil) nil))
  (let ((expected-value '(298 429)))
    (should (equal (country-code-subdivision-categories "AG") expected-value))
    (should (equal (country-code-subdivision-categories (country-code-record "AG")) expected-value))))

(ert-deftest country-code-subdivisions-lookup-test ()
  "Test the lookup of country-code-subdivisions by code, or by recordq."
  (should (eq (country-code-territories nil) nil))
  (let ((expected-value '(#s(country-subdivision   "AG-10" 298 nil)
                            #s(country-subdivision "AG-11" 298 nil)
                            #s(country-subdivision "AG-03" 429 nil)
                            #s(country-subdivision "AG-04" 429 nil)
                            #s(country-subdivision "AG-05" 429 nil)
                            #s(country-subdivision "AG-06" 429 nil)
                            #s(country-subdivision "AG-07" 429 nil)
                            #s(country-subdivision "AG-08" 429 nil))))
    (should (equal (country-code-subdivisions "AG") expected-value))
    (should (equal (country-code-subdivisions (country-code-record "AG")) expected-value))))

(ert "^country-code-*")

(provide 'country-codes)

;;; country-codes.el ends here
