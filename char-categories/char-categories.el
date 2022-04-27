;;; char-categories.el --- Predicates for testing Unicode character general categories -*- lexical-binding: t; -*-

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

;; --------------------------------------------------------------------------
;; Macros
;; --------------------------------------------------------------------------

(defmacro make-category-predicates (cat-name cat)
  `(progn
     (defun ,(intern (concat "char-" (symbol-name cat-name) "-p")) (c)
       ,(concat
         "Test that the character C has the Unicode general category \""
         cat
         "\".")
       (when (characterp c)
         (string= ,cat (get-char-code-property c 'general-category))))
     (defun ,(intern (concat "string-" (symbol-name cat-name) "-p")) (s)
       ,(concat
         "Test that the string S's characters are have the Unicode general category \""
         cat
         "\".")
       (when (stringp s)
         (seq-every-p
          #',(intern (concat "char-" (symbol-name cat-name) "-p"))
          s)))))

;; --------------------------------------------------------------------------
;; General category predicates
;; --------------------------------------------------------------------------

(make-category-predicates letter-uppercase "Lu")
(make-category-predicates letter-lowercase "Ll")
(make-category-predicates letter-titlecase "Lt")
(make-category-predicates letter-modifier "Lm")
(make-category-predicates letter-other "Lo")
(make-category-predicates mark-nonspacing "Mn")
(make-category-predicates mark-spacing "Mc")
(make-category-predicates mark-enclosing "Me")

(make-category-predicates number-decimal "Nd")
(make-category-predicates number-letter "Nl")
(make-category-predicates number-other "No")

(make-category-predicates punctuation-connector "Pc")
(make-category-predicates punctuation-dash "Pd")
(make-category-predicates punctuation-open "Ps")
(make-category-predicates punctuation-close "Pe")
(make-category-predicates punctuation-initial-quote "Pi")
(make-category-predicates punctuation-final-quote "Pf")
(make-category-predicates punctuation-other "Po")

(make-category-predicates separator-space "Zs")
(make-category-predicates separator-line "Zl")
(make-category-predicates separator-paragraph "Zp")

(make-category-predicates other-control "Cc")
(make-category-predicates other-format "Cf")
(make-category-predicates other-surrogate "Cs")
(make-category-predicates other-private-use "Co")
(make-category-predicates other-not-assigned "Cn")

;; --------------------------------------------------------------------------
;; Unit Tests
;; --------------------------------------------------------------------------

(ert-deftest char-letter-lower-p-test ()
  "Test the predicate for lower case letter characters."
  (should (eq (char-letter-lowercase-p nil) nil))
  (should (eq (char-letter-lowercase-p "") nil))
  (should (eq (char-letter-lowercase-p ?A) nil))
  (should (eq (char-letter-lowercase-p ?1) nil))
  (should (eq (char-letter-lowercase-p ?a) t)))

(ert-deftest string-letter-lower-p-test ()
  "Test the predicate for lower case letter strings."
  (should (eq (string-letter-lowercase-p nil) nil))
  (should (eq (string-letter-lowercase-p ?a) nil))
  (should (eq (string-letter-lowercase-p "aBc") nil))
  (should (eq (string-letter-lowercase-p "a8c") nil))
  (should (eq (string-letter-lowercase-p "abc") t)))

(provide 'char-categories)

;;; char-categories.el ends here
