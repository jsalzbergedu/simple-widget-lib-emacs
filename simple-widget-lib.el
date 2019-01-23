;;; simple-widget-lib.el --- A simple widget library for creating interactive documents -*- lexical-binding: t -*-

;; Copyright © 2019 Jacob Salzberg

;; Author: Jacob Salzberg <jssalzbe@ncsu.edu>
;; URL: https://github.com/jsalzbergedu/simple-widget-lib-emacs
;; Version: 0.1.0
;; Keywords: widget library simple interactive
;; Prefix: swli

;; This file is not a part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'cl-lib)
(require 'eieio)
(require 'subr-x)

;; Observer pattern
(cl-defgeneric swli-notify (subject)
  "Notify the SUBJECTs observers")

(cl-defgeneric swli-add-listener (subject listener)
  "Add a LISTENER to the SUBJECT.")

(defclass swli-subject ()
  ((listeners :initarg :listeners
              :initform nil
              :type list
              :custom list
              :accessor swli-listeners
              :documentation "The objects that depend on this object's state"))
  "An object that notifies listeners of
state changes when swli-notify is called on it.")

(defclass swli-element ()
  ((element-id :initform (gensym)
               :type symbol
               :custom symbol
               :reader swli-element-id
               :documentation "The unique element id.")
   (visible :initarg :visible
            :initform t
            :type boolean
            :custom boolean
            :accessor swli-visible
            :documentation "Whether the element is visible."))
  "An element of the document")

(cl-defmethod swli-point-at-element ((element swli-element))
  "Whether the point is currently at ELEMENT."
  (when-let ((id-element (swli-element-id element))
             (id-text (get-text-property (point) 'swli-element-id)))
    (eq id-element id-text)))

(cl-defgeneric swli-element-ids (element)
  "Get all of the ids that ELEMENT is responsible for rendering.")

(cl-defgeneric swli-insert-element (element)
  "Insert the ELEMENT into the document.")

(cl-defmethod swli-insert-element :before ((element swli-element))
  (setf (swli-visible element) t))

(cl-defgeneric swli-delete-element (element)
  "Remove ELEMENT from the document.")

(cl-defmethod swli-delete-element :before ((element swli-element))
  (setf (swli-visible element) nil))

(cl-defgeneric swli-redraw-element (element)
  "Redraw ELEMENT in the document.")

(cl-defmethod swli-redraw-element :around ((element swli-element))
  (when (swli-visible element) (cl-call-next-method)))

(defclass swli-widget (swli-subject swli-element)
  ((widget-position :initform 0
                    :type integer
                    :custom integer
                    :accessor swli-widget-position
                    :documentation "The position of the widget"))
  "A widget in the document.")

(cl-defmethod swli-element-ids ((element swli-widget))
  (list (swli-element-id element)))

(cl-defmethod swli-insert-element ((element swli-widget))
  (let ((inhibit-point-motion-hooks t))
    (setf (swli-widget-position element) (point))
    (with-silent-modifications
      (insert (propertize (swli-render-element element)
                          'swli-element-id
                          (swli-element-id element))))))

(cl-defmethod swli-insert-element ((element string))
  (let ((inhibit-point-motion-hooks t))
    (with-silent-modifications
      (insert element))))

(cl-defmethod swli-delete-element ((element swli-widget))
  (save-excursion
    (let ((inhibit-point-motion-hooks t)
          (point 0))
      (with-silent-modifications
        (goto-char (swli-widget-position element))
        (setq point (point))
        (while (swli-point-at-element element)
          (delete-region point (+ 1 point)))))))

(cl-defmethod swli-redraw-element ((element swli-widget))
  (save-excursion
    (let ((inhibit-point-motion-hooks t))
      (with-silent-modifications
        (swli-delete-element element)
        (goto-char (swli-widget-position element))
        (swli-insert-element element)))))

(cl-defun swli--shared-expand-single (prefix
                                      &key
                                      name
                                      initform
                                      documentation)
  "Using PREFIX, NAME, INITFORM, and DOCUMENTATION, generate a slot."
  `(,name
    :initform ,initform
    :reader ,(intern (format "%s%s" prefix name))
    :allocation :class
    :documentation ,documentation))

(cl-defun swli--shared-expand (prefix children)
  "Expand CHILDREN into forms to be passed to DEFCLASS using PREFIX."
  (cl-loop for child in children
           collect (apply #'swli--shared-expand-single prefix child)))

(cl-defun swli--immutable-expand-single (prefix
                                         &key
                                         name
                                         ((:initform initform) nil initform-bound)
                                         ((:type type) nil type-bound)
                                         documentation)
  "Using PREFIX, NAME, INITFORM, TYPE, and DOCUMENTATION, generate a slot."
  (let ((slot-type (if type-bound type (type-of initform))))
    (append `(,name)
            `(:initarg ,(intern (format ":%s" name)))
            (when initform-bound
              `(:initform ,initform))
            `(:type ,slot-type)
            `(:custom ,slot-type)
            `(:reader ,(intern (format "%s%s" prefix name)))
            `(:documentation ,documentation))))
    ;; `(,name
    ;;   :initarg ,(intern (format ":%s" name))
    ;;   ,(when (or (eq slot-type 'null) initform)
    ;;       :initform initform)
    ;;   :type ,slot-type
    ;;   :custom ,slot-type
    ;;   :reader ,(intern (format "%s%s" prefix name))
    ;;   :documentation ,documentation)))

(cl-defun swli--immutable-expand (prefix children)
  "Expand CHILDREN into forms to be passed to DEFCLASS using PREFIX."
  (cl-loop for child in children
           collect (apply #'swli--immutable-expand-single prefix child)))

(cl-defun swli--mutable-expand-single (prefix
                                       &key
                                       name
                                       initform
                                       ((:type type) nil type-bound)
                                       documentation
                                       redraw)
  "Using PREFIX, NAME, INITFORM, TYPE, and DOCUMENTATION, generate a slot."
  (let ((slot-type (if type-bound type (type-of initform))))
    `(,name
      :initform ,initform
      :type ,slot-type
      :custom ,slot-type
      :accessor ,(intern (format "%s%s" prefix name))
      :documentation ,documentation)))

(cl-defun swli--mutable-expand (prefix children)
  "Expand CHILDREN into forms to be passed to DEFCLASS using PREFIX."
  (cl-loop for child in children
           collect (apply #'swli--mutable-expand-single prefix child)))

(cl-defun swli--state-expand-single (prefix
                                     &key
                                     name
                                     initform
                                     ((:type type) nil type-bound)
                                     documentation
                                     redraw)
  (let ((slot-type (if type-bound type (type-of initform))))
    `(,name
      :initform ,initform
      :type ,slot-type
      :custom ,slot-type
      :accessor ,(intern (format "%s%s" prefix name))
      :documentation ,documentation)))

(defun swli--state-expand (prefix children)
  "Expand CHILDREN into forms to be passed to DEFCLASS using PREFIX"
  (cl-loop for child in children
           collect (apply #'swli--state-expand-single prefix child)))


;; (defun swli--grab-until (plist props)
;;   "Remove all the values of PLIST up to one of the PROPS and return them."
;;   (let ((continue t)
;;         out)
;;     (cl-loop with fst = (pop plist)
;;              while (cl-loop for prop in props
;;                             do (setq continue (and continue
;;                                                    (not (eq fst prop))))
;;                             finally return continue)
;;              do (push fst out) (setq fst (pop plist)))
;;     (nreverse out)))

(defun swli-group-into-plist (plist props)
  "Group everything after each key in the PLIST into a group based on the PROPS."
  (let ((plist plist)
        (props props)
        out)
    (setq plist (nreverse plist))
    (while (not (null plist))
      (let (group)
        (while (not (member (car plist) props))
          (push (pop plist) group))
        (push group out)
        (push (pop plist) out)))
    out))

(cl-defmethod swli--redraw-expand-single (prefix
                                          name
                                          &key
                                          ((:name slotname))
                                          initform
                                          type
                                          documentation
                                          redraw)
  (when redraw
    `(cl-defmethod (setf ,(intern (format "%s%s" prefix slotname))) :after
       (value (element ,name))
       (swli-redraw-element element))))

(cl-defmethod swli--redraw-expand (prefix name slots)
  (cl-loop for slot in slots
           collect (apply #'swli--redraw-expand-single prefix name slot)))


;; (defmacro swli-defwidget (name prefix &rest rest))
(defun swli--defwidget-helper (name
                               prefix
                               parent-classes
                               docstring
                               shared-children
                               immutable-children
                               mutable-children
                               state
                               render
                               initializer)
  "A helper to help swli-defwidget define a widget.
NAME, PREFIX, PARENT-CLASSES, DOCSTRING, SHARED-CHILDREN,
IMMUTABLE-CHILDREN, MUTABLE-CHILDREN, STATE, RENDER,
and INITIALIZER are all documented by SWLI-DEFWIDGET."
  (when (not (symbolp name))
    (error "Name is not a symbol"))
  (let ((prefix (format "%s" prefix))
        (initializer-bound (if initializer t nil)))
    `(progn (defclass ,name ,(cons 'swli-widget parent-classes)
              ,(append (swli--shared-expand prefix shared-children)
                       (swli--immutable-expand prefix immutable-children)
                       (swli--mutable-expand prefix mutable-children)
                       (swli--state-expand prefix state))
              ,docstring)
            (cl-defmethod swli-render-element ((element ,name))
              ,@render)
            (when ,initializer-bound
              (cl-defmethod initialize-instance :after ((element ,name)
                                                        &rest slots)
                ,@initializer))
            ,@(swli--redraw-expand prefix name
                                  (append mutable-children state)))))

;; (swli-defwidget fishy-fish fishy-
;;                 :docstring
;;                 "A fishy fish in the fishmobile"
;;                 :shared-children
;;                 (:name x :initform 0 :documentation "x")
;;                 :immutable-children
;;                 (:name y :initform 1 :documentation "y")
;;                 :mutable-children
;;                 (:name z :initform 2 :documentation "z")
;;                 (:name n :initform 3 :documentation "n")
;;                 :state
;;                 (:name c :initform 5 :type integer :documentation "x")
;;                 :render
;;                 (concat (format "%s%s%s" (fishy-x element)
;;                                 (fishy-y element)
;;                                 (fishy-z element)))
;;                 :initializer
;;                 (message "hey")
;;                 (message "heyhey"))

(cl-defmacro swli-defwidget (name prefix &rest rest)
  "Define a widget.
NAME is the name of the widget; a corresponding eieio class will be created.
PREFIX is the prefix for the methods created.
DOCSTRING is the class' docstring.
SHARED-CHILDREN are all of the parts of the widget shared amongst all classes.
IMMUTABLE-CHILDREN are all of the parts of the widget that cannot change.
MUTABLE-CHILDREN are all of the parts of the widget that can change.
STATE contains non renderable, mutable parts of the widget.
INITIALIZER is a list of forms to be run on an object when constructed
RENDER is a list of forms to be run on the object with the element
to be rendered bound to ELEMENT.
\(fn NAME PREFIX &key PARENT-CLASSES DOCSTRING SHARED-CHILDREN IMMUTABLE-CHILDREN MUTABLE-CHILDREN STATE RENDER INITIALIZER)"
  (declare (indent 1))
  (-let* ((list (swli-group-into-plist rest '(:parent-classes
                                               :docstring
                                               :shared-children
                                               :immutable-children
                                               :mutable-children
                                               :state
                                               :render
                                               :initializer)))
          ((&plist :parent-classes parent-classes
                   :docstring docstring
                   :shared-children shared-children
                   :immutable-children immutable-children
                   :mutable-children mutable-children
                   :state state
                   :render render
                   :initializer initializer)
           list)
          (parent-classes (car parent-classes))
          (docstring (car docstring)))
    (swli--defwidget-helper name
                            prefix
                            parent-classes
                            docstring
                            shared-children
                            immutable-children
                            mutable-children
                            state
                            render
                            initializer)))

;; (cl-defmacro swli-defwidget (name
;;                              prefix
;;                              &rest
;;                              rest
;;                              &key
;;                              parent-classes
;;                              docstring
;;                              shared-children
;;                              immutable-children
;;                              mutable-children
;;                              state)
;;                              ;; initializer
;;                              ;; render)
;;   "Define a widget.
;; NAME is the name of the widget; a corresponding eieio class will be created.
;; PREFIX is the prefix for the methods created.
;; DOCSTRING is the class' docstring.
;; SHARED-CHILDREN are all of the parts of the widget shared amongst all classes.
;; IMMUTABLE-CHILDREN are all of the parts of the widget that cannot change.
;; MUTABLE-CHILDREN are all of the parts of the widget that can change.
;; STATE contains non renderable, mutable parts of the widget.
;; INITIALIZER is a list of forms to be run on an object when constructed
;; RENDER is a list of forms to be run on the object with the element
;; to be rendered bound to ELEMENT.
;; \(fn NAME PREFIX INITIALIZER RENDER &key PARENT-CLASSES DOCSTRING SHARED-CHILDREN IMMUTABLE-CHILDREN MUTABLE-CHILDREN STATE)"
;;   (let (initializer render)
;;     (let ((fst (car rest)))
;;       (while (and (not (null fst) (not (eq :render fst))))
;;         (push (pop rest) initializer)
;;         (setq fst (car rest)))
;;       (when (not (null fst))
;;         (pop rest))
;;       (while (not (null fst))
;;         (push (pop rest) render)
;;         (set fst (car rest)))
;;       (setq initializer (nreverse initializer))
;;       (setq render (nreverse render)))
;;     (when (not (symbolp name))
;;       (error "Name is not a symbol"))
;;     (when (not (or immutable-children mutable-children ))
;;       (error "No children"))
;;     (when state
;;       (error "State is not yet supported"))
;;     (when initializer
;;       (error "Initializer is not yet supported"))
;;     (let ((prefix (format "%s" prefix)))
;;       `(progn (defclass ,name ,(cons 'swli-widget parent-classes)
;;                 ,(append (swli--shared-expand prefix shared-children)
;;                          (swli--immutable-expand prefix immutable-children)
;;                          (swli--mutable-expand prefix mutable-children))
;;                 ,docstring)
;;               (cl-defmethod swli-render-element ((element ,name))
;;                 ,@render)))))



(provide 'simple-widget-lib)
;;; simple-widget-lib.el ends here
