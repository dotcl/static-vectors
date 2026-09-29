;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- DotCL implementation
;;;

(in-package :static-vectors)

;;; DotCL runs on .NET. A static vector is an ordinary specialized Lisp
;;; vector whose backing .NET array is pinned, so the GC never moves it and
;;; its address can be passed to foreign code. Elements are stored at the
;;; width foreign code expects; element types without a fixed-width layout
;;; (T, (UNSIGNED-BYTE 64), CHARACTER) are refused.

(declaim (inline fill-foreign-memory))
(defun fill-foreign-memory (pointer length value)
  "Fill LENGTH octets in foreign memory area POINTER with VALUE."
  (foreign-funcall "memset" :pointer pointer :int value :size length :pointer)
  pointer)

(declaim (inline replace-foreign-memory))
(defun replace-foreign-memory (dst-ptr src-ptr length)
  "Copy LENGTH octets from foreign memory area SRC-PTR to DST-PTR."
  (foreign-funcall "memcpy" :pointer dst-ptr :pointer src-ptr :size length :pointer)
  dst-ptr)

(declaim (inline %allocate-static-vector))
(defun %allocate-static-vector (length element-type)
  (dotcl:make-pinned-vector length element-type))

(declaim (inline static-vector-pointer))
(defun static-vector-pointer (vector &key (offset 0))
  "Return a foreign pointer to the beginning of VECTOR + OFFSET octets.
VECTOR must be a vector created by MAKE-STATIC-VECTOR."
  (check-type offset unsigned-byte)
  (inc-pointer (make-pointer (dotcl:pinned-vector-address vector)) offset))

(declaim (inline free-static-vector))
(defun free-static-vector (vector)
  "Free VECTOR, which must be a vector created by MAKE-STATIC-VECTOR."
  (dotcl:unpin-vector vector)
  (values))

(defmacro with-static-vector ((var length &rest args
                               &key (element-type ''(unsigned-byte 8))
                                 initial-contents initial-element)
                              &body body &environment env)
  "Bind PTR-VAR to a static vector of length LENGTH and execute BODY
within its dynamic extent. The vector is freed upon exit."
  (declare (ignorable element-type initial-contents initial-element))
  (multiple-value-bind (real-element-type length type-spec)
      (canonicalize-args env element-type length)
    (let ((args (copy-list args)))
      (remf args :element-type)
      `(let ((,var (make-static-vector ,length ,@args
                                       :element-type ,real-element-type)))
         (declare (type ,type-spec ,var))
         (unwind-protect
              (locally ,@body)
           (when ,var (free-static-vector ,var)))))))
