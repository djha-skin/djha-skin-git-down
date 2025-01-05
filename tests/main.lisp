;;;; main.lisp -- Git ASDF Groveler -- Tests
;;;;
;;;; SPDX-FileCopyrightText: 2025 Daniel Jay Haskin
;;;; SPDX-License-Identifier: MIT


#+(or)

(progn
  (declaim (optimize (speed 0) (space 0) (debug 3)))
  (progn

    (asdf:load-system "parachute")

    (asdf:load-system "com.djhaskin.nrdl")

    (asdf:test-system "com.djhaskin.nrdl")

    )
  )

(in-package #:cl-user)

(defpackage #:com.djhaskin.gag/tests
  (:use #:cl)
  (:import-from
    #:org.shirakumo.parachute
    #:define-test
    #:true
    #:false
    #:fail
    #:is
    #:isnt
    #:is-values
    #:isnt-values
    #:of-type
    #:finish
    #:test)
  (:import-from
    #:com.djhaskin.nrdl)
  (:local-nicknames
    (#:parachute #:org.shirakumo.parachute)
    (#:nrdl #:com.djhaskin.gag)))

(in-package #:com.djhaskin.gag/tests)

(define-test base-cases)

(define-test "base-cases: simple cases"
  :parent base-cases
  (is equal "One of these things is the same as the other"
      "a" "a"))