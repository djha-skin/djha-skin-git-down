;;;; main.lisp -- Git ASDF Groveler
;;;;
;;;; SPDX-FileCopyrightText: 2025 Daniel Jay Haskin
;;;; SPDX-License-Identifier: MIT
;;;;
#+(or)
(progn
  (declaim (optimize (speed 0) (space 0) (debug 3)))
           (asdf:load-system "serapeum"))

(in-package #:cl-user)

(defpackage
  #:com.djhaskin.gag (:use #:cl)
  (:documentation
    "
    Nestable Readable Document Language
    ")
  (:import-from #:serapeum/bundle)
  (:import-from #:quri)
  (:local-nicknames
    (#:cliff #:com.djhaskin.cliff)
    (#:nrdl #:com.djhaskin.nrdl)
    (#:s #:serapeum/bundle)
    (#:dex #:dexador))
  (:export
    github-asdfs))

(in-package #:com.djhaskin.gag)

(asdf:load-system "dexador")

(dex:get
  (quri:make-uri
    :defaults "https://api.github.com/search/code"
    :query `((
              "q" . ,(format nil "~@{~A~^ ~}"
                             "com.djhaskin.cliff.asd"
                             "in:path")
              )))
  :headers '(("Accept" . "application/vnd.github+json")
             ("X-GitHub-Api-Version" . "2022-11-28")
             ("Authorization" . "Bearer <TOKEN>"))
  :verbose t)


