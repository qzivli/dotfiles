;;; -*- mode: scheme -*-
(use numbers utf8 srfi-1)
(use parley)
(let ((old (current-input-port)))
  (current-input-port (make-parley-port old)))
