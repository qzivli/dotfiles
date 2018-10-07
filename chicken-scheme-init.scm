;;; -*- mode: scheme -*-
(use parley)
(let ((old-port (current-input-port)))
  (current-input-port (make-parley-port old-port)))
