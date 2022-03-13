;; -*- scheme -*-

;; Microcontroller monitor.
;;
;; This is:
;;
;; . Written in Lure Scheme, so it can be easily hosted on top of
;;   Racket, Lua, Erlang, JavaScript, RVM (and its targets), etc...
;;
;; . Library-less hardware interface.  I.e. no libopencm3: use direct
;;   register access only.  Centralize everything without external
;;   magic dependencies.  Use target desicription language.
;;
;; . Run-time based on Forth, init code using Forth-like compression,
;;   bootloader uses 3-instruction Forth that can be wrapped as a
;;   gdbstub.


                            
(define (f x) (+ 100 x))
(define (g x) (f (+ 1 (f x))))

