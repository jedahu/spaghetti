; # Spaghetti: a simple state machine
;
(defproject
  me.panzoo/spaghetti "0.0.6-SNAPSHOT"

  :description "Finite State Machine"

; In production, Spaghetti has no external dependencies.
  :dependencies
  [
;<?
; Testing depends on [me.panzoo/jasmine], which is a collection of macros that
; wrap the [Jasmine] javascript test library.
;
; [me.panzoo/jasmine]: https://github.com/jedahu/jasmine-clj
; [Jasmine]: https://github.com/pivotal/jasmine
   [me.panzoo/jasmine "0.0.1-SNAPSHOT"]
;?>
   ])

;%include src/me/panzoo/spaghetti.cljs
