; # Spaghetti: a simple state machine
;
(defproject
  me.panzoo/spaghetti "0.0.6-SNAPSHOT"

  :description "Finite State Machine"

;<?
; Spaghetti's test code depends on [jasmine-clj], which is a collection of
; macros that wrap the [Jasmine] javascript test library.
;
; [jasmine-clj]: https://github.com/jedahu/jasmine-clj
; [Jasmine]: https://github.com/pivotal/jasmine
  :dependencies
  [[me.panzoo/jasmine "0.0.1-SNAPSHOT"]]
;?>
  )

;%include src/me/panzoo/spaghetti.cljs
