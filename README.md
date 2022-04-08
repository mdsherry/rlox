# RLox

A Rust implementation of an approximation of the Lox language from Crafting Interpretters.

Known bugs:
  * Super doesn't work as expected when the superclass hasn't implemented a method, but its superclass has.
  * Statically ensuring that `this` and `super` are only used in method calls.
  * `super` dispatch less efficient than it could be

