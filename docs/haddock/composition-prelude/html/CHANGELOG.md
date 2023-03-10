# composition-prelude

# 3.0.0.2

  * Faster `thread`, hopefully

# 3.0.0.0

  * Remove `-.**` etc. and replace with `.@`
  * Fix documentation
  * Add rule for `thread`
  * `-.` has a different fixity
  * Add `+>`, `&:` infix synonyms

# 2.0.5.0

  * Add `(.*****)` and `(.******)`.
  * Polish documentation

# 2.0.4.0

  * Fix `(.@@@)`

# 2.0.3.0

  * Add `(.@)`, `(.@@)`, `(.@@@)`, `(.@@@@)` operators
  * Improved documentation

# 2.0.2.2

  * Polish documentation

# 2.0.2.1

  * Polish documentation

# 2.0.2.0

  * Add `.$`

# 2.0.1.0

  * Add `dup` for tuples

# 2.0.0.0

  * Fix fixity of various operators

# 1.5.3.0

  * Add `between` and `~@~`

# 1.5.2.0

  * Add monadic versions of various composition operators.
  * Performance improvements related to the inliner

# 1.5.1.0

  * Add `threadM`
  * Generalize `thread` to work on any `Foldable` of functions
