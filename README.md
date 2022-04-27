# refined-extra
Extra definitions for Nikita Volkov's
[refined](https://hackage.haskell.org/package/refined) library.

  * `Refine` and `Unrefine` typeclasses, plus generic derivations
  * `WithRefine ps p a`, a helper newtype that packages `Refined p a` and `a`
    into the same definition using a type-level switch `ps` indicating whether the
    predicate is applied or not.

See the Hackage documentation for details.
