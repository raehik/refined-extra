# refined-extra
Extra definitions for Nikita Volkov's
[refined](https://hackage.haskell.org/package/refined) library.

  * `Refine` and `Unrefine` typeclasses, plus generic derivations
    * Relies on a fork that swaps `Refined p a` for `WithRefine 'Validated p a`,
      and enables users to write one definition with both validated &
      unvalidated views over it.

See the Hackage documentation for details.
