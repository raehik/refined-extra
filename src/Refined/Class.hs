module Refined.Class
  (   Refine(..),   refineGeneric
  , Unrefine(..), unrefineGeneric
  ) where

import Refined.Class.Refine
import Refined.Class.Unrefine

{- $generic-deriveration

The generic derivers require that both types have near-identical structure. The
types are traversed together, and at each field, either

  * the types must match, in which case the field is simply rewrapped, or
  * there must be an instance of the relevant typeclass from @a@ to @b@, in
    which case it is used

If the types differ in structure (differing number of fields, constructors), or
a pair of fields can't be resolved as above, we refuse to derive the instance.
Even two fields with their order flipped will prevent derivation.

Their main use is for automatically generating instances for types that have a
@Validation@ type-level "switch".

If you can't derive an instance generically, you may of course define one
manually, but keep in mind such errors can indicate the existence of a type
disparity unrelated to refining.
-}

