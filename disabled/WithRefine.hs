{-|
A wrapper on top of "Refined" that encourages using the same type for refined
and unrefined views of the same data. Useful when both views have identical
structure (i.e. the only difference is that whether some invariants are
validated). For information on refinement types and the underlying concept, see
"Refined".

Definitions generally order typevars predicate first and @a@ last, to support
visible type application (@a@ is usually obvious to the compiler).

Say you're already sold on refinement types as a method to easily describe data
more precisely without complicating algorithms or comprising runtime
performance. You work with plain types internally, and refine only when you need
to. This means when working with records with refined types, you necessarily
duplicate the record: one for refined, one for unrefined.

Sometimes this is fine, because you change the data structure between refining
(e.g. you drop some indices that are generatable). Sometimes, it means you have
two records storing the same data, where one has some invariants validated and
the other doesn't. In the former case, you should have one data type with
'Refined' fields, one without. This module focuses on the latter use case.

The 'WithRefine' newtype enables you to use the same type for both views by
wrapping refined types with a type-level switch that tracks whether the
predicate is currently validated.

  * The type @t'WithRefine' ''Validated' p a@ corresponds to @'Refined' p a@: a
    value of type @a@ with a validated predicate @p@.
  * The type @t'WithRefine' ''Unvalidated' p a@ corresponds to... @a@. It's a
    value of type @a@ tagged with predicate @p@, but the predicate remains
    unvalidated.

Runtime impact should be the same as refined, pretty much nothing.
-}

module Refined.WithRefine
  (
  -- * 'WithRefine' types
    WithRefine
  , withRefine, withoutRefine
  , Validation(..)

  -- * Coercing between Refined
  , validatedToRefined
  , refinedToValidated

  -- * Unsafe
  , unsafeValidated

  -- * Other definitions
  , WithRefineRep
  ) where

import Refined hiding ( refine, unrefine, refineFail )
import Refined qualified as R
import Refined.Unsafe
import Refined.Class
import Data.Proxy
import Control.DeepSeq ( NFData )
import Data.Hashable ( Hashable )
import Data.Aeson
import Control.Exception ( displayException )
import GHC.TypeLits ( TypeError, ErrorMessage(..) )

import GHC.Generics ( Generic )
import Data.Data ( Typeable, Data )

-- | A wrapper over a type @a@ with a predicate @p@, with a type-level "switch"
--   indicating whether the predicate is validated for the value or not.
--
-- @t'WithRefine' ''Validated' p a@ is equivalent to 'Refined p a', while
-- @t'WithRefine' ''Unvalidated' p a@ is equivalent to @a@. This newtype lets you
-- use the same term-level constructor for both. One could then combine multiple
-- t'WithRefine'-wrapped fields into a single record and parameterize over the
-- predicate status to essentially enable "weak" and "strong" views of the
-- record, while conveniently using the same value-level representation.
newtype WithRefine (v :: Validation) (p :: k) a
  = WithRefine {
      -- | Strips the predicate from a any wrapped value (validated or not).
      withoutRefine :: a
  } deriving       (Eq, Ord, Hashable, NFData) via a
    deriving stock (Generic, Typeable, Data, Foldable, Show)

deriving stock instance Functor     (WithRefine 'Unvalidated p)
deriving stock instance Traversable (WithRefine 'Unvalidated p)

instance ToJSON   a => ToJSON   (WithRefine v          p a) where
    toJSON     = toJSON     . withoutRefine
    toEncoding = toEncoding . withoutRefine

-- | Use the underlying type's instance.
instance FromJSON a => FromJSON (WithRefine 'Unvalidated p a) where
    parseJSON = fmap withRefine . parseJSON

instance (FromJSON a, Predicate p a) => FromJSON (WithRefine 'Validated p a) where
    parseJSON = fmap refinedToValidated . parseJSON

refineFail :: forall a b m. (Refine a b, MonadFail m) => a -> m b
refineFail a = case refine a of
                 Left  e -> fail $ displayException e
                 Right b -> pure b


-- TODO I don't really understand this. I naively use the same role annotation
-- as 'Refined'. GHC user's guide page:
-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/roles.html
type role WithRefine nominal nominal nominal

-- | Wrap a value with any unvalidated predicate.
withRefine :: forall p a. a -> WithRefine 'Unvalidated p a
withRefine = WithRefine

-- | Has the associated predicate been validated for the associated data?
--
-- TODO Used to be called @PredicateStatus@. Unsure.
data Validation
  = Validated
  | Unvalidated

-- | If you have a @t'WithRefine' ''Validated'@, you can obtain a matching
--   'Refined'.
validatedToRefined :: forall p a. WithRefine 'Validated p a -> Refined p a
validatedToRefined = reallyUnsafeRefine . withoutRefine

-- | If you have a 'Refined', you can obtain a matching @t'WithRefine'
--   ''Validated'@.
refinedToValidated :: forall p a. Refined p a -> WithRefine 'Validated p a
refinedToValidated = unsafeValidated . unrefine

-- | Construct an @t'WithRefine' ''Validated'@ without validating the predicate.
--
-- You should only use this if you can prove that the predicate holds.
unsafeValidated :: forall p a. a -> WithRefine 'Validated p a
unsafeValidated = WithRefine

{-
-- | Wrap a value with any predicate, enforced or unenforced. This is useful for
--   "no-op" refinements, potentially letting you to merge two instances into
--   one, where you explicitly ignore the 'Validation' (perhaps clearer).
--
-- You should only use this if you can prove that the refinement holds for all
-- values of @a@.
unsafeWithRefine :: forall v p a. a -> WithRefine v p a
unsafeWithRefine = WithRefine
-}

-- | Not very useful, but clarifies the meaning of 'WithRefine'
type family WithRefineRep (v :: Validation) (p :: k) a where
    WithRefineRep 'Validated   p a = Refined p a
    WithRefineRep 'Unvalidated _ a = a

instance Unrefine (WithRefine 'Validated p a) (WithRefine 'Unvalidated p a) where
    unrefine = withRefine . withoutRefine

instance Unrefine (WithRefine 'Validated p a) a where
    unrefine = withoutRefine

instance Predicate p a => Refine (WithRefine 'Unvalidated p a) (WithRefine 'Validated p a) where
    refine = fmap refinedToValidated . refine . withoutRefine

--instance TypeError ('Text "TODO no, you're already unvalidated") => Unrefine (WithRefine 'Unvalidated p a) (WithRefine 'Unvalidated p a) where
--    unrefine = undefined
