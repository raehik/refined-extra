{-| Typeclass for refining one data type to another.

By defining the main class's default method to use the generic deriver, we make
it dependent on the generic deriver class - which is inherently dependent on the
main class, so in order to avoid a dependency cycle, we must place the generic
code in here together.
-}

{-# LANGUAGE FunctionalDependencies #-}

module Refined.Class.Unrefine where

import Refined

import GHC.Generics
import Control.Applicative ( liftA2 )

class Unrefine a b | a -> b where
    unrefine' :: a -> b
    default unrefine' :: (Generic a, Generic b, GUnrefine (Rep a) (Rep b)) => a -> b
    unrefine' = unrefineGeneric

instance Unrefine (WithRefine 'Validated p a) (WithRefine 'Unvalidated p a) where
    unrefine' = unrefineWith

instance Unrefine a b => Unrefine [a] [b] where
    unrefine' = map unrefine'

---

unrefineGeneric :: (Generic a, Generic b, GUnrefine (Rep a) (Rep b)) => a -> b
unrefineGeneric = to . gunrefine . from

class GUnrefine fa fb where
    gunrefine :: fa p -> fb p

-- | Pair of meta wrappers: unwrap both, ignore all meta.
instance GUnrefine fa fb => GUnrefine (M1 ia ma fa) (M1 ib mb fb) where
    gunrefine = M1 . gunrefine . unM1

-- | Pair of void datatypes.
instance GUnrefine V1 V1 where
    gunrefine = id

-- | Pair of empty constructors.
instance GUnrefine U1 U1 where
    gunrefine = id

{-
instance {-# OVERLAPS #-} Unrefine a b => GUnrefine (Rec0 (WithRefine 'Validated p a)) (Rec0 (WithRefine 'Unvalidated p b)) where
    gunrefine = K1 . withRefine . unrefine' . withoutRefine . unK1
-}

instance {-# OVERLAPS #-} UnrefineG a b => GUnrefine (Rec0 (WithRefine 'Validated p a)) (Rec0 (WithRefine 'Unvalidated p b)) where
    gunrefine = K1 . withRefine . unrefineg . withoutRefine . unK1

-- | Pair of fields with equivalent types.
instance {-# OVERLAPS #-} GUnrefine (Rec0 a) (Rec0 a) where
    gunrefine = K1 . unK1

-- | Pair of fields we can unrefine.
instance Unrefine a b => GUnrefine (Rec0 a) (Rec0 b) where
    gunrefine = K1 . unrefine' . unK1

{-
instance {-# OVERLAPPING #-} GUnrefine (Rec0 (WithRefined 'Validated p a))
                                       (Rec0 (WithRefined 'Unvalidated p a)) where
    gunrefine = K1 . unK1
-}

instance (GUnrefine lfa lfb, GUnrefine rfa rfb)
  => GUnrefine (lfa :*: rfa) (lfb :*: rfb) where
    gunrefine (l :*: r) = gunrefine l :*: gunrefine r

instance (GUnrefine lfa lfb, GUnrefine rfa rfb)
  => GUnrefine (lfa :+: rfa) (lfb :+: rfb) where
    gunrefine = \case L1 l -> L1 $ gunrefine l
                      R1 r -> R1 $ gunrefine r

class UnrefineG a b where unrefineg :: a -> b
instance Unrefine a b => UnrefineG a b where unrefineg = unrefine'
instance {-# OVERLAPS #-} UnrefineG a a where unrefineg = id
