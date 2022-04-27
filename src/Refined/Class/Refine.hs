{-| Typeclass for refining one data type to another.

By defining the main class's default method to use the generic deriver, we make
it dependent on the generic deriver class - which is inherently dependent on the
main class, so in order to avoid a dependency cycle, we must place the generic
code in here together.
-}

{-# LANGUAGE FunctionalDependencies #-}

module Refined.Class.Refine where

import Refined

import GHC.Generics
import Control.Applicative ( liftA2 )

class Refine a b | a -> b where
    refine' :: a -> Either RefineException b
    default refine' :: (Generic a, Generic b, GRefine (Rep a) (Rep b)) => a -> Either RefineException b
    refine' = refineGeneric

instance Predicate p a => Refine (WithRefine 'Unvalidated p a) (WithRefine 'Validated p a) where
    refine' = refineWith

{-
instance Refine a b => Refine (WithRefine 'Unvalidated p a) (WithRefine 'Validated p b) where
    refine' = undefined -- withRefine . unrefine' . withoutRefine
-}

instance (Refine a b) => Refine [a] [b] where
    refine' = traverse refine'

---

refineGeneric
    :: (Generic a, Generic b, GRefine (Rep a) (Rep b))
    => a -> Either RefineException b
refineGeneric = fmap to . grefine . from

class GRefine fa fb where
    grefine :: fa p -> Either RefineException (fb p)

-- | Pair of meta wrappers: unwrap both, ignore all meta.
instance GRefine fa fb => GRefine (M1 ia ma fa) (M1 ib mb fb) where
    grefine = fmap M1 . grefine . unM1

-- | Pair of void datatypes.
instance GRefine V1 V1 where
    grefine = Right

-- | Pair of empty constructors.
instance GRefine U1 U1 where
    grefine = Right

instance {-# OVERLAPS #-} (RefineG a b, Predicate p b) => GRefine (Rec0 (WithRefine 'Unvalidated p a)) (Rec0 (WithRefine 'Validated p b)) where
    grefine wrva = do
        b <- refineg $ withoutRefine $ unK1 wrva
        wrvb <- refine b
        return $ K1 wrvb

instance {-# OVERLAPS #-} GRefine (Rec0 a) (Rec0 a) where
    grefine = Right . K1 . unK1

instance Refine a b => GRefine (Rec0 a) (Rec0 b) where
    grefine = fmap K1 . refine' . unK1

instance (GRefine lfa lfb, GRefine rfa rfb)
  => GRefine (lfa :*: rfa) (lfb :*: rfb) where
    grefine (l :*: r) = liftA2 (:*:) (grefine l) (grefine r)

instance (GRefine lfa lfb, GRefine rfa rfb)
  => GRefine (lfa :+: rfa) (lfb :+: rfb) where
    grefine = \case L1 l -> L1 <$> grefine l
                    R1 r -> R1 <$> grefine r

class RefineG a b where refineg :: a -> Either RefineException b
instance {-# OVERLAPS #-} RefineG a a where refineg = Right
instance Refine a b => RefineG a b where refineg = refine'

