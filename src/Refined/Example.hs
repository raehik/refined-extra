module Refined.Example where

import Refined
import Refined.Class
import Refined.Extra ( UV, V )

import GHC.Generics ( Generic )

data Ex (v :: Validation) = Ex
  { ex1 :: String
  , ex2 :: WithRefine v (SizeLessThan 10) [Integer]
  , ex3 :: Integer
  , ex4 :: WithRefine 'Unvalidated (LessThan 2) Integer
  } deriving (Generic, Show, Eq)
ex = Ex "hi" (withRefine [1]) 2 (withRefine 3)
deriving anyclass instance   Refine (Ex UV) (Ex  V)
deriving anyclass instance Unrefine (Ex  V) (Ex UV)

data ExSimple = ExSimple
  { exs1 :: String
  , exs2 :: [Integer]
  , exs3 :: Integer
  , exs4 :: Integer
  } deriving (Generic, Show, Eq)
exS = ExSimple "hi" [1] 2 3
--deriving anyclass instance Unrefine (Ex V) ExSimple

data DInner (v :: Validation) = DInner
  { dinner1 :: String
  , dinner2 :: WithRefine v (SizeLessThan 10) [WithRefine v (LessThan 100) Integer]
  } deriving (Generic, Show, Eq)

data DOuter (v :: Validation) = DOuter
  { douter1 :: [DInner v]
  , douter2 :: WithRefine v (SizeLessThan 10) [Integer]
  } deriving (Generic, Show, Eq)

dinner :: DInner UV
dinner = DInner "hi" (withRefine [withRefine 99])

deriving anyclass instance   Refine (DInner UV) (DInner  V)
deriving anyclass instance Unrefine (DInner  V) (DInner UV)

deriving anyclass instance   Refine (DOuter UV) (DOuter  V)
deriving anyclass instance Unrefine (DOuter  V) (DOuter UV)
