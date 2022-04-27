A **refinement type** is a type with an associated predicate, where the
predicate holds for all values of the type. The refined library encodes this
concept into a few convenient definitions, primarily

  * the newtype `Refined p a`, which stores a value of type `a` with a validated
    predicate `p`
  * the `Predicate p a` typeclass, which states that the given predicate `p` may
    be tested on the a value of type `a` at runtime

Runtime refining is then defined as:

```haskell
  refine :: Predicate p a =>           a -> Either RefineException (Refined p a)
unrefine ::                  Refined p a -> a
```

Lovely. While making the most of this, I had begun writing data types with lots
of refined fields, then duplicating them for the unrefined view, and writing
`refineX` and `unrefineX` definitions for each one. These just rewrap fields and
plug `refine` and `unrefine` calls to and fro. I'll spare you the details. The
upshot was lots of definitions that looked like this:

```haskell
  refineX :: XUnref -> Either RefineException XRef
unrefineX ::   XRef -> XUnref
```

Let's make these typeclasses!

```haskell
class   Refine unref   ref where   refine :: unref -> Either RefineException ref
class Unrefine   ref unref where unrefine ::   ref -> unref
```

These definitions might seem strange, especially `unrefine :: a -> b`, which
doesn't sound useful. But bear with me. GHC a has function that looks like both
of these merged together:

```haskell
coerce :: Coercible a b => a -> b
```

`coerce` is GHC's safe coercion primitive. A `Coercible a b` instance means `a`
and `b` have the same term-level representation. As it typechecks, GHC creates
instances of this class, enabling e.g. coercing between a newtype and its
internal type. A nice bonus is that `coerce` is zero-cost! (If you'd like to
read more, see [Safe zero-cost coercions for Haskell
(2014)](https://richarde.dev/papers/2014/coercible/coercible.pdf).)

We can think of refined and our typeclasses as a framework for _safe runtime
coercions_.

```haskell
instance Unrefine (Refined p a) a where
    -- | Remove a refined value's refinement (for free).
    unrefine = coerce

instance Predicate p a => Refine a (Refined p a) where
    -- | Attempt to refine a value. If the predicate holds, we return the same
    --   value, but tagged with the refinement. If not, we return an error.
    refine a = case validate @p a of
                 Nothing  -> Right $ coerce a
                 Just err -> Left err
```

TODO Etc. Introduce WithRefine & PredicateStatus, how they fit in, the generics
work.
