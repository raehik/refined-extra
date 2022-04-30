## 0.4.0 (Unreleased)
  * rewrite as refined-extra
  * working (if wonky) classy interface
  * `WithRefine` etc. provided by a fork with various small patches
    * TODO: this prevents us from releasing to Hackage :(

## 0.3.0 (2022-04-23)
  * use more consistently useful type variable order across definitions

## 0.2.1 (2022-04-23)
  * add `unsafeWithRefine :: forall p ps a. a -> WithRefine ps p a`

## 0.2.0 (2022-04-23)
  * rename `unWithRefine` to `withoutRefine`
  * reorder `reallyUnsafeEnforce` typevars for visible type application
    convenience

## 0.1.0 (2022-04-22)
Initial release.

  * extracted from gtvm-hs
