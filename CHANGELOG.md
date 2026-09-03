# Changelog

## 2.0

* Merged `inj-base` into `inj`. The instances for `base` types are no longer
  orphans and no longer require a separate package.
* Renamed the `Inj` module to `Control.Inj`. Users of `inj-1.0` should update
  their imports from `Inj` to `Control.Inj`; users of `inj-base-0.2` should
  replace `Inj.Base` with `Control.Inj`.
* Removed the instance for `Data.Semigroup.Option`, which no longer exists in
  `base`.
* Added the missing identity instance for `Char`, so that `inj 'a' :: String`
  and similar now work.
* Changed the license from `PublicDomain` to `BSD-3-Clause`.

## 1.0

* Initial release.
