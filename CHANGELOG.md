# Revision history for marshal-contt

## 0.1.2.1 -- 2019-09-14

* Fix documentation bugs.

## 0.1.2.0 -- 2019-09-14

* Reimplement `calloc` and friends in terms of `alloca` to make them 
  exception-safe.
* Add `bracketContT` to add initialization and finalization hooks to an 
  existing `ContT`.

## 0.1.1.0 -- 2019-09-14

* Add combinators for using optics and projection functions to marshal more
  general types than simply lists into C arrays.

## 0.1.0.0 -- 2019-09-14

* Initial release.
