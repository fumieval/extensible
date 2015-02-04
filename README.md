extensible
======================

[![Build Status](https://travis-ci.org/fumieval/extensible.svg?branch=master)](https://travis-ci.org/fumieval/extensible)
[![Hackage](https://budueba.com/hackage/extensible)](https://hackage.haskell.org/package/extensible)

This package provides extensible poly-kinded data types, including records and polymorphic open unions.

While most rival packages takes O(n) for looking up, this package provides O(log n) access.

Extensible products can be applied to first-class pattern matching. It is potentially faster than the ordinary pattern matching, since accessing to an element is O(log n).

Bug reports and contributions are welcome.
