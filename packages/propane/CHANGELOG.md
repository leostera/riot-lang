# Changelog

All notable changes to `propane` are documented here.

## 0.0.26 - 2026-04-28

### Changed

- Generators, shrinkers, printers, and property runners have more consistent behavior for common standard-library containers such as lists, arrays, options, results, hash maps, hash sets, queues, deques, and heaps.
- Property failures now retain stable printed values and shrink toward smaller counterexamples more predictably, making failing property tests easier to debug.
