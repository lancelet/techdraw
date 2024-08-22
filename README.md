# Technical Drawing in Elm

![GitHub CI](https://github.com/lancelet/techdraw/actions/workflows/elm.yml/badge.svg)

This package is built on top of the `typed-svg` package in Elm. It provides its own affine
transformation stack which tracks local-to-world and world-to-local coordinate transformations.
This allows:

- Easy transformation of mouse events to local coordinates.
- Geometry expressed in the local coordinate system.
- Consistent global line widths.
- Shortening paths to accommodate arrow heads.

This is implemented as a tree of nodes which are interpreted at render time to `svg` nodes.

## Tooling Notes

Use [elm-test](https://package.elm-lang.org/packages/elm-explorations/test/) and
[elm-verity-examples](https://github.com/stoeffel/elm-verify-examples) for testing:

```sh
elm-test
elm-verify-examples --run-tests
```

For local documentation preview, use [elm-doc-preview](https://github.com/dmy/elm-doc-preview)

```sh
elm-doc-preview
```

## TODO

- Better fuzzer for paths - don't create primitives which start and end at the
  same point.

## Ideas

- Tagged coordinate systems. Maybe you want coordinates in some arbitrary system
  defined in the document, not just path-local or world?
- Tagged anchor points. You could specify a named anchor somewhere in the document,
  and then fetch its coordinates to use for layout.
