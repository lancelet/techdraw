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

- Capture L2W transformation when events are declared, not when they are
  attached to objects.
- Text.
- "Bake" node.
- Refactor to separate out styles and events stuff from the huge main file.
- Use a non-recursive traversal of the graph.
- Need a way to draw items in world coordinates.
- Arrow heads.
- More examples.