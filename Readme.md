# Technical Drawing in Elm

This package interoperates with the `svg` package in Elm. It provides its own affine
transformation stack which tracks local-to-world and world-to-local coordinate transformations.
This allows:

- Easy transformation of mouse events to local coordinates.
- Geometry expressed in the local coordinate system.
- Consistent global line widths.
- Shortening paths to accommodate arrow heads.

This is implemented as a tree of nodes which are interpreted at render time to `svg` nodes.
The tree also includes an SVG breakout node, so that regular SVG can be interspersed with the
technical drawing nodes.

## Notes

Use [elm-test](https://package.elm-lang.org/packages/elm-explorations/test/) and
[elm-verity-examples](https://github.com/stoeffel/elm-verify-examples) for testing:

```sh
elm-test
elm-verify-examples --run-tests
```
