module Techdraw.PathBuilder exposing
    ( PathBuilder
    , empty
    , createPath
    , moveTo, lineTo, qBezierTo, cBezierTo, arcTo, close
    )

{-| Building paths using procedural syntax.


# Types

@docs PathBuilder


# Functions

@docs empty
@docs createPath
@docs moveTo, lineTo, qBezierTo, cBezierTo, arcTo, close

-}

import List.Nonempty as Nonempty
import Techdraw.Math exposing (P2, orientationPi, p2)
import Techdraw.Path
    exposing
        ( ArcTo(..)
        , CBezierTo(..)
        , Completion(..)
        , LineTo(..)
        , Path(..)
        , QBezierTo(..)
        , Segment(..)
        , Start(..)
        , SubPath(..)
        )


{-| Building paths.
-}
type PathBuilder
    = PathBuilder
        { -- Builder for the current SubPath
          subPathBuilder : SubPathBuilder

        -- Completed SubPaths
        , subPaths : List SubPath
        }


{-| Empty `PathBuilder` to start creating paths.
-}
empty : PathBuilder
empty =
    PathBuilder
        { subPathBuilder = newSubPathBuilder NothingPending
        , subPaths = []
        }


{-| Update the SubPathBuilder inside the PathBuilder, based on the result of
some operation.

Basically, what's happening here is that the SubPathBuilder is either
continuing (ready for more segments), or it's completed. If it has completed,
the PathBuilder needs a new SubPathBuilder, and to process any pending
segments on it.

Pending segments are necessary because a SubPath can be closed by a `moveTo`
operation, which leaves the last SubPathBuilder hanging with a move that
has to go as the start of the next SubPathBuilder.

-}
processSubPathResult : SubPathResult -> PathBuilder -> PathBuilder
processSubPathResult result (PathBuilder pathBuilder) =
    case result of
        SubPathContinuing subPathBuilder ->
            PathBuilder
                { pathBuilder
                    | subPathBuilder = subPathBuilder
                }

        SubPathComplete pending completedSubPath ->
            PathBuilder
                { pathBuilder
                    | subPathBuilder = newSubPathBuilder pending
                    , subPaths = completedSubPath :: pathBuilder.subPaths
                }


{-| Turn a `PathBuilder` into a completed path.
-}
createPath : PathBuilder -> Path
createPath (PathBuilder pathBuilder) =
    let
        final_backward_subpaths =
            if subPathHasSegments pathBuilder.subPathBuilder then
                -- Here, we're emptying out any remaining segments in the
                -- subpath builder.
                subPathBuilderToSubPath pathBuilder.subPathBuilder
                    :: pathBuilder.subPaths

            else
                pathBuilder.subPaths
    in
    List.reverse final_backward_subpaths
        |> Path


{-| Execute a subpath program.

Most of the PathBuilder functionality defers to the SubPathBuilder via this.

-}
execSubPath : (SubPathBuilder -> SubPathResult) -> PathBuilder -> PathBuilder
execSubPath program (PathBuilder builder) =
    processSubPathResult
        (builder.subPathBuilder |> program)
        (PathBuilder builder)


{-| Move to a point.
-}
moveTo : ( Float, Float ) -> PathBuilder -> PathBuilder
moveTo ( x, y ) =
    execSubPath <| subPathMoveTo <| p2 x y


{-| Draw a line to a point.
-}
lineTo : ( Float, Float ) -> PathBuilder -> PathBuilder
lineTo ( x, y ) =
    execSubPath <| subPathPush <| SegLineTo <| LineTo <| p2 x y


{-| Draw a quadratic Bezier curve.
-}
qBezierTo : ( Float, Float ) -> ( Float, Float ) -> PathBuilder -> PathBuilder
qBezierTo ( ax, ay ) ( bx, by ) =
    execSubPath <|
        subPathPush <|
            SegQBezierTo <|
                QBezierTo (p2 ax ay) (p2 bx by)


{-| Draw a cubic Bezier curve.
-}
cBezierTo :
    ( Float, Float )
    -> ( Float, Float )
    -> ( Float, Float )
    -> PathBuilder
    -> PathBuilder
cBezierTo ( ax, ay ) ( bx, by ) ( cx, cy ) =
    execSubPath <|
        subPathPush <|
            SegCBezierTo <|
                CBezierTo (p2 ax ay) (p2 bx by) (p2 cx cy)


{-| Draw an arc.
-}
arcTo :
    { rx : Float
    , ry : Float
    , xAxisAngleDegrees : Float
    , large : Bool
    , sweep : Bool
    }
    -> ( Float, Float )
    -> PathBuilder
    -> PathBuilder
arcTo params ( endx, endy ) =
    let
        cmd =
            SegArcTo <|
                ArcTo
                    { end = p2 endx endy
                    , rx = params.rx
                    , ry = params.ry
                    , xOrient =
                        orientationPi <|
                            params.xAxisAngleDegrees
                                * pi
                                / 180
                    , large = params.large
                    , sweep = params.sweep
                    }
    in
    execSubPath <| subPathPush <| cmd


{-| Mark the current sub-path as closed.
-}
close : PathBuilder -> PathBuilder
close =
    execSubPath <| subPathClose



---- Building SubPaths --------------------------------------------------------


{-| Builder of subpaths.
-}
type SubPathBuilder
    = SubPathBuilder
        { -- Closure status of the subpath (open or closed).
          closure : Completion

        -- Start of the subpath, which we may or may not know.
        , start : Maybe P2

        -- List of path segments inside the SubPath.
        , segments : List Segment
        }


{-| Check if the SubPathBuilder contains any path segments.
-}
subPathHasSegments : SubPathBuilder -> Bool
subPathHasSegments (SubPathBuilder builder) =
    not <| List.isEmpty builder.segments


{-| Convert the SubPathBuilder to a SubPath.
-}
subPathBuilderToSubPath : SubPathBuilder -> SubPath
subPathBuilderToSubPath (SubPathBuilder builder) =
    let
        -- This exists to unpack a Maybe value that we know is always a Just
        dummy_subpath =
            SubPath Open (Start <| p2 0 0) <|
                Nonempty.singleton <|
                    SegLineTo <|
                        LineTo <|
                            p2 100 100

        spMoveTo =
            Start <| Maybe.withDefault (p2 0 0) builder.start
    in
    builder.segments
        |> List.reverse
        |> Nonempty.fromList
        |> Maybe.map (SubPath builder.closure spMoveTo)
        |> Maybe.withDefault dummy_subpath


type SubPathResult
    = SubPathComplete Pending SubPath
    | SubPathContinuing SubPathBuilder


{-| When a SubPathBuilder is completed, it can be in one of two states:

  - `NothingPending`: everything is finished because the SubPath was
    completed using `subPathClosed`
  - `PendingMoveTo`: a new `Start` should be added to the start of the next
    SubPathBuilder because the SubPath was completed using a `MoveTo`.

-}
type Pending
    = NothingPending
    | PendingMoveTo P2


pendingToStartPt : Pending -> Maybe P2
pendingToStartPt pending =
    case pending of
        NothingPending ->
            Nothing

        PendingMoveTo pt ->
            Just pt


newSubPathBuilder : Pending -> SubPathBuilder
newSubPathBuilder pending =
    SubPathBuilder
        { closure = Open
        , start = pendingToStartPt <| pending
        , segments = []
        }


subPathMoveTo : P2 -> SubPathBuilder -> SubPathResult
subPathMoveTo pt (SubPathBuilder builder) =
    if List.isEmpty builder.segments then
        -- Here the subpath hasn't started yet, so we can move it wherever
        -- it needs to go.
        SubPathContinuing <|
            SubPathBuilder { builder | start = Just pt }

    else
        -- Here the subpath has stuff in it already, so a MoveTo indicates
        -- the start of a new path.
        SubPathComplete
            (PendingMoveTo pt)
            (subPathBuilderToSubPath (SubPathBuilder builder))


subPathClose : SubPathBuilder -> SubPathResult
subPathClose (SubPathBuilder builder) =
    if List.isEmpty builder.segments then
        -- Here we closed a builder with nothing in it; re-use the builder.
        SubPathContinuing (SubPathBuilder builder)

    else
        -- Closing a path that has stuff in it.
        SubPathComplete NothingPending <|
            subPathBuilderToSubPath <|
                SubPathBuilder { builder | closure = Closed }


subPathPush : Segment -> SubPathBuilder -> SubPathResult
subPathPush segment (SubPathBuilder builder) =
    SubPathContinuing <|
        SubPathBuilder
            { builder
                | start = Just <| Maybe.withDefault (p2 0 0) builder.start
                , segments = segment :: builder.segments
            }
