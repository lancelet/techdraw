module Techdraw.PathBuilder exposing
    ( PathBuilder
    , empty
    , createPath
    , moveTo, lineTo, qBezierTo, cBezierTo, arcTo, close
    )

{-|


# Types

@docs PathBuilder


# Functions

@docs empty
@docs createPath
@docs moveTo, lineTo, qBezierTo, cBezierTo, arcTo, close

-}

import List.Nonempty as Nonempty
import Techdraw.Internal.Util exposing (unsafeForceMaybe)
import Techdraw.Math
    exposing
        ( ArcTo(..)
        , CBezierTo(..)
        , LineTo(..)
        , MoveTo(..)
        , P2
        , Path(..)
        , PathCommand(..)
        , QBezierTo(..)
        , SubPath(..)
        , SubPathClosure(..)
        , orientationPi
        , p2
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
continuing (ready for more commands), or it's completed. If it has completed,
the PathBuilder needs a new SubPathBuilder, and to process any pending
commands on it.

Pending commands are necessary because a SubPath can be closed by a `moveTo`
operation, which leaves the last SubPathBuilder hanging with a move that
has to go on the front of the next SubPathBuilder.

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
createPath : PathBuilder -> Maybe Path
createPath (PathBuilder pathBuilder) =
    let
        final_backward_subpaths =
            if subPathHasCommands pathBuilder.subPathBuilder then
                subPathBuilderToSubPath pathBuilder.subPathBuilder :: pathBuilder.subPaths

            else
                pathBuilder.subPaths
    in
    Nonempty.fromList (List.reverse final_backward_subpaths)
        |> Maybe.map Path


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
    execSubPath <| subPathPush <| CmdLineTo <| LineTo <| p2 x y


{-| Draw a quadratic Bezier curve.
-}
qBezierTo : ( Float, Float ) -> ( Float, Float ) -> PathBuilder -> PathBuilder
qBezierTo ( ax, ay ) ( bx, by ) =
    execSubPath <| subPathPush <| CmdQBezierTo <| QBezierTo (p2 ax ay) (p2 bx by)


{-| Draw a cubic Bezier curve.
-}
cBezierTo : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> PathBuilder -> PathBuilder
cBezierTo ( ax, ay ) ( bx, by ) ( cx, cy ) =
    execSubPath <| subPathPush <| CmdCBezierTo <| CBezierTo (p2 ax ay) (p2 bx by) (p2 cx cy)


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
            CmdArcTo <|
                ArcTo
                    { end = p2 endx endy
                    , rx = params.rx
                    , ry = params.ry
                    , xOrient = orientationPi <| params.xAxisAngleDegrees * pi / 180
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
          closure : SubPathClosure

        -- Start of the subpath, which we may or may not know.
        , start : Maybe P2

        -- List of path commands inside the SubPath.
        , commands : List PathCommand
        }


{-| Check if the SubPathBuilder contains any path commands.
-}
subPathHasCommands : SubPathBuilder -> Bool
subPathHasCommands (SubPathBuilder builder) =
    not <| List.isEmpty builder.commands


{-| Convert the SubPathBuilder to a SubPath.
-}
subPathBuilderToSubPath : SubPathBuilder -> SubPath
subPathBuilderToSubPath (SubPathBuilder builder) =
    SubPath
        builder.closure
        (MoveTo <| Maybe.withDefault (p2 0 0) builder.start)
        (Nonempty.fromList builder.commands
            |> unsafeForceMaybe
                "Should only building a final SubPath when commands are present."
        )


type SubPathResult
    = SubPathComplete Pending SubPath
    | SubPathContinuing SubPathBuilder


{-| When a SubPathBuilder is completed, it can be in one of two states:

  - `NothingPending`: everything is finished because the SubPath was
    completed using `subPathClosed`
  - `PendingMoveTo`: a new `MoveTo` command should be added to the start
    of the next SubPathBuilder because the SubPath was completed using
    a `MoveTo`.

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
        { closure = SubPathOpen
        , start = pendingToStartPt <| pending
        , commands = []
        }


subPathMoveTo : P2 -> SubPathBuilder -> SubPathResult
subPathMoveTo pt (SubPathBuilder builder) =
    if List.isEmpty builder.commands then
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
    if List.isEmpty builder.commands then
        -- Here we closed a builder with nothing in it; re-use the builder.
        SubPathContinuing (SubPathBuilder builder)

    else
        -- Closing a path that has stuff in it.
        SubPathComplete NothingPending <|
            subPathBuilderToSubPath <|
                SubPathBuilder { builder | closure = SubPathClosed }


subPathPush : PathCommand -> SubPathBuilder -> SubPathResult
subPathPush command (SubPathBuilder builder) =
    SubPathContinuing <|
        SubPathBuilder
            { builder
                | start = Just <| Maybe.withDefault (p2 0 0) builder.start
                , commands = command :: builder.commands
            }
