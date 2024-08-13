module Techdraw.Internal.TransformStack exposing
    ( TransformStack
    , new, current, push, pop
    )

{-| Transform stack.

A transform stack keeps track of a stack transformations, which can apply to multiple contexts.

    import Techdraw.Internal.BiTransform as BiTransform

    let stack1 = new
        stack2 = push (BiTransform.translation (1, 2)) stack1
        stack3 = push (BiTransform.scaling (5, 4)) stack2
        stack4 = pop stack3
        p1 = BiTransform.applyFwd (current stack1) (0, 0)
        p2 = BiTransform.applyFwd (current stack2) (0, 0)
        p3 = BiTransform.applyFwd (current stack3) (0, 0)
        p4 = BiTransform.applyFwd (current stack4) (0, 0)
    in { p1=p1, p2=p2, p3=p3, p4=p4 }
    --> {p1=(0, 0), p2=(1, 2), p3=(5, 8), p4=(1, 2)}


# Types

@docs TransformStack


# Functions

@docs new, current, push, pop

-}

import Techdraw.Internal.BiTransform as BiTransform exposing (BiTransform)


{-| Stack of transforms.
-}
type TransformStack
    = TransformStack
        { transforms : List BiTransform
        }


{-| Create a new transform stack with a single identity transformation.
-}
new : TransformStack
new =
    TransformStack { transforms = [ BiTransform.identity ] }


{-| Return the current transformation in the transform stack.

The transform stack can never be empty.

-}
current : TransformStack -> BiTransform
current (TransformStack stack) =
    case stack.transforms of
        xform :: _ ->
            xform

        [] ->
            let
                _ =
                    Debug.log "TransformStack error: empty stack!"
            in
            BiTransform.identity


{-| Push a transformation onto the stack.

The transformation matrix is composed with the current transformation and then pushed onto the
stack.

-}
push : BiTransform -> TransformStack -> TransformStack
push xform stack =
    let
        newHead =
            BiTransform.compose (current stack) xform

        (TransformStack dict) =
            stack
    in
    TransformStack
        { transforms = newHead :: dict.transforms
        }


{-| Pop the last element off a transform stack.

If a `pop` would cause the transform stack to become empty, then the `pop` is not performed (ie.
no operation is performed). There will be some debugging messages printed describing the error,
however.

-}
pop : TransformStack -> TransformStack
pop (TransformStack stack) =
    case stack.transforms of
        _ :: [] ->
            let
                _ =
                    Debug.log "TransformStack warning: trying to pop the last element!"
            in
            TransformStack stack

        _ :: rem ->
            TransformStack { transforms = rem }

        [] ->
            let
                _ =
                    Debug.log "TransformStack error: empty stack!"
            in
            new
