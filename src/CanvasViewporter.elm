module CanvasViewporter exposing (CanvasViewport, canvasViewporter)

import Array exposing (Array, get)
import Canvas.Internal.Canvas as C exposing (..)
import Canvas.Internal.CustomElementJsonApi as CE exposing (Commands, commands)
import Canvas.Internal.Texture as T
import Canvas.Texture as Texture exposing (Texture)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Html.Keyed as Keyed
import Json.Decode as D


type alias CanvasViewport =
    { width : Float
    , height : Float
    , viewBox : List Float
    }


type alias Renderable =
    C.Renderable


type alias Point =
    C.Point


type alias Shape =
    C.Shape


canvasViewporter : CanvasViewport -> List Renderable -> List Renderable
canvasViewporter vp entities =
    List.map (renderableViewporter vp) entities


renderableViewporter : CanvasViewport -> Renderable -> Renderable
renderableViewporter vp (Renderable entity) =
    Renderable
        { commands = entity.commands
        , drawOp = entity.drawOp
        , drawable = drawableViewporter vp entity.drawable
        }


drawableViewporter : CanvasViewport -> Drawable -> Drawable
drawableViewporter vp entity =
    case entity of
        DrawableText text ->
            DrawableText text

        DrawableShapes list ->
            DrawableShapes (List.map (shapeMapper vp) list)

        DrawableTexture point texture ->
            DrawableTexture (pointMapper vp point) texture

        DrawableClear ( x, y ) w h ->
            let
                ( nx, ny ) =
                    pointMapper vp ( x, y )

                ( xe, ye ) =
                    pointMapper vp ( x + w, y + h )
            in
            DrawableClear ( nx, ny ) (xe - nx) (ye - ny)

        DrawableGroup list ->
            DrawableGroup (List.map (renderableViewporter vp) list)

        DrawableEmpty ->
            DrawableEmpty


shapeMapper : CanvasViewport -> Shape -> Shape
shapeMapper vp entity =
    case entity of
        Rect ( x, y ) w h ->
            let
                ( nx, ny ) =
                    pointMapper vp ( x, y )

                ( xe, ye ) =
                    pointMapper vp ( x + w, y + h )
            in
            Rect ( nx, ny ) (xe - nx) (ye - ny)

        RoundRect ( x, y ) w h r ->
            let
                ( nx, ny ) =
                    pointMapper vp ( x, y )

                ( xe, ye ) =
                    pointMapper vp ( x + w, y + h )

                nr =
                    List.map (\t -> t * getRatio vp) r
            in
            RoundRect ( nx, ny ) (xe - nx) (ye - ny) nr

        Circle ( x, y ) r ->
            Circle (pointMapper vp ( x, y )) (r * getRatio vp)

        Path ( x, y ) segments ->
            let
                ( nx, ny ) =
                    pointMapper vp ( x, y )

                nSegments =
                    List.map (pathSegmemtMapper vp) segments
            in
            Path ( nx, ny ) nSegments

        Arc ( x, y ) radius startAngle endAngle anticlockwise ->
            let
                ( nx, ny ) =
                    pointMapper vp ( x, y )

                nr =
                    radius * getRatio vp
            in
            Arc ( nx, ny ) nr startAngle endAngle anticlockwise



-- | Circle Point Float
-- | Path Point (List PathSegment)
-- | Arc Point Float Float Float Bool


pathSegmemtMapper : CanvasViewport -> PathSegment -> PathSegment
pathSegmemtMapper vp entity =
    case entity of
        MoveTo ( x, y ) ->
            MoveTo (pointMapper vp ( x, y ))

        LineTo ( x, y ) ->
            LineTo (pointMapper vp ( x, y ))

        BezierCurveTo ( x1, y1 ) ( x2, y2 ) ( x3, y3 ) ->
            BezierCurveTo (pointMapper vp ( x1, y1 )) (pointMapper vp ( x2, y2 )) (pointMapper vp ( x3, y3 ))

        QuadraticCurveTo ( x1, y1 ) ( x2, y2 ) ->
            QuadraticCurveTo (pointMapper vp ( x1, y1 )) (pointMapper vp ( x2, y2 ))

        ArcTo ( x1, y1 ) ( x2, y2 ) radius ->
            ArcTo (pointMapper vp ( x1, y1 )) (pointMapper vp ( x2, y2 )) radius


pointMapper : CanvasViewport -> Point -> Point
pointMapper vp ( x, y ) =
    let
        viewBox =
            Array.fromList vp.viewBox

        x1 =
            get 0 viewBox |> Maybe.withDefault 0

        y1 =
            get 1 viewBox |> Maybe.withDefault 0

        x2 =
            get 2 viewBox |> Maybe.withDefault 0

        y2 =
            get 3 viewBox |> Maybe.withDefault 0

        ( nWidth, nHeight ) =
            if (x2 - x1) / (y2 - y1) > vp.width / vp.height then
                ( vp.width, (y2 - y1) / (x2 - x1) * vp.width )

            else
                ( (x2 - x1) / (y2 - y1) * vp.height, vp.height )

        nx =
            (x - x1) / (x2 - x1) * nWidth + (vp.width - nWidth) / 2

        ny =
            (y - y1) / (y2 - y1) * nHeight + (vp.height - nHeight) / 2
    in
    ( nx, ny )


getRatio : CanvasViewport -> Float
getRatio vp =
    let
        viewBox =
            Array.fromList vp.viewBox

        x1 =
            get 0 viewBox |> Maybe.withDefault 0

        y1 =
            get 1 viewBox |> Maybe.withDefault 0

        x2 =
            get 2 viewBox |> Maybe.withDefault 0

        y2 =
            get 3 viewBox |> Maybe.withDefault 0

        ratio =
            if (x2 - x1) / (y2 - y1) > vp.width / vp.height then
                vp.width / (x2 - x1) * (y2 - y1)

            else
                vp.height / (y2 - y1) * (x2 - x1)
    in
    ratio
