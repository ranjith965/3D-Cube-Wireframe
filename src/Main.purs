module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.JQuery (appendText, setText, setValue, addClass, body, create, css, getCss, getPageX, getPageY, off, on, select, setAttr, append)
import Control.Monad.Eff.Timer (TIMER, setTimeout)
import Control.Monad.ST (ST, STRef, newSTRef, readSTRef, writeSTRef)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.HTMLAnchorElement (text)
import DOM.HTML.Window (requestAnimationFrame)
import Data.Array (index, length, updateAt, zipWith, replicate, snoc)
import Data.Array.Partial (tail)
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe)
import Data.Number (fromString)
import Data.String (Pattern(..), contains, split, stripPrefix, stripSuffix)
import LinearAlgebra.Matrix (Matrix, column, element, fromArray, identity, rows)
import LinearAlgebra.Matrix (multiply) as M
import Math (abs, atan, cos, pi, sin, sqrt)
import Partial.Unsafe (unsafePartial)



newtype RotationVector = RotationVector (Matrix Number)

rotationVector :: Array Number -> RotationVector
rotationVector a
  | length a == 4 = RotationVector (fromMaybe (identity 1) (fromArray 4 1 a))
  | otherwise = noRotation

noRotation :: RotationVector
noRotation = RotationVector (fromMaybe (identity 1)
  (fromArray 4 1 [0.0, 0.0, 0.0, 0.0]))


newtype TransformMatrix = TransformMatrix (Matrix Number)

transformMatrix :: Array Number -> TransformMatrix
transformMatrix a
  | length a == 16 = TransformMatrix (fromMaybe (identity 1) (fromArray 4 4 a))
  | otherwise = noTransformation

noTransformation :: TransformMatrix
noTransformation = TransformMatrix (fromMaybe (identity 1)
  (fromArray 4 4 [1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0]))


class MatrixToString a where
  toString :: a -> String

instance transformMatrixToString :: MatrixToString TransformMatrix where
  toString (TransformMatrix mt) =
    "(" <>
    (fromMaybe "" $ stripPrefix (Pattern ", ") $
      foldl (\s vec -> s <> foldl (\s' n -> s' <> ", " <> show n) "" vec) "" (rows mt))
    <> ")"

instance rotationVectorToString :: MatrixToString RotationVector where
  toString (RotationVector v) =
    "("
    <> (fromMaybe "" $ stripPrefix (Pattern ", ") $
          foldl (\s vec -> s <> foldl (\s' n -> s' <> ", " <> show n) "" vec) "" (rows v))
    <> "deg)"


toTransformMatrix :: String -> TransformMatrix
toTransformMatrix str
  | contains (Pattern "matrix3d(") str = do
      let a = foldl (\ar s -> ar <> [fromMaybe 0.0 (fromString s)]) [] $
            split (Pattern ", ") $
            fromMaybe "" (stripSuffix (Pattern ")") $
            fromMaybe "" (stripPrefix (Pattern "matrix3d(") str))
      if length a /= 16
        then
          noTransformation
        else
          transformMatrix a
  | otherwise = noTransformation


multiply :: TransformMatrix -> RotationVector -> RotationVector
multiply (TransformMatrix mt) (RotationVector v) = RotationVector (M.multiply mt v)

angle :: RotationVector -> Number
angle (RotationVector m) = fromMaybe 0.0 $ element 3 0 m

sum :: Array (RotationVector) -> RotationVector
sum vs = foldl (\acc v -> add acc v) noRotation vs
  where
    add :: RotationVector -> RotationVector -> RotationVector
    add (RotationVector m1) (RotationVector m2) = do
      let a = zipWith (+) (column 0 m1) (column 0 m2)
      let x = fromMaybe 0.0 (index a 0)
      let y = fromMaybe 0.0 (index a 1)
      rotationVector $ fromMaybe [] (updateAt 3 (sqrt $ x * x + y * y) a)

average :: Array (RotationVector) -> RotationVector
average vs = do
  let (RotationVector sum) = sum vs
  let s = (fromMaybe 0.0 (element 3 0 sum)) / toNumber (length vs)
  changeSpeed s (RotationVector sum)

changeSpeed :: Number -> RotationVector -> RotationVector
changeSpeed s (RotationVector v) = do
  let x = fromMaybe 0.0 (element 0 0 v)
  let y = fromMaybe 0.0 (element 1 0 v)
  let a = if y == 0.0 then 0.0 else if x == 0.0 then pi / 2.0 else atan (abs(y / x))
  rotationVector [
    (s * cos a) * (if x < 0.0 then -1.0 else 1.0),
    (s * sin a) * (if y < 0.0 then -1.0 else 1.0),
    0.0,
    s
  ]



drawCube :: forall e. Eff (dom :: DOM |e) Unit
drawCube = do
  frontFace <- create "<figure>"
  setAttr "id" "front_face" frontFace
  addClass "face" frontFace
  setText "cube" frontFace

  backFace <- create "<figure>"
  setAttr "id" "back_face" backFace
  addClass "face" backFace

  rightFace <- create "<figure>"
  setAttr "id" "right_face" rightFace
  addClass "face" rightFace

  leftFace <- create "<figure>"
  setAttr "id" "left_face" leftFace
  addClass "face" leftFace

  topFace <- create "<figure>"
  setAttr "id" "top_face" topFace
  addClass "face" topFace

  bottomFace <- create "<figure>"
  setAttr "id" "bottom_face" bottomFace
  addClass "face" bottomFace

  cube <- create "<figure>"
  addClass "cube" cube

  css {
  	transform : "translateX(-100px) translateY(-100px) translateZ(100px)"
  } frontFace

  css {
  	transform : "translateX(-100px) translateY(-100px) translateZ(-100px)"
  } backFace
  css {
  	transform : "translateY(-100px) rotateY(90deg)"
  } rightFace
  css {
    transform : "translateY(-100px) translateX(-200px) rotateY(90deg)"
  } leftFace
  css {
    transform : "translateX(-100px) translateY(-200px) rotateX(90deg)"
  } topFace
  css {
    transform : "translateX(-100px) rotateX(90deg)"
  } bottomFace

  css {
    position: "relative",
  	transformStyle: "preserve-3d"
  } cube

  append frontFace cube
  append backFace cube
  append rightFace cube
  append leftFace cube
  append topFace cube
  append bottomFace cube

  cubeWrapper <- create "<div>"
  setAttr "id" "cube-wrapper" cubeWrapper
  css {
    position : "absolute",
    left : "50%",
    top : "50%",
    perspective: "1500px"
  } cubeWrapper
  append cube cubeWrapper

  body <- body
  append cubeWrapper body

  css { width: "100%", height: "100%" } body

  face <- select ".face"
  css {
  	position : "absolute",
  	width : "200px",
  	height : "200px",
    border : "solid black 3px"
  } face

  css { transform: "rotateX(-45deg)rotateY(45deg)"} cube


startSpeedometer :: forall eff h. STRef h RotationVector
  -> STRef h { x::Number, y::Number } -> STRef h Boolean
  -> Eff (dom :: DOM, st :: ST h, timer :: TIMER, console :: CONSOLE  | eff) Unit
startSpeedometer velocityRef mousePosRef runFlagRef = do
  let looper prevPos velocities = do
        let speedometer = do
              pos <- readSTRef mousePosRef
              let r = rotationVector [
                (negate (pos.y - prevPos.y)) * 0.4,
                (pos.x - prevPos.x) * 0.4,
                0.0, 0.0
              ]
              let newVels = snoc (unsafePartial tail velocities) r
              runFlag <- readSTRef runFlagRef
              if runFlag then looper pos newVels
                else do
                  currentVelocity <- readSTRef velocityRef
                  void $ writeSTRef velocityRef (sum [average newVels, currentVelocity])
                -- else log $ toString (average newVels)
        void $ setTimeout 25 (speedometer)
  p <- readSTRef mousePosRef
  looper p (replicate 5 noRotation)


rotateCube :: forall h e. STRef h TransformMatrix
  -> RotationVector
  -> Eff (dom :: DOM, st:: ST h |e) TransformMatrix
rotateCube transformRef rotation = do
  cube <- select ".cube"
  transform <- readSTRef transformRef
  css {
    transform: "matrix3d" <> toString transform
                  <> " rotate3d" <> (toString $ multiply transform rotation)
  } cube
  t <- getCss "transform" cube
  pure (toTransformMatrix t)


startMouseHandlers :: forall h e. STRef h TransformMatrix
  -> STRef h RotationVector
  -> Eff (dom :: DOM, st :: ST h, timer :: TIMER, console :: CONSOLE | e) Unit
startMouseHandlers transformRef velocityRef = do
  body <- body
  mousePosRef <- newSTRef {x:0.0,y:0.0}
  let downHandler event jq = do
        downX <- getPageX event
        downY <- getPageY event
        void $ writeSTRef mousePosRef {x: downX, y:downY}
        runFlagRef <- newSTRef true
        let moveHandler event' jq' = do
              x <- getPageX event'
              y <- getPageY event'
              void $ writeSTRef mousePosRef {x: x, y:y}
              let dx = negate (y - downY)
              let dy = x - downX
              let rotation = rotationVector [dx, dy, 0.0,
                    sqrt (dx * dx + dy * dy) * 0.4]
              rotateCube transformRef rotation
        let upHandler event' jq' = do
              cube <- select ".cube"
              off "mousemove" body
              t <- getCss "transform" cube
              void $ writeSTRef transformRef (toTransformMatrix t)
              writeSTRef runFlagRef false
        on "mousemove" moveHandler body
        on "mouseup" upHandler body
  on "mousedown" downHandler body


startSpinner :: forall h e. STRef h TransformMatrix
  -> STRef h RotationVector
  -> Eff (dom :: DOM, st :: ST h | e) Unit
startSpinner transformRef velocityRef = do
  let spinner = do
        rotation <- readSTRef velocityRef
        if angle rotation /= 0.0
          then do
            t <- rotateCube transformRef rotation
            void $ writeSTRef transformRef t
          else pure unit
        w <- window
        void $ requestAnimationFrame spinner w
  spinner


run :: forall e h. Eff (dom :: DOM, st :: ST h, timer :: TIMER, console :: CONSOLE | e) Unit
run = do
  cube <- select ".cube"
  t <- getCss "transform" cube
  log t
  transformRef <- newSTRef $ toTransformMatrix  t
  velocityRef <- newSTRef noRotation
  startSpinner transformRef velocityRef
  startMouseHandlers transformRef velocityRef
  pure unit


main :: forall h e. Eff (dom :: DOM, st :: ST h, timer :: TIMER, console :: CONSOLE | e) Unit
main = do
  drawCube
  run
