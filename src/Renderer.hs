module Renderer where

import Control.Monad
import Control.Monad.State
import Data.Char (ord)
import Data.List (partition)
import Geom
import Graphics.UI.GLUT (($=), GLfloat)
import Problem
import qualified Graphics.UI.GLUT as GLUT

polygonColor = (209, 209, 111)
holeColor = (0, 0, 0)
skeletonColor = (154, 145, 27)

type Bounds = (Point, Point)

getBounds :: [Polygon] -> Bounds
getBounds polygons = (Point (minimum xs) (minimum ys), Point (maximum xs) (maximum ys))
    where points = concat polygons
          xs = map getX points
          ys = map getY points

normalizePoint :: Bounds -> Point -> Point
normalizePoint (Point minX minY, Point maxX maxY) (Point x y) = Point (x - minX) (y - minY)

normalizePolygon :: Bounds -> Polygon -> Polygon
normalizePolygon bounds points = map (normalizePoint bounds) points

setColor :: (Int, Int, Int) -> IO ()
setColor (r, g, b) = GLUT.color $ GLUT.Color3 (fromIntegral r / 255.0 :: GLfloat)
                                              (fromIntegral g / 255.0)
                                              (fromIntegral b / 255.0)

vertex3 :: GLfloat -> GLfloat -> GLfloat -> GLUT.Vertex3 GLfloat
vertex3 x y z = GLUT.Vertex3 x y z

vector3 :: GLfloat -> GLfloat -> GLfloat -> GLUT.Vector3 GLfloat
vector3 x y z = GLUT.Vector3 x y z

onDisplay :: Problem -> GLUT.DisplayCallback
onDisplay (Problem silhouette skeleton) = do
  GLUT.clear [GLUT.ColorBuffer]

  let toVertex (Point x y) = vertex3 (fromRational x) (fromRational y) 0.0

  GLUT.preservingMatrix $ do
    GLUT.translate $ vector3 (-1.0) (-1.0) 0.0
    GLUT.scale (2.0 :: GLfloat) 2.0 2.0

    let bounds = getBounds silhouette
        (polygons, holes) = partition isCCW $ map (normalizePolygon bounds) silhouette

    setColor polygonColor
    forM_ polygons $ \points -> do
      let vertices = map toVertex points
      GLUT.renderPrimitive GLUT.Polygon $ do
        mapM_ GLUT.vertex vertices

    setColor holeColor
    forM_ holes $ \points -> do
      let vertices = map toVertex points
      GLUT.renderPrimitive GLUT.Polygon $ do
        mapM_ GLUT.vertex vertices

    setColor skeletonColor
    GLUT.lineWidth $= 10
    forM_ skeleton $ \(s, e) -> do
      let vertices = map (toVertex . normalizePoint bounds) [s, e]
      GLUT.renderPrimitive GLUT.Lines $ do
        mapM_ GLUT.vertex vertices

  GLUT.swapBuffers

onReshape :: GLUT.ReshapeCallback
onReshape size@(GLUT.Size width height) = do
  GLUT.viewport $= (GLUT.Position 0 0, size)
  GLUT.matrixMode $= GLUT.Projection
  GLUT.loadIdentity
  GLUT.ortho2D (-1.0) 1.0 (-1.0) 1.0
  GLUT.postRedisplay Nothing

onKey :: GLUT.KeyboardCallback
onKey c p = do
  when (ord c == 27) $ GLUT.leaveMainLoop
  GLUT.postRedisplay Nothing
           
main :: IO ()
main = do
  GLUT.getArgsAndInitialize

  problem <- liftM (evalState nextProblem) getContents

  GLUT.initialWindowSize $= GLUT.Size 800 800
  w <- GLUT.createWindow "ICFP2016"
  GLUT.displayCallback $= onDisplay problem
  GLUT.reshapeCallback $= Just onReshape
  GLUT.keyboardCallback $= Just onKey
  GLUT.mainLoop
  GLUT.exit
