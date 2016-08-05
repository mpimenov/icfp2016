module Renderer where

import Control.Monad
import Control.Monad.State
import Data.Char (ord)
import Data.List (partition)
import Geom
import Graphics.UI.GLUT (($=), GLdouble, GLfloat)
import Problem
import qualified Graphics.UI.GLUT as GLUT

type Bounds = (Point, Point)
type GLRect = (GLdouble, GLdouble, GLdouble, GLdouble)

eps :: GLdouble
eps = 0.1

getBounds :: [Polygon] -> Bounds
getBounds polygons = (Point (minimum xs) (minimum ys), Point (maximum xs) (maximum ys))
    where points = concat polygons
          xs = map getX points
          ys = map getY points

normalize :: Problem -> Problem
normalize (Problem silhouette skeleton) = Problem silhouette' skeleton'
    where (origin, _) = getBounds silhouette
          silhouette' = map (map (`sub` origin)) silhouette
          skeleton' = map (\(s, e) -> (s `sub` origin, e `sub` origin)) skeleton

polygonColor = (209, 209, 111)
holeColor = (0, 0, 0)
skeletonColor = (154, 145, 27)
gridColor = (120, 120, 120)

setColor :: (Int, Int, Int) -> IO ()
setColor (r, g, b) = GLUT.color $ GLUT.Color3 (fromIntegral r / 255.0 :: GLfloat)
                                              (fromIntegral g / 255.0)
                                              (fromIntegral b / 255.0)

vertex2 :: Real a => a -> a -> GLUT.Vertex2 GLfloat
vertex2 x y = GLUT.Vertex2 (realToFrac x) (realToFrac y)

onDisplay :: Problem -> GLRect -> GLUT.DisplayCallback
onDisplay (Problem silhouette skeleton) (minX, minY, maxX, maxY) = do
  GLUT.clear [GLUT.ColorBuffer]

  let toVertex (Point x y) = vertex2 x y

  GLUT.preservingMatrix $ do
    let (polygons, holes) = partition isCCW silhouette

    setColor gridColor
    GLUT.renderPrimitive GLUT.Lines $ do
      forM_ [minX, minX + 1 .. maxX] $ \x -> do
        GLUT.vertex $ vertex2 x (minY - eps)
        GLUT.vertex $ vertex2 x (maxY + eps)
      forM_ [minY, minY + 1 .. maxY] $ \y -> do
        GLUT.vertex $ vertex2 (minX - eps) y
        GLUT.vertex $ vertex2 (maxX + eps) y

    setColor polygonColor
    forM_ polygons $ \points -> do
      GLUT.renderPrimitive GLUT.Polygon $ do
        mapM_ (GLUT.vertex . toVertex) points

    setColor holeColor
    forM_ holes $ \points -> do
      GLUT.renderPrimitive GLUT.Polygon $ do
        mapM_ (GLUT.vertex . toVertex) points

    setColor skeletonColor
    GLUT.lineWidth $= 5
    GLUT.renderPrimitive GLUT.Lines $ do
      forM_ skeleton $ \(s, e) -> do
        GLUT.vertex $ toVertex s
        GLUT.vertex $ toVertex e

  GLUT.swapBuffers

onReshape :: GLRect -> GLUT.ReshapeCallback
onReshape (minX, minY, maxX, maxY) size@(GLUT.Size width height) = do
  GLUT.viewport $= (GLUT.Position 0 0, size)
  GLUT.matrixMode $= GLUT.Projection
  GLUT.loadIdentity

  GLUT.ortho2D (minX - eps) (maxX + eps) (minY - eps) (maxY + eps)
  GLUT.postRedisplay Nothing

onKey :: GLUT.KeyboardCallback
onKey c p = do
  when (ord c == 27) $ GLUT.leaveMainLoop
  GLUT.postRedisplay Nothing
           
main :: IO ()
main = do
  GLUT.getArgsAndInitialize

  problem@(Problem silhouette _) <- liftM (normalize . evalState nextProblem) getContents
  let bounds = getBounds silhouette
      minX = realToFrac . floor . fromRational . getX $ fst bounds :: GLdouble
      minY = realToFrac . floor . fromRational . getY $ fst bounds :: GLdouble
      maxX = realToFrac . ceiling . fromRational . getX $ snd bounds :: GLdouble
      maxY = realToFrac . ceiling . fromRational . getY $ snd bounds :: GLdouble
      rect = (minX, minY, maxX, maxY)

  GLUT.initialWindowSize $= GLUT.Size 800 800
  w <- GLUT.createWindow "ICFP2016"
  GLUT.displayCallback $= onDisplay problem rect
  GLUT.reshapeCallback $= Just (onReshape rect)
  GLUT.keyboardCallback $= Just onKey
  GLUT.mainLoop
  GLUT.exit
