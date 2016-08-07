module Main where

import Control.Monad
import Control.Monad.State
import Data.Char (ord)
import Data.IORef
import Data.List (partition)
import Folding
import Geom
import Graphics.UI.GLUT (($=), GLdouble, GLfloat)
import Problem
import Solution
import qualified Graphics.UI.GLUT as GLUT

type Bounds = (PointR, PointR)
type GLRect = (GLdouble, GLdouble, GLdouble, GLdouble)

data RState = RState { figure :: PolygonR
                     , wrapping :: [(PolygonR, History Rational)]
                     }
              deriving (Show)

eps :: GLdouble
eps = 0.1

polygonColor = (209, 209, 111, 80)
holeColor = (0, 0, 0, 80)
skeletonColor = (100, 100, 27, 80)
gridColor = (120, 120, 120, 80)
hullColor = (170, 80, 80, 40)
wrappingColor = (0, 0, 255, 40)

setColor :: (Int, Int, Int, Int) -> IO ()
setColor (r, g, b, a) = GLUT.color $ GLUT.Color4 (fromIntegral r / 255.0 :: GLfloat)
                                                 (fromIntegral g / 255.0)
                                                 (fromIntegral b / 255.0)
                                                 (fromIntegral a / 100.0)

vertex2 :: Real a => a -> a -> GLUT.Vertex2 GLfloat
vertex2 x y = GLUT.Vertex2 (realToFrac x) (realToFrac y)

onDisplay :: IORef RState -> ProblemR -> GLRect -> GLUT.DisplayCallback
onDisplay rstate (Problem silhouette skeleton) (minX, minY, maxX, maxY) = do
  GLUT.clear [GLUT.ColorBuffer]

  let toVertex (Point x y) = vertex2 x y

  GLUT.preservingMatrix $ do
    let (polygons, holes) = partition isCCW silhouette

    setColor gridColor
    GLUT.lineWidth $= 1
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

    setColor hullColor
    GLUT.renderPrimitive GLUT.Polygon $ do
      mapM_ (GLUT.vertex . toVertex) (convexHull . concat $ silhouette)

    setColor wrappingColor
    facets <- liftM (map fst . wrapping) (readIORef rstate)
    forM_ facets $ \facet -> do
      GLUT.renderPrimitive GLUT.Polygon $ do
        mapM_ (GLUT.vertex . toVertex) facet

  GLUT.swapBuffers

onReshape :: GLRect -> GLUT.ReshapeCallback
onReshape (minX, minY, maxX, maxY) size@(GLUT.Size width height) = do
  GLUT.viewport $= (GLUT.Position 0 0, size)
  GLUT.matrixMode $= GLUT.Projection
  GLUT.loadIdentity

  GLUT.ortho2D (minX - eps) (maxX + eps) (minY - eps) (maxY + eps)
  GLUT.postRedisplay Nothing

onKey :: IORef RState -> GLUT.KeyboardCallback
onKey rstate c p = do
  when (ord c == 27) $ GLUT.leaveMainLoop
  when (c == 'w' || c == 'W') $ do
    RState figure polygons <- readIORef rstate
    writeIORef rstate $ RState figure (step figure polygons)
  GLUT.postRedisplay Nothing

normalize :: (Problem Rational) -> (Problem Rational)
normalize (Problem silhouette skeleton) = Problem silhouette'' skeleton''
    where ax@(Point a c) = getAxis silhouette
          ay@(Point b d) = ort ax

          to' (Point x y) = Point (x * a + y * c) (x * b + y * d)
          silhouette' = map (map to') silhouette
          (origin', _) = getBounds silhouette'
          silhouette'' = map (map (`sub` origin')) silhouette'

          skeleton'' = map (\(s, e) -> ((to' s) `sub` origin', (to' e) `sub` origin')) skeleton
           
main :: IO ()
main = do
  GLUT.getArgsAndInitialize

  problem@(Problem silhouette _) <- liftM (normalize . evalState nextProblem) getContents
  let bounds = getBounds silhouette
      minX = realToFrac . floor . getX $ fst bounds :: GLdouble
      minY = realToFrac . floor . getY $ fst bounds :: GLdouble
      maxX = realToFrac . ceiling . getX $ snd bounds :: GLdouble
      maxY = realToFrac . ceiling . getY $ snd bounds :: GLdouble
      rect = (minX, minY, maxX, maxY)

  rstate <- newIORef $ RState { figure = (convexHull $ concat silhouette)
                              , wrapping = [([Point 0 0, Point 1 0, Point 1 1, Point 0 1], [])]
                              }

  GLUT.initialWindowSize $= GLUT.Size 800 800
  GLUT.initialDisplayMode $= [GLUT.DoubleBuffered]
  w <- GLUT.createWindow "ICFP2016"
  GLUT.blend $= GLUT.Enabled
  GLUT.blendFunc $= (GLUT.SrcAlpha, GLUT.OneMinusSrcAlpha) 
  GLUT.displayCallback $= onDisplay rstate problem rect
  GLUT.reshapeCallback $= Just (onReshape rect)
  GLUT.keyboardCallback $= Just (onKey rstate)
  GLUT.mainLoop
  GLUT.exit
