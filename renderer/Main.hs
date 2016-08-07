module Main where

import Control.Monad
import Control.Monad.State
import Data.Char (ord)
import Data.Ratio
import Data.IORef
import Data.List (partition)
import Folding
import Geom
import Graphics.UI.GLUT (($=), GLdouble, GLfloat)
import Solution
import qualified Graphics.UI.GLUT as GLUT

type Bounds = (PointR, PointR)
type GLRect = (GLdouble, GLdouble, GLdouble, GLdouble)

data RState = RState { wrapping :: [(PolygonR, History Rational)]
                     , wrapLines :: [LineR]
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

onDisplay :: IORef RState -> GLRect -> GLUT.DisplayCallback
onDisplay rstate (minX, minY, maxX, maxY) = do
  GLUT.clear [GLUT.ColorBuffer]

  let toVertex (Point x y) = vertex2 x y

  GLUT.preservingMatrix $ do
    setColor gridColor
    GLUT.lineWidth $= 1
    GLUT.renderPrimitive GLUT.Lines $ do
      forM_ [minX, minX + 1 .. maxX] $ \x -> do
        GLUT.vertex $ vertex2 x (minY - eps)
        GLUT.vertex $ vertex2 x (maxY + eps)
      forM_ [minY, minY + 1 .. maxY] $ \y -> do
        GLUT.vertex $ vertex2 (minX - eps) y
        GLUT.vertex $ vertex2 (maxX + eps) y
    
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
    RState polygons wrapLines <- readIORef rstate
    case wrapLines of
      [] -> return ()
      (l:ls) -> do let polygons' = stepLine l polygons
                       solution = mkSolution polygons'
                   putStrLn "Solution:"
                   mapM_ putStrLn $ toStrings solution
                   writeIORef rstate $ RState polygons' ls
  GLUT.postRedisplay Nothing

main :: IO ()
main = do
  GLUT.getArgsAndInitialize

  let minX = (-2) :: GLdouble
      minY = (-2) :: GLdouble
      maxX = 2 :: GLdouble
      maxY = 2 :: GLdouble
      rect = (minX, minY, maxX, maxY)

  rstate <- newIORef $ RState { wrapping = [([Point 0 0, Point 1 0, Point 1 1, Point 0 1], [])]
                              , wrapLines = map (\x -> Line (Point 0 (x % 10))
                                                            (Point ((x + 4) % 2) (2 % x)))
                                            [3 .. 10]
                              }

  GLUT.initialWindowSize $= GLUT.Size 800 800
  GLUT.initialDisplayMode $= [GLUT.DoubleBuffered]
  w <- GLUT.createWindow "ICFP2016"
  GLUT.blend $= GLUT.Enabled
  GLUT.blendFunc $= (GLUT.SrcAlpha, GLUT.OneMinusSrcAlpha) 
  GLUT.displayCallback $= onDisplay rstate rect
  GLUT.reshapeCallback $= Just (onReshape rect)
  GLUT.keyboardCallback $= Just (onKey rstate)
  GLUT.mainLoop
  GLUT.exit
