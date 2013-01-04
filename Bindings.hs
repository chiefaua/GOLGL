module Bindings (display, reshape, keyboardMouse, advanceAnimation, animationSpeed) where

import Data.IORef
import Control.Monad
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Display
import GOL

animationSpeed :: Int
animationSpeed = 100

reshape :: Size -> IO ()
reshape s@(Size _ _) = do
  viewport $= (Position 0 0, s)
  postRedisplay Nothing

keyboardMouse :: IORef Board -> IORef Bool -> KeyboardMouseCallback
keyboardMouse _ animate (Char 't') Down _ _ = do
  a <- get animate
  animate $= not a

keyboardMouse board _ (Char 'g') Down _ _ = do
  board $= glider
  postRedisplay Nothing
keyboardMouse board _ (Char 'b') Down _ _ = do
  board $= gosperGliderGun
  postRedisplay Nothing
keyboardMouse board _ (Char 's') Down _ _ = do
  board $= smallestGrower
  postRedisplay Nothing
keyboardMouse board _ (Char 'l') Down _ _ = do
  board $= littleGrower
  postRedisplay Nothing
keyboardMouse board _ (Char 'o') Down _ _ = do
  board $= oneLineGrower
  postRedisplay Nothing
keyboardMouse board _ (Char 'p') Down _ _ = do
  b <- get board
  print b

keyboardMouse board _ (MouseButton LeftButton) Down _ (Position x y) = do
  b <- get board
  (Vertex3 x' y' _) <- clickPosition (fromIntegral x) (fromIntegral y)
  let p = (floor x', floor y')
  board $= if p `elem` b then
              filter (/=p) b
            else
              b ++ [p]
  postRedisplay Nothing

keyboardMouse _ _ _ _ _ _ = return()

advanceAnimation :: IORef Board -> IORef Bool -> TimerCallback
advanceAnimation board animate = do
  a <- get animate
  when a $
    do
      b <- get board
      board $= nextgen b
      postRedisplay Nothing
  addTimerCallback animationSpeed (advanceAnimation board animate)

clickPosition :: Int -> Int -> IO (Vertex3 GLdouble)
clickPosition x y = do
  (pos@(Position _ vy), size@(Size _ vh)) <- get viewport
  let clickPos = Vertex3 (fromIntegral x) (fromIntegral (fromIntegral vy + fromIntegral vh - y)) 0
  mv <- get (matrix (Just (Modelview 0))) :: IO (GLmatrix GLdouble)
  prj <- get (matrix (Just Projection)) :: IO (GLmatrix GLdouble)
  unProject clickPos mv prj (pos, size)