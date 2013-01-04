import Data.IORef
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Bindings
import GOL

main :: IO ()
main = do
  (_, _) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  _ <- createWindow "Game of Life - GL"

  matrixMode $= Projection
  ortho2D (1::GLdouble) (fromIntegral width + 1) (fromIntegral height + 1) (1::GLdouble)
  matrixMode $= Modelview 0

  board <- newIORef glider
  animate <- newIORef True

  displayCallback $= display board
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just (keyboardMouse board animate)

  addTimerCallback animationSpeed (advanceAnimation board animate)

  mainLoop
