module Display (display) where

import Data.IORef
import Control.Arrow
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import GOL

vertify2 :: [(GLfloat,GLfloat)] -> IO ()
vertify2 = mapM_ (\(a,b) -> vertex $ Vertex2 a b)

vertifyInt2 :: Integral a => [(a,a)] -> IO ()
vertifyInt2 = vertify2 . map (fromIntegral *** fromIntegral)

square :: GLfloat -> IO ()
square s = renderPrimitive Polygon $ vertify2
    [ (l,l), (h,l), (h,h), (l,h) ]
    where l = 1 - s
          h = s

renderGrid :: IO ()
renderGrid = 
  renderPrimitive Lines $
    vertifyInt2 (concatMap (\x -> [(x,1), (x,height+1)]) [1..width+1] ++
                 concatMap (\y -> [(1,y), (width+1,y)]) [1..height+1])

display :: IORef Board -> IO ()
display b = do
  clear [ColorBuffer]
  loadIdentity
  
  color $ Color3 0.2 0.2 (0.2::GLfloat)
  renderGrid

  color $ Color3 0.2 1.0 (0.2::GLfloat)
  board <- get b
  mapM_ (
    (\(x,y) -> preservingMatrix $ do
                translate (Vector3 x y (0::GLfloat))
                square (0.8::GLfloat))
    .
    (fromIntegral *** fromIntegral)
    ) board
  swapBuffers