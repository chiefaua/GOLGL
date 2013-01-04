module GOL (Board, Pos, width, height, nextgen,
            glider, gosperGliderGun, smallestGrower, littleGrower, oneLineGrower) where

import Data.List

width :: Int
width = 50
height :: Int
height = 50

type Board = [Pos]
type Pos = (Int, Int)

isAlive :: Board -> Pos -> Bool
isAlive b p = p `elem` b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1,y-1), (x,y-1),
                          (x+1,y-1), (x-1,y),
                          (x+1,y), (x-1,y+1),
                          (x,y+1), (x+1,y+1)]

wrap :: Pos -> Pos
wrap (x,y) = (((x-1) `mod` width) + 1,
              ((y-1) `mod` height) +1)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> [Pos]
survivors b = [p | p <- b, liveneighbs b p `elem` [2,3]]

births :: Board -> [Pos]
births b = [p | p <- nub (concatMap neighbs b),
                  isEmpty b p,
                  liveneighbs b p == 3]

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

glider :: Board
glider = [(4,2), (2,3), (4,3), (3,4), (4,4)]

gosperGliderGun :: Board
gosperGliderGun = [(3,11),(2,11),(3,12),(2,12),(12,12),(12,11),(12,13),
                   (13,10),(13,14),(14,9),(15,9),(14,15),(15,15),(16,12),
                   (17,10),(18,11),(18,12),(18,13),(17,14),(19,12),(22,11),
                   (22,10),(22,9),(23,9),(23,10),(23,11),(24,12),(24,8),
                   (26,8),(26,7),(26,12),(26,13),(36,9),(36,10),(37,9),(37,10)]

smallestGrower :: Board
smallestGrower = [(10,16),(12,16),(12,15),(14,14),(14,13),(14,12),(16,13),
                  (16,12),(16,11),(17,12)]

littleGrower :: Board
littleGrower = [(10,6),(10,5),(11,5),(12,5),(14,5),(14,7),(14,8),(14,9),
                (13,7),(12,9),(12,8),(11,8),(10,9)]

oneLineGrower :: Board
oneLineGrower = [(3,2),(2,2),(4,2),(5,2),(6,2),(7,2),(8,2),(9,2),(11,2),(12,2),
                 (13,2),(14,2),(15,2),(19,2),(20,2),(21,2),(28,2),(29,2),(30,2),
                 (31,2),(32,2),(34,2),(33,2),(36,2),(37,2),(38,2),(39,2),(40,2)]