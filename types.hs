module Shapes
( Point(..)
, Shape(..)
, area
, nudge
, baseCircle
, baseRect
, baseSquare
) where


data Point = Point {  x :: Float
                    , y :: Float
                    } deriving (Show, Eq)

data Shape = Circle { centerPoint :: Point 
                    , radius :: Float
                    } | 
             Rect   { corner1 :: Point 
                    , corner2 :: Point
                    } deriving (Show, Eq)


nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rect (Point x1 y1) (Point x2 y2)) a b = Rect (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

center :: Shape -> Point
center (Rect(Point x1 y1) (Point x2 y2)) = Point ((x1 + x2)/2) ((y1 + y2)/2)
center c = centerPoint c

alignShapes :: Shape -> Shape -> Shape
alignShapes shape1 shape2 = nudge shape2 xdif ydif
    where xdif = (x (center shape1)) - (x (center shape2))
          ydif = (y (center shape1)) - (y (center shape2))

area :: Shape -> Float
area (Rect (Point x1 y1) (Point x2 y2)) = abs (x1 - x2) * abs (y1 - y2)
area (Circle (Point x y) r) = pi * (r^2)

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect w h = Rect (Point 0 0) (Point w h)

baseSquare :: Float -> Shape
baseSquare size = Rect (Point 0 0) (Point size size)


data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector l m n) = (i*l) + (j*m) + (k*n)

vmult :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vmult` (Vector l m n) = Vector (i*m) (j*m) (k*m)

