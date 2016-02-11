module Shapes
(
Point(..),
Shape(..),
surface,
nudge,
baseRect,
baseCir,
circle,
rect
) where

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r^2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2-y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

baseRect :: Float -> Float-> Shape
baseRect a b = Rectangle (Point 0 0) (Point a b)

baseCir :: Float -> Shape
baseCir r = Circle (Point 0 0) r

rect :: Float -> Float -> Float -> Float -> Shape
rect x1 y1 l b = let rectangle = baseRect l b in nudge rectangle x1 y1

circle :: Float -> Float -> Float -> Shape
circle x1 y1 r = let some  = baseCir r in nudge some x1 y1
