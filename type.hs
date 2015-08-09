--Types
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

--Functions
surface :: Shape->Float
surface (Circle _ r) = pi * r^2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x1-x2) * (abs $ y1-y2)

translate::Shape->Float->Float->Shape
translate (Circle (Point x y)  r) a b = Circle (Point x+a y+b) r
translate (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point x1+a y1+b) (Point x2+a x2+b)

data Person = {}
