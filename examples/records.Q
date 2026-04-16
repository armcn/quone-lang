# Records compile to R named lists with $ field access

import abs : Double -> Double


type alias Point <-
    { x : Double, y : Double }


point : Double -> Double -> Point
point x y <-
    { x = x, y = y }


distance : Point -> Point -> Double
distance a b <-
    let
        dx <- a.x - b.x
        dy <- a.y - b.y
    in
    sqrt (dx * dx + dy * dy)


scale : Double -> Point -> Point
scale factor p <-
    { x = p.x * factor, y = p.y * factor }


origin : Point
origin <-
    point 0.0 0.0


p1 : Point
p1 <-
    point 3.0 4.0


dist : Double
dist <-
    distance origin p1


scaled : Point
scaled <-
    scale 2.0 p1
