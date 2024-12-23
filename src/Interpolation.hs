module Interpolation(lineInterpolation, lagrInterpolation) where

type Point a = (a, a)


lineInterpolation :: Double -> (Point Double, Point Double) -> [Point Double]
lineInterpolation step ((x0, y0), (x1, y1)) =
    let xCoordinates = takeWhile (< x1 + step) [x0, x0 + step .. ] in
        [(i, take_y_from_x i) | i <- xCoordinates] where
            take_y_from_x x = 
                (y0 * (x1 - x) + y1 * (x - x0)) / (x1 - x0)

lagrInterpolation :: Double -> (Point Double, Point Double, Point Double) -> [Point Double]
lagrInterpolation step ((x0, y0), (x1, y1), (x2, y2)) =
    let xCoordinates = takeWhile (< x2 + step) [x0, x0 + step .. ] in
        [(i, take_y_from_x i) | i <- xCoordinates] where
            take_y_from_x x = 
                (x - x1) * (x - x2) * y0 / ((x0 - x1) * (x0 - x2)) + 
                (x - x0) * (x - x2) * y1 / ((x1 - x0) * (x1 - x2)) + 
                (x - x0) * (x - x1) * y2 / ((x2 - x0) * (x2 - x1))