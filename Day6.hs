module Day6 where

import GHC.Float
import Parser

testTimes = [7, 15, 30]

testDistances = [9, 40, 200]

times = [38, 94, 79, 70]

distances = [241, 1549, 1074, 1091]

times2 = [38947970]

distances2 = [241154910741091]

isSquare n = sq * sq == n
  where
    sq = floor $ sqrt $ fromIntegral n

discriminant t r = t ^ 2 - 4 * r

intSqrt = sqrt . fromIntegral

distanceBetweenRoots t r
  | isSquare d = i - 1
  | otherwise = i + 1
  where
    d = discriminant t r
    f x = (t - x) * x
    sqrtD = intSqrt $ discriminant t r
    leftRoot = ceiling ((fromIntegral t - sqrtD) / 2)
    i = floor $ intSqrt $ discriminant t $ f leftRoot

solve a b = product $ zipWith distanceBetweenRoots a b
