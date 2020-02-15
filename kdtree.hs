{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

import Data.List

class (Ord m) => Coordinates m where 
    dist :: m -> m -> Double
    getCoordinate :: Int -> m -> m
    getDim :: Int
    
getVal = id

instance Coordinates Double where
    dist x y = abs $ x - y
    getCoordinate _ x = x

instance Coordinates (Double, Double) where
    dist (x1, y1) (x2, y2) = sqrt $ (x1 - x2)^2 + (y1 - y2)^2
    getCoordinate 1 (a, b) = (0, b)
    getCoordinate _ (a, b) = (a, 0)
    

getfr :: (Double, Double) -> Double
getfr = fst

getsec :: (Double, Double) -> Double
getsec = snd

data Tree a = Leaf | Node (Tree a) a Int (Tree a) deriving (Show, Read, Eq, Ord)

buildKdTree :: (Coordinates a) => [a] -> Int -> Int -> Tree a
buildKdTree [] _ _= Leaf
buildKdTree xs dim max = Node (buildKdTree smaller dim' max) x dim (buildKdTree bigger dim' max)
    where mid = length xs `div` 2
          nthCoordinate = getCoordinate dim 
          sorted = Data.List.sortBy (\ a b -> compare (nthCoordinate a) (nthCoordinate b)) xs
          x = sorted !! mid
          sortedWithoutMid = removeNth mid $ sorted 
          smaller = [z| z <- sortedWithoutMid, nthCoordinate z <= nthCoordinate x]
          bigger = [z| z <- sortedWithoutMid, nthCoordinate z > nthCoordinate x]
          dim' = (dim + 1) `mod` max 

removeNth :: Int -> [a] -> [a]
removeNth n xs = a ++ tail b 
    where (a, b) = splitAt n xs

nvl :: Maybe a -> a -> a
nvl Nothing b = b
nvl (Just a) _ = a

if' :: Bool -> a -> a -> a
if' True v _ = v
if' _ _ v = v

findClsPoint :: (Coordinates a) => Tree a -> a -> [(a -> Double)] -> Maybe a
findClsPoint Leaf p extractors = Nothing
findClsPoint (Node left curr dim right) p extractors
    | newPointPos <= currDelimiter = let clsPoint = getClsrPointWithCurr leftPoint
                                         minDist = calcDist clsPoint
                                    in if (newPointPos + minDist)  > currDelimiter
                                        then getClsrPoint clsPoint rightPoint
                                        else clsPoint
    | otherwise = let clsPoint = getClsrPointWithCurr rightPoint
                      minDist = calcDist clsPoint
                in if (newPointPos - minDist)  < currDelimiter
                    then getClsrPoint clsPoint leftPoint
                    else clsPoint
    where rightPoint = findClsPoint right p extractors
          leftPoint = findClsPoint left p extractors
          currDelimiter = getValue curr
          calcDist (Just a) = dist a p
          calcDist Nothing = 0
          newPointPos = getValue p
          getClsrPoint Nothing b = b
          getClsrPoint a Nothing = a
          getClsrPoint aa@(Just a) bb@(Just b) = let dista = dist a p
                                                     distb = dist b p
                                                in if' (dista > distb) bb aa
          getClsrPointWithCurr = getClsrPoint (Just curr)
          getValue coord = (extractors !! dim) coord

allPointIn :: (Coordinates a) => Tree a -> [(Double, Double)] -> [(a -> Double)] -> [a]
allPointIn Leaf _ _= []
allPointIn _ [] _ = []
allPointIn (Node left curr dim right) intervals extractors = (if' (inRange) [curr] [] ) 
                                                          ++ (if' (leftSide <= currPos) leftPoints [])
                                                          ++ (if' (currPos < rightSide) rightPoints [])
    where getValue coord = (extractors !! dim) coord
          inRange = foldr1 (&&) (map calc zips)
          zips = zip extractors intervals
          calc (f, (x, y)) = let curCordPos = (f) $ curr in x <= curCordPos && curCordPos <= y
          rightPoints = allPointIn right intervals extractors
          leftPoints = allPointIn left intervals extractors
          currPos = getValue curr
          (leftSide, rightSide) = intervals !! dim

allPointInRadius :: (Coordinates a) => Tree a -> a -> Double -> [(a -> Double)] -> [a]
allPointInRadius Leaf _ _ _ = []
allPointInRadius (Node left curr dim right) centr r extractors = (if' (distToCur <= r) [curr] [])
                                                               ++ (if' (pointPos - r <= currPos) leftPoints [])
                                                               ++ (if' (pointPos + r >= currPos) rightPoints [])
    where distToCur = dist centr curr
          getValue coord = (extractors !! dim) coord
          currPos = getValue curr
          pointPos = getValue centr
          leftPoints = allPointInRadius left centr r extractors
          rightPoints= allPointInRadius right centr r extractors

          