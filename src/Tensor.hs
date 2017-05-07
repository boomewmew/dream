-- Dream: music composition via machine learning.
-- Copyright (C) 2017  Boo Mew Mew

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- Address correspondence about this library to boomewmew@gmail.com.

module Tensor (Scalar, Vector, Matrix, (*)) where

import qualified Data.Array
import qualified Data.Ix

import Data.Array ((!))

type Index  = Int
type Length = Int

offset :: Length
offset = 1

maxToLength :: Index -> Length
maxToLength l = l + offset

lengthToMax :: Length -> Index
lengthToMax l = l - offset

bounds :: Length -> (Index, Index)
bounds l = (0, lengthToMax l)

indices :: Length -> [Index]
indices l = let b = bounds l in [fst b..snd b]

type Scalar = Double

type Vector = Data.Array.Array Index Scalar

vector :: [Scalar] -> Vector
vector l = Data.Array.listArray (bounds $ Prelude.length l) l

length :: Vector -> Length
length v = maxToLength $ snd $ Data.Array.bounds v

type Matrix = Data.Array.Array (Index, Index) Scalar

height :: Matrix -> Length
height m = maxToLength $ fst $ snd $ Data.Array.bounds m

width :: Matrix -> Length
width m = maxToLength $ snd $ snd $ Data.Array.bounds m

matTimesVec :: Matrix -> Vector -> Either String Vector
matTimesVec m v
    | w == l =
        Right $
            vector
                [foldr (+) 0 [m ! (i, j) * v ! j | j <- indices l]
                    | i <- indices $ height m]
    | otherwise =
        Left $
            "Matrix width (" ++ show w ++ ") must match vector length (" ++
                show l ++ ")."
    where
        w = width m
        l = Tensor.length v
