module Space where

import Data.Set
import qualified Data.Set as S

import Data.Vector
import qualified Data.Vector as V

import Numeric.LinearAlgebra
import qualified Numeric.LinearAlgebra as M





-- INITIALISATION ---------------------------------------------------------------------------------

-- The data Space (short for simplicial complex) consist of a set of simplices with
-- the following property: all the faces of all the simplices in the set must also
-- belong to the set. A simplex is implemented as a Set for efficiency.
-- For convenience, the set of simplices is split into a list according to the
-- dimension. The defining condition is unchecked as, in practice, the helper
-- function allSimplices (below) is always used to iniatialise the data. The output
-- of the helper function is always valid.

data Space t = Space { subsimplices :: [Set (Set t)] } deriving (Show)



-- This helper function takes a set of simplices as input and returns a valid
-- simplicial complex. The input consists of all the highest dimensional simplices
-- which make up the space. The function takes all their boundaries and sorts
-- them by dimension. The lowest dimension is -1 and it is the dimension of the
-- empty set. It is considered a simplex.
allSimplices :: (Ord t) => [[t]] -> [Set (Set t)]
allSimplices [] = error "This space has no faces. For empty space input '[[]]'."
allSimplices faces =

    let associatedSet = S.fromList (Prelude.map (S.fromList) faces)
        powerFaces = S.map (S.powerSet) associatedSet
        allMixedFaces = S.foldl (S.union) S.empty powerFaces
        lengthFunctions = [ \xs -> (S.size xs == n) | n <- [0,1..] ]

    in  sieveAndForget lengthFunctions allMixedFaces



sieveAndForget :: [t -> Bool] -> Set t -> [Set t]
sieveAndForget (f:fs) xs 
    | (S.size xs == 0) = []
    | otherwise        = p1 : (sieveAndForget fs p2)
        where (p1,p2) = S.partition (f) xs





-- HOMOLOGY ---------------------------------------------------------------------------------------

-- This function only computes the free rank of the homology groups. It does not give
-- any information about the torsion subgroups. These require knowledge of the
-- Smith decomposition of a matrix, an algorithm which has never been developed in
-- Haskell (over a PID). To compute the free rank, it is enough to know the rank
-- of the boundary operators (through the package Numeric.LinearALgebra)

-- This function returns the homology numbers of a space from 0 up to its dimension.
-- All the other homology numbers must be zero.
homology :: (Ord t) => Space t -> [Int]
homology (Space { subsimplices = xs }) =
    let operators = chainComplex (Space { subsimplices = xs })
        dimensionDomains = Prelude.map (M.cols) operators 
        dimensionImages = Prelude.map (M.rank) (Prelude.tail operators)
        dimensionAugmentedImages = 0:dimensionImages
        dimensionShiftedImages = dimensionImages Prelude.++ [0]
        dimensionKernels = Prelude.zipWith (-) dimensionDomains dimensionAugmentedImages

    in  Prelude.zipWith (-) dimensionKernels dimensionShiftedImages



-- Very small variation on homology. It is used to understand if a space is connected.
reducedHomology :: (Ord t) => Space t -> [Int]
reducedHomology space = reduced
    where reduced = (x-1):xs
          (x:xs) = homology space



-- Computes all boundary operators in a chain complex
chainComplex :: (Ord t) => Space t -> [M.Matrix R]
chainComplex (Space { subsimplices = xs }) = 

    let listOfSimplices = Prelude.map (S.toList) xs
        domainListOfSimplices = Prelude.tail listOfSimplices
        imageListOfSimplices = Prelude.init listOfSimplices
        domainVectorOfSimplices = Prelude.map (V.fromList) domainListOfSimplices
        imageVectorOfSimplices = Prelude.map (V.fromList) imageListOfSimplices
        domainImage = Prelude.zip domainVectorOfSimplices imageVectorOfSimplices

    in  Prelude.map (boundaryOperator) domainImage
       


-- Computes boundary operator for all simplices of a fixed dimension.
boundaryOperator :: (Ord t) => (V.Vector (Set t) , V.Vector (Set t))-> M.Matrix R
boundaryOperator (domain , image) = convertToMatrix vectorMatrix
    where vectorMatrix = V.map (f) domain
          f = \x -> (piecewiseBoundaryOperator x image)



-- Computes boundary operator of a single simplex.
piecewiseBoundaryOperator :: (Ord t) => Set t -> V.Vector (Set t) -> V.Vector Int
piecewiseBoundaryOperator element image =         
    let f imageFace
              | (what == False) = 0
              | odd indicator   = 1
              | otherwise       = -1
          
              where (s1 , what , s2) = S.splitMember imageFace facesOfElement
                    -- Shortcut to compute the sign of a face
                    indicator = S.size element + S.size s1
                    facesOfElement = S.map (g) element 
                    g = \y -> (S.delete y element)

    in  V.map (f) image 
  




-- MATRIX CONVERSION ------------------------------------------------------------------------------

-- These algorithms are necessary to convert from V.Vector to M.Matrix. 
-- Unfortunately these two data lack compatibility. Furthermore
-- M.Vector cannot store Sets, hence they cannot be used from the beginning.
-- Ideally, the Smith Normal Form should be called directly on the V.Vector
-- so to avoid using M.Matrix all together.

convertToMatrix :: V.Vector (V.Vector Int) -> M.Matrix R
convertToMatrix vectorMatrix = M.matrix s doubleFlattenedListMatrix
    where s = V.length vectorMatrix
          doubleFlattenedListMatrix = Prelude.map (fromIntegral) flattenedListMatrix
          flattenedListMatrix = Prelude.foldl (Prelude.++) [] listMatrix
          listMatrix = Prelude.map (V.toList) listVectorMatrix
          listVectorMatrix = V.toList transposedVectorMatrix
          transposedVectorMatrix = transpose vectorMatrix
    

transpose :: V.Vector (V.Vector Int) -> V.Vector (V.Vector Int)
transpose matrix = V.foldr (g) emptyVector matrix
    where emptyVector = V.replicate s V.empty
          s = V.length ((V.!) matrix 0)
          g = \x y -> V.zipWith (V.cons) x y 





-- BASIC FUNCTIONS --------------------------------------------------------------------------------

-- These functions are basic as they do not rely on computing homology.

-- The function returns the dimension of the highest dimensional simplices. 
-- 2 is subtracted to account for the simplices of dimension 0 and -1.
dimension :: (Ord t) => Space t -> Int
dimension (Space { subsimplices = xs }) = Prelude.length xs - 2



-- This function returns the number of simplices of a given dimension.
numberOfSimplices :: (Ord t) => Space t -> Int -> Int
numberOfSimplices (Space { subsimplices = xs }) dim
    | dim < -1 || dim > maxDim = 0
    | otherwise                   = S.size simplicesDim

    where maxDim = dimension (Space { subsimplices = xs })
          simplicesDim = xs !! index
          index = dim + 1



eulerCharacteristic :: (Ord t) => Space t -> Int
eulerCharacteristic (Space { subsimplices = xs }) = 
    alternatingSum (Prelude.tail lengths)

        where alternatingSum :: (Num t) => [t] -> t
              alternatingSum [] = error "Not defined."
              alternatingSum [x] = x
              alternatingSum (x:xs) = x - alternatingSum xs

              lengths = (Prelude.map (S.size) xs)





-- ADVANCED FUNCTIONS -----------------------------------------------------------------------------

-- These functions all rely on homology.

isConnected :: (Ord t) => Space t -> Bool
isConnected space = (zeroHomology == 1)
    where zeroHomology = Prelude.head (homology space)



-- This function can tell if a space is not homotopic to a topological manifold.
-- Note that, for the reasons above, it only checks Poincaré duality up to torsion.
rationalPoincareDuality :: (Ord t) => Space t -> Bool
rationalPoincareDuality space = (homNumbers == revHomNumbers)
    where revHomNumbers = Prelude.reverse homNumbers
          homNumbers = homology space



-- If this function returns False then the answer is definitely No. If it returns
-- True, then the algorthm is inconcludent. The function first checks the Euler
-- characteristic as this is a less expensice invariant to compute, albeit coarser.
-- If it is the same, homology ranks are computed.
couldItBeContractible :: (Ord t) => Space t -> Bool
couldItBeContractible space = (euler == 1) && (isZero reduced) 
    where euler = eulerCharacteristic space
          reduced = reducedHomology space



-- Simple helper function to check if all the entries in a list are zero
isZero :: [Int] -> Bool
isZero [] = error "Empty list."
isZero [0] = True
isZero (x:xs) = (x == 0) && (isZero xs)



-- Again, if the function returns True, no new information is gained. The
-- Euler characteristic is computed first. This algorithm only checks
-- if the homology numbers are the same, however, it is necessary to add
-- enough zeros to the shorter list to equate the lengths.
couldTheyBeHomotopic :: (Ord t) => Space t -> Space t -> Bool
couldTheyBeHomotopic space1 space2 = (euler1 == euler2) && (hom1 == hom2)
    where euler1 = eulerCharacteristic space1
          euler2 = eulerCharacteristic space2
          hom1 = tempHom1 Prelude.++ zeroList1
          hom2 = tempHom2 Prelude.++ zeroList2
          tempHom1 = homology space1
          tempHom2 = homology space2
          zeroList1 = [ 0 | x <- [1..s1]]
          zeroList2 = [ 0 | x <- [1..s2]]
          s1 = maxLength - length1
          s2 = maxLength - length2
          length1 = Prelude.length tempHom1
          length2 = Prelude.length tempHom2
          maxLength = Prelude.maximum [length1, length2]