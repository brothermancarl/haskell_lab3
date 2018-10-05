{-

  Lab 3:

  Carl Wiede & Nicklas Botö


-}



-- | Types and functions for shapes. The list of all tetris pieces.
module Shapes where
import Data.List(transpose)
import Data.Maybe(isNothing)
import Test.QuickCheck

-- * Shapes

type Square = Maybe Colour

data Colour = Black | Red | Green | Yellow | Blue | Purple | Cyan | Grey
              deriving (Eq,Bounded,Enum,Show)

-- | A geometric shape is represented as a list of lists of squares. Each square
-- can be empty or filled with a block of a specific colour.

data Shape = S [Row] deriving (Eq)
type Row = [Square]

rows :: Shape -> [Row]
rows (S rs) = rs

-- * Showing shapes

showShape :: Shape -> String
showShape s = unlines [showRow r | r <- rows s]
  where
    showRow :: Row -> String
    showRow r = [showSquare s | s <- r]

    showSquare Nothing = '.'
    showSquare (Just Black) = '#' -- can change to '█' on linux/mac
    showSquare (Just Grey)  = 'g' -- can change to '▓'
    showSquare (Just c)     = head (show c)

instance Show Shape where
  show = showShape
  showList ss r = unlines (map show ss)++r


-- * The shapes used in the Tetris game

-- | All 7 tetrominoes (all combinations of connected 4 blocks),
-- see <https://en.wikipedia.org/wiki/Tetromino>
allShapes :: [Shape]
allShapes = [S (makeSquares s) | s <- shapes]
   where
      makeSquares = map (map colour)
      colour c    = lookup c [('I',Red),('J',Grey),('T',Blue),('O',Yellow),
                              ('Z',Cyan),('L',Green),('S',Purple)]
      shapes =
              [["I",
               "I",
               "I",
               "I"],
              [" J",
               " J",
               "JJ"],
              [" T",
               "TT",
               " T"],
              ["OO",
               "OO"],
              [" Z",
               "ZZ",
               "Z "],
              ["LL",
               " L",
               " L"],
              ["S ",
               "SS",
               " S"]]

-- * Some simple functions

{-

Part A: Shapes

-}

{-

-- ** A01

-}

-- emptyShape takes a pair of Ints as input
-- and outputs a list with r elements of
-- Nothing lists with n elements

emptyShape :: (Int, Int) -> Shape
emptyShape (n,r) = S (replicate r $ replicate n Nothing)


{-

-- ** A02

-}

-- shapeSize takes a Shape as input and
-- returns the widht and height of it

shapeSize :: Shape -> (Int,Int)
shapeSize (S s) = (length $ s !! 0, length s)


{-

-- ** A03

-}

-- blockCount takes an input Shape and uses
-- the helper function rowCheck to return
-- the amount of non-empty square

blockCount :: Shape -> Int
blockCount (S s) = length [ c | c <- concat s, c /= Nothing]


{-

-- * The Shape invariant

-- ** A04

-}

-- prop_Shape takes a Shape and uses guards to
-- determine if it has no rows or any empty rows,
-- and lastly checks if all the rows are the
-- same size

prop_Shape :: Shape -> Bool
prop_Shape (S s)
  | length s == 0     = False
  | any (== []) s     = False
  | otherwise         =
    all (== length(s !! 0)) [ length(c) | c <- s]


{-

-- * Test data generators

-- ** A05

-}

-- rColour uses the quickCheck function elements
-- to generate a random Colour

rColour :: Gen Colour
rColour = [elements [Black, Red, Green, Yellow, Blue, Purple, Cyan, Grey]]

instance Arbitrary Colour where
  arbitrary = rColour


{-

-- ** A06

-}

-- rShape uses the quickCheck function elements
-- to generate a random Shape from allShapes

rShape :: Gen Shape
rShape = elements allShapes

instance Arbitrary Shape where
  arbitrary = rShape


{-

-- * Transforming shapes

-- ** A07

-}

-- rotateShape makes use Data.List function
-- transpose on the reverse of input Shape to
-- rotate the Shape correctly

rotateShape :: Shape -> Shape
rotateShape (S s) = S $ transpose $ reverse s


{-

-- ** A08

-}

-- shiftShape makes use of two helper functions
-- to first add an Int amount of empty Squares
-- to the left side of a Shape, and then adding
-- another Int amount to the top of a Shape

shiftTop :: Int -> Shape -> Shape
shiftTop t (S s) = S $ rows (emptyShape (length (s !! 0), t)) ++ s

shiftLeft :: Int -> Shape -> Shape
shiftLeft l (S s) = S $ zipWith (++) (rows $ emptyShape (l, (length s))) s

shiftShape :: (Int,Int) -> Shape -> Shape
shiftShape (l,t) s = shiftTop t (shiftLeft l s)


{-

-- ** A09

-}

-- padShape does the same thing as shiftShape,
-- but with the bottom and right side instead

padBottom :: Int -> Shape -> Shape
padBottom t (S s) = S $ s ++ rows (emptyShape (length (s !! 0), t))

padRight :: Int -> Shape -> Shape
padRight l (S s) = S $ zipWith (++) s (rows $ emptyShape (l, (length s)))

padShape :: (Int,Int) -> Shape -> Shape
padShape (r,b) s = padBottom b (padRight r s)


{-

-- ** A10

-}

-- padShapeTo makes use of padShapes to add empty squares
-- to the bottom and right of the input Shape until it
-- satisfies the size of the input tuple. If the width
-- or the height of the input tuple is smaller than the
-- actual shape, that part of the shape will not change

padShapeTo :: (Int,Int) -> Shape -> Shape
padShapeTo (w,h) s = padShape ((w-(fst size)),(h-(snd size))) s
  where size = shapeSize s


{-

-- * Comparing and combining shapes

-- ** B1

-}

-- | Test if two shapes overlap
overlaps :: Shape -> Shape -> Bool
s1 `overlaps` s2 = error "B1 overlaps undefined"

-- ** B2
-- | zipShapeWith, like 'zipWith' for lists
zipShapeWith :: (Square->Square->Square) -> Shape -> Shape -> Shape
zipShapeWith = error "A12 zipShapeWith undefined"

-- ** B3
-- | Combine two shapes. The two shapes should not overlap.
-- The resulting shape will be big enough to fit both shapes.
combine :: Shape -> Shape -> Shape
s1 `combine` s2 = error "A13 zipShapeWith undefined"
