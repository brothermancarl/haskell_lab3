-- | The Tetris game (main module)
module Main where
import ConsoleGUI       -- cabal install ansi-terminal
--import CodeWorldGUI     -- cabal install codeworld-api
import Shapes

--------------------------------------------------------------------------------
-- * The code that puts all the piece together

main = runGame tetrisGame

tetrisGame = Game { startGame     = startTetris,
                    stepGame      = stepTetris,
                    drawGame      = drawTetris,
                    gameInfo      = defaultGameInfo prop_Tetris,
                    tickDelay     = defaultDelay,
                    gameInvariant = prop_Tetris }

--------------------------------------------------------------------------------
-- * The various parts of the Tetris game implementation


-- | The state of the game
data Tetris = Tetris (Vector,Shape) Shape [Shape]
-- The state consists of three parts:
--   * The position and shape of the falling piece
--   * The well (the playing field), where the falling pieces pile up
--   * An infinite supply of random shapes

-- ** Positions and sizes

type Vector = (Int,Int)

-- | The size of the well
wellWidth, wellHeight :: Int
wellWidth = 10
wellHeight = 20

wellSize :: (Int,Int)
wellSize = (wellWidth,wellHeight)


-- | Starting position for falling pieces
startPosition :: Vector
startPosition = (wellWidth `div` 2 - 1, 0)

-- | Vector addition
vAdd :: Vector -> Vector -> Vector
(x1,y1) `vAdd` (x2,y2) = (x1+x2,y1+y2)

-- | Move the falling piece into position
place :: (Vector,Shape) -> Shape
place (v,s) = shiftShape v s


-- prop_Tetris return a True value if prop_Shape returns
-- True and the size of the well is equal to the shapeSize of the well

prop_Tetris :: Tetris -> Bool
prop_Tetris (Tetris (p, s) w r) = prop_Shape s && shapeSize w == wellSize


-- addWalls adds squares of black around the
-- input shape by rotating it 4 times, adding
-- the wall pieces one side at a time

addWall :: Shape -> Shape
addWall (S (s:ss)) = S $ (s:ss) ++ [createWall]
  where createWall = replicate (length s) (Just Black)

addWalls :: Shape -> Shape
addWalls s = iterate (rotateShape . addWall) s !! 4


-- drawTetris creates the game field by combining
-- the first falling shape and the well

drawTetris :: Tetris -> Shape
drawTetris (Tetris (v,p) w _) = addWalls $ combine (shiftShape v p) w


-- startTetris creates a simple initialization of the game,
-- putting a shapes from allShapes in the well, with the
-- index 0-6, depending on the input double.

startTetris :: [Double] -> Tetris
startTetris rs = Tetris (startPosition,shape1) (emptyShape wellSize) supply
  where
    shape1:supply = map (allShapes!!) (map floor (map (*6.99) rs))
    -- BÄTTRE SÄTT ATT FIXA RANDOM? enbart 0.5 för lägsta och högsta
      --repeat (allShapes!!round(rs*(length(allShapes)-1)))


-- stepTetris reacts to user input and moves the
-- piece accordingly

stepTetris :: Action -> Tetris -> Maybe (Int,Tetris)
stepTetris MoveDown t  = tick t
stepTetris MoveLeft t  = Just (0, movePiece (-1) t)
stepTetris MoveRight t = Just (0, movePiece 1 t)
stepTetris Rotate t    = Just (0, rotatePiece t)

stepTetris _ t      = tick t


-- "move" adds the input Vector with the current state of the game
-- which will move the currently falling piece accordingly

move :: Vector -> Tetris -> Tetris
move v1 (Tetris (v2, p) w r) = Tetris (v1 `vAdd` v2, p) w r


-- "tick" makes sure that the game actually changes according
-- to the user input. Guards are put in to make sure
-- that the falling piece does not move further down
-- when it has reached the bottom.

tick :: Tetris -> Maybe (Int, Tetris)
tick t
  | collision update = dropNewPiece t
  | otherwise        = Just (0, update)
    where update = move (0, 1) t

{-

-- ** C01

-}

-- collision tests if the piece overlaps with any side
-- of the well or other placed shape

collision :: Tetris -> Bool
collision (Tetris ((x, y), p) w r)
  | x < 0                          = True
  | (x + width) > wellWidth        = True
  | (y + height) > wellHeight      = True
  | overlaps (place ((x, y), p)) w = True
  | otherwise                      = False
    where (width, height) = shapeSize p

{-

-- ** C03

-}

-- movePiece is used for horizontal movement of
-- the falling piece. If the shape is colliding
-- with either the left or right side, it will not
-- move further to each respective way.

movePiece :: Int -> Tetris -> Tetris
movePiece n t
  | collision newPos = t
  | otherwise        = newPos
  where newPos = move (n, 0) t

{-

-- ** C04

&

-- ** C06

-}

-- rotate is rotates the falling shape of the input Tetris,
-- regardless if it collides with anything.

rotate :: Tetris -> Tetris
rotate (Tetris (v, p) w r) = Tetris (v, rotateShape(p)) w r


-- rotatePiece takes a Tetris and checks if any collision
-- occurs when the falling shape is rotated.

rotatePiece :: Tetris -> Tetris
rotatePiece t
  | collision rotPos = adjust (rotate t)
  | otherwise        = rotPos
  where rotPos = rotate t

{-

-- ** C05

-}

-- adjust checks if the rotated shape collides with any sides
-- or the bottom of the well, and places the rotated shape
-- accordingly, instead of blocking the rotation.

adjust :: Tetris -> Tetris
adjust (Tetris ((x, y), p) w r)
  | (x + width) > wellWidth   = (Tetris ((wellWidth-width, y), p) w r)
  | (y + height) > wellHeight = (Tetris ((x, y-(height-1)), p) w r)
  | otherwise                 = (Tetris ((x, y), p) w r)
  where (width,height) = shapeSize p


{-

-- ** C07

-}

-- dropNewPiece is called when the currently falling piece
-- collides with something downwards, meaning it shall be
-- placed there. A piece from the supply of shapes is used
-- from the startPosition, while the well is shanged to include
-- the previously falling shape.

dropNewPiece :: Tetris -> Maybe (Int,Tetris)
dropNewPiece (Tetris (v, p) w (r:rs))
  | collision newPiece = Nothing
  | otherwise          = Just (0, newPiece)
  where newPiece = (Tetris (startPosition, r) (combine w (place(v,p))) rs)
      --  (n,_)        =


{-

-- ** C08

-}

-- clearLines :: Shape -> (Int, Shape)
