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
-- putting index 1 of allShapes in an empty well

startTetris :: [Double] -> Tetris
startTetris rs = Tetris (startPosition,shape1) (emptyShape wellSize) supply
  where
    shape1:supply = repeat (allShapes!!1)


-- | React to input. The function returns 'Nothing' when it's game over,
-- and @'Just' (n,t)@, when the game continues in a new state @t@.

-- (currently only calls the tick function,
-- not caring for any actual user input)

stepTetris :: Action -> Tetris -> Maybe (Int,Tetris)
stepTetris _ t = tick t


-- "move" adds the input Vector with the current state of the game
-- which will move the currently falling piece accordingly

move :: Vector -> Tetris -> Tetris
move v1 (Tetris (v2, p) w r) = Tetris (v1 `vAdd` v2, p) w r


-- "tick" makes sure that the game actually changes according
-- to the user input. For now it only moves the piece down
-- one position, which means any user input will also move it down
-- one position.

tick :: Tetris -> Maybe (Int, Tetris)
tick (Tetris (v, p) w r) = Just (0, (Tetris (vAdd (0, 1) v, p) w r))
