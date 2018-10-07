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


-- | An invariant that startTetris and stepTetris should uphold
prop_Tetris :: Tetris -> Bool
prop_Tetris (Tetris (p, s) w r) = prop_Shape s && shapeSize w == wellSize


-- | Add black walls around a shape
createWall :: Int -> Row
createWall l = replicate l (Just Black)

addWall :: Shape -> Shape
addWall (S (s:ss)) = S $ (s:ss) ++ [createWall $ length s]

addWalls :: Shape -> Shape
addWalls s = iterate (rotateShape . addWall) s !! 4

-- | Visualize the current game state. This is what the user will see
-- when playing the game.
drawTetris :: Tetris -> Shape
drawTetris (Tetris (v,p) w _) = addWalls $ combine (shiftShape v p) w


-- | The initial game state
startTetris :: [Double] -> Tetris
startTetris rs = Tetris (startPosition,shape1) (emptyShape wellSize) supply
  where
    shape1:supply = repeat (allShapes!!1)


-- | React to input. The function returns 'Nothing' when it's game over,
-- and @'Just' (n,t)@, when the game continues in a new state @t@.
stepTetris :: Action -> Tetris -> Maybe (Int,Tetris)
stepTetris _ t = tick t

move :: Vector -> Tetris -> Tetris
move v1 (Tetris (v2, p) w r) = Tetris (vAdd v1 v2, p) w r

tick :: Tetris -> Maybe (Int, Tetris)
tick (Tetris (v, p) w r) = Just (0, (Tetris (vAdd (0, 1) v, p) w r))
