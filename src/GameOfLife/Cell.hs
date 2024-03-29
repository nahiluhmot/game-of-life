module GameOfLife.Cell
  ( Cell
  , alive
  , dead
  , toCell
  , isAlive
  , isDead
  ) where

-- 'Cell's can either be 'alive' or 'dead'. I chose to use a type alias here
-- instead of using a traditional data declaration so that I could store these
-- values in unboxed vectors.
type Cell = Bool

alive :: Cell
alive = True

dead :: Cell
dead = False

toCell :: Bool -> Cell
toCell = id

isAlive :: Cell -> Bool
isAlive = id

isDead :: Cell -> Bool
isDead = not
