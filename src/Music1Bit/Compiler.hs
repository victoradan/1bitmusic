module Music1Bit.Compiler where

-- import Music1Bit.Audio (writeMono, seq2audio)
-- import Music1Bit.Comp
import Music1Bit.Tick
import GHC.Int (Int32)
import Data.Int (Int16 (..))
import Music1Bit.Types

seq2ints :: Eq a => a -> [a] -> [Int32]
seq2ints onVal seq = [if s == onVal then 1 else 0 | s <- seq]


-- wavify :: Comp  -> Int -> FilePath -> IO ()
-- wavify composition rate fileName = 
--   writeMono fileName $ seq2audio rate $ tick2ints $ collapse composition
