module Music1Bit.Audio
  ( writerInfo,
    writeMono,
    toWav
  )
where

import           Data.Functor.Identity
import qualified Data.StorableVector   as SV
import qualified GHC.Int
import           Music1Bit.Music       (Tick)
import           Pipes                 (Producer)
import qualified Pipes.Prelude         as Pipes
import qualified Sound.SoxLib          as SoxLib

toWav :: String -> Double -> Producer Tick Identity () -> IO ()
toWav name sr signal = writeMono sr name ((* 2 ^ 30) . toInt <$> Pipes.toList signal)


toInt :: Bool -> GHC.Int.Int32
toInt x = if x then 1 else 0

writerInfo :: Int -> SoxLib.Rate -> Int -> SoxLib.WriterInfo
writerInfo numChans sampleRate precision =
  SoxLib.defaultWriterInfo
    { SoxLib.writerSignalInfo =
        Just $
          SoxLib.defaultSignalInfo
            { SoxLib.rate = Just sampleRate,
              SoxLib.channels = Just numChans,
              SoxLib.precision = Just precision
            }
    }

writeMono rate filename chunk =
  SoxLib.withWrite (writerInfo 1 rate 16) filename $ \fmt ->
    SoxLib.writeStorableVector fmt $ SV.pack chunk
