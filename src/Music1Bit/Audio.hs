module Music1Bit.Audio
  ( writerInfo,
    writeMono,
    toWav
  )
where

import qualified Data.StorableVector as SV
import qualified Sound.SoxLib as SoxLib
import Music1Bit.Types (Tick)
import qualified GHC.Int

toWav :: String -> Double -> [Tick] -> IO ()
toWav name sr signal = writeMono sr name $ map ((* 2 ^ 30) . toInt) signal

toInt :: Tick -> GHC.Int.Int32
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