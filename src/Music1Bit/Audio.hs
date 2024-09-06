module Music1Bit.Audio
  ( writerInfo,
    writeMono,
    toWav,
    toWav2
  )
where

import qualified Data.StorableVector      as SV
import qualified Data.StorableVector.Lazy as SVL
import qualified GHC.Int
import qualified Sound.SoxLib             as SoxLib

toWav :: String -> Double -> [Bool] -> IO ()
toWav name sr signal = writeMono sr name $ map ((* 2 ^ 30) . toInt) signal

toWav2 :: String -> Double -> [(Bool, Bool)] -> IO ()
toWav2 name sr signal = writeStereo sr name (map asInt $ flatten signal)
  where
    asInt = (*2^30) . toInt
    flatten sig = concat [[l, r] | (l, r) <- sig]

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

writeStereo rate filename chunk =
  SoxLib.withWrite (writerInfo 2 rate 16) filename $ \fmt ->
    SoxLib.writeStorableVectorLazy fmt $ SVL.pack 4000 chunk
