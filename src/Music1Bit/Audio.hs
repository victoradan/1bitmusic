module Music1Bit.Audio
  ( writerInfo,
    writeMono,
    list2vector,
  )
where

-- import qualified GHC.Int
import qualified Data.StorableVector as SV
import qualified Sound.SoxLib as SoxLib

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

writeMono filename chunk =
  SoxLib.withWrite (writerInfo 1 44100 16) filename $ \fmt ->
    SoxLib.writeStorableVector fmt $ list2vector chunk

-- list2vector :: Foreign.Storable.Storable a => [a] -> SV.Vector a
list2vector = SV.pack
