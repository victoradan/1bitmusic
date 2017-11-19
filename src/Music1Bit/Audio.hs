module Music1Bit.Audio
  (
    writerInfo,
    writeMono,
    seq2audio,
    list2vector
  ) where

-- import qualified GHC.Int
import qualified Data.StorableVector as SV
import qualified Sound.SoxLib as SoxLib


writerInfo :: Int -> SoxLib.Rate -> Int -> SoxLib.WriterInfo
writerInfo numChans sampleRate precision =
   SoxLib.defaultWriterInfo {
      SoxLib.writerSignalInfo = Just $
         SoxLib.defaultSignalInfo {
            SoxLib.rate = Just sampleRate,
            SoxLib.channels = Just numChans,
            SoxLib.precision = Just precision
         }
   }

writeMono filename chunk =
   SoxLib.withWrite (writerInfo 1 44100 16) filename $ \fmt ->
      SoxLib.writeStorableVector fmt  $ list2vector chunk
      
tempo2audioRate :: Integral a => a -> a
tempo2audioRate n = 44100 `div` n

-- convert a Seq to 44100 16 bit train of pulses
-- TODO: optimize. This is the slowest function of the package.
seq2audio :: Num a => Int -> [a] -> [a]
seq2audio _ [] = []
seq2audio sampleRate (x:xs) = (x*2^29):(replicate ((tempo2audioRate sampleRate)-1) 0) ++ seq2audio sampleRate xs

-- list2vector :: Foreign.Storable.Storable a => [a] -> SV.Vector a
list2vector lst = SV.pack lst

