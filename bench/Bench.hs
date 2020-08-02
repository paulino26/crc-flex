
import Criterion.Main
import Data.Digest.CrcFlex

main :: IO ()
main = do
    let crc16Table = mkCrcTable crcCCITT
    let crc32Table = mkCrcTable crc32Iso
    defaultMain [
        bgroup "crc-16" [ 
              bench "slow" $ whnf (applyCrcModel crcCCITT) (replicate 32768 0x55)
            , bench "table" $ whnf (applyCrcTable crc16Table) (replicate 32768 0x55)
            ]
      , bgroup "crc-32" [ 
              bench "slow" $ whnf (applyCrcModel crc32Iso) (replicate 32768 0x55)
            , bench "table" $ whnf (applyCrcTable crc32Table) (replicate 32768 0x55)
            ]
        ] 