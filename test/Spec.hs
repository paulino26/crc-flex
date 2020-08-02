
import Test.Hspec
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString as BS
import Data.Digest.CrcFlex
import Data.Digest.CrcFlex.Internal
import Data.Word

testinput :: [Word8]
testinput = BS.unpack $ BSC.pack "123456789"

builtinChecks :: [(String, CrcModel, Integer)]
builtinChecks = [
          ("crc-7 (MMC)", crc7Mmc, 0x75)
        , ("crc-8 (MAXIM)", crc8Maxim, 0xa1)
        , ("crc-11 (FLEXRAY)", crc11FlexRay, 0x5a3)
        , ("crc-16 (CCITT)", crcCCITT, 0x29b1)
        , ("crc-16 (IBM)", crc16IBM, 0xbb3d)
        , ("crc-32 (ISO)", crc32Iso, 0xcbf43926)
        , ("crc-64 (ISO)", crc64Iso, 0xb90956c775a41001)
        , ("crc-64 (EMCA)", crc64Emca, 0x6c40df5f0b497347)
    ]

main :: IO ()
main = hspec $ do
    describe "Data.Digest.CrcFlex.Internal.pow2" $ do
        it "produces 2^n for positive numbers" $ 
            pow2 8 `shouldBe` (256 :: Int)
        it "should work for n == 0" $
            pow2 0 `shouldBe` (1 :: Int)

    describe "Data.Digest.CrcFlex.Internal.reflect" $ do
        it "can reflect an 8-bit integer" $ 
            reflect 8 0xA3 `shouldBe` (0xC5 :: Integer)
        it "can reflect a 16-bit integer" $ 
            reflect 16 0x1A23 `shouldBe` (0xC458 :: Integer)
        it "reflects the least significant bits" $ 
            reflect 8 0x1A23 `shouldBe` (0x1AC4 :: Integer)

    describe "Data.Digest.CrcFlex.applyCrcModel" $ 
            mapM_ (\(n, c,chk) -> it ("should work for " ++ n) (applyCrcModel c testinput `shouldBe` chk)) builtinChecks
   
    describe "Data.Digest.CrcFlex.applyCrcTable" $
        mapM_ (\(n, c,chk) -> it ("should work for " ++ n) (applyCrcTable (mkCrcTable c) testinput `shouldBe` chk)) builtinChecks
        