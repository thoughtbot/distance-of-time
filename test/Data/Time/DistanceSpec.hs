module Data.Time.DistanceSpec
    ( main
    , spec
    ) where

import qualified Data.Time as T
import           Data.Time.Distance
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

instance Arbitrary T.Day where
    arbitrary =
        T.fromGregorian
        <$> choose (1600, 2400)
        <*> choose (1, 12)
        <*> choose (1, 31)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    context "Data.Time.Distance" $ parallel $ modifyMaxSuccess (* 1000) $ do
        describe "distanceOfTimeInWords" $ do
            it "handles milliseconds" $ property $ \d -> do
                distanceOfTimeInWords (addMilliseconds d (-30)) (toTime d) `shouldBe` "30 milliseconds ago"
                distanceOfTimeInWords (addMilliseconds d (900)) (toTime d) `shouldBe` "900 milliseconds from now"

            it "handles seconds" $ property $ \d -> do
                distanceOfTimeInWords (addMilliseconds d (-30000)) (toTime d) `shouldBe` "30 seconds ago"
                distanceOfTimeInWords (addMilliseconds d (45000)) (toTime d) `shouldBe` "45 seconds from now"

            it "handles hours" $ property $ \d -> do
                distanceOfTimeInWords (addHours d (-2)) (toTime d) `shouldBe` "2 hours ago"
                distanceOfTimeInWords (addHours d 2) (toTime d) `shouldBe` "2 hours from now"

            it "handles days" $ property $ \d -> do
                distanceOfTimeInWords (addDays d (-2)) (toTime d) `shouldBe` "2 days ago"
                distanceOfTimeInWords (addDays d 2) (toTime d) `shouldBe` "2 days from now"

            it "handles months" $ property $ \d -> do
                distanceOfTimeInWords (addDays d (-182)) (toTime d) `shouldBe` "6 months ago"
                distanceOfTimeInWords (addDays d 40) (toTime d) `shouldBe` "1 month from now"

            it "handles years ago" $ property $ \d -> do
                distanceOfTimeInWords (addDays d (-365)) (toTime d) `shouldBe` "12 months ago"
                distanceOfTimeInWords (addDays d (-366)) (toTime d) `shouldBe` "2 years ago"
                distanceOfTimeInWords (addDays d 365) (toTime d) `shouldBe` "12 months from now"
                distanceOfTimeInWords (addDays d 400) (toTime d) `shouldBe` "1 year from now"
                distanceOfTimeInWords (addDays d 710) (toTime d) `shouldBe` "1 year from now"
                distanceOfTimeInWords (addDays d 730) (toTime d) `shouldBe` "2 years from now"

addHours :: T.Day -> Integer -> T.UTCTime
addHours d i = T.UTCTime d (fromInteger $ i * 60 * 60)

addMilliseconds :: T.Day -> Rational -> T.UTCTime
addMilliseconds d i = T.UTCTime d (fromRational $ i / 1000)

addDays :: T.Day -> Integer -> T.UTCTime
addDays d i = T.UTCTime (T.addDays i d) 0

toTime :: T.Day -> T.UTCTime
toTime d = T.UTCTime d (fromInteger 0)
