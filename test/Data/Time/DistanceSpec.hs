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
            it "handles hours in the past" $ property $ \d ->
                distanceOfTimeInWords (addHours d (-2)) (toTime d) `shouldBe` "2 hours ago"

            it "handles hours in the future" $ property $ \d ->
                distanceOfTimeInWords (addHours d 2) (toTime d) `shouldBe` "2 hours from now"

            it "handles days in the future" $ property $ \d ->
                distanceOfTimeInWords (addDays d 2) (toTime d) `shouldBe` "2 days from now"

            it "handles days in the past" $ property $ \d ->
                distanceOfTimeInWords (addDays d (-2)) (toTime d) `shouldBe` "2 days ago"

            it "handles years ago" $ property $ \d -> do
                distanceOfTimeInWords (addDays d (-365)) (toTime d) `shouldBe` "12 months ago"
                distanceOfTimeInWords (addDays d (-366)) (toTime d) `shouldBe` "2 years ago"

            it "handles years from now" $ property $ \d ->
                distanceOfTimeInWords (addDays d 365) (toTime d) `shouldBe` "12 months from now"

            it "handles years from now" $ property $ \d -> do
                distanceOfTimeInWords (addDays d 400) (toTime d) `shouldBe` "1 year from now"
                distanceOfTimeInWords (addDays d 710) (toTime d) `shouldBe` "1 year from now"
                distanceOfTimeInWords (addDays d 730) (toTime d) `shouldBe` "2 years from now"

addHours :: T.Day -> Integer -> T.UTCTime
addHours d i = T.UTCTime d (fromInteger $ i * 60 * 60)

addDays :: T.Day -> Integer -> T.UTCTime
addDays d i = T.UTCTime (T.addDays i d) 0

toTime :: T.Day -> T.UTCTime
toTime d = T.UTCTime d (fromInteger 0)
