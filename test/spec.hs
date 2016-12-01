import Test.Hspec
import Pokerhand


main = hspec $ do
  describe "the game of poker" $ do
    let jack = Card Jack Diamond
    let queen = Card Queen Diamond
    let king = Card King Diamond
    let ace = Card Ace Diamond
    let ten = Card X Diamond
    let nine = Card IX Clubs
    let eight = Card VIII Diamond

--First off, we need to make this work.
--Hint : try `:i shouldBe` a stack ghci

    describe "the cards" $ do
      it "cards are made of a suit and a rank" $ do
        jack `shouldBe` jack

--Here are plenty more tests that should pass :)

      it "There is an order in rank" $ do
        queen > jack `shouldBe` True
      it "There is no order in suits" $ do
        Card Jack Diamond > Card Jack Clubs `shouldBe` False
        Card Jack Diamond < Card Jack Clubs `shouldBe` False
        Card Jack Diamond == Card Jack Clubs `shouldBe` True

    describe "hands" $ do
      it "hands are FIVE cards" $ do
        let queens = Hand queen queen queen queen queen
        queens `shouldBe` queens
      describe "hands are comparable" $ do
        it "card order does not matter" $ do
          let jqtnk = Hand jack ten nine king ace
          let kqjtn = Hand ace king jack ten nine
          jqtnk < kqjtn `shouldBe` False
          jqtnk > kqjtn `shouldBe` False
          jqtnk == kqjtn `shouldBe` True

        describe "high card" $ do
          it "a hand with the highest card should be higher" $ do
            let high = Hand ace king jack ten nine
            let low = Hand king queen ten nine eight
            high > low `shouldBe` True
          it "if high cards are equal, compare the next one" $ do
            let high = Hand ace king queen nine eight
            let low = Hand ace king jack ten nine
            high > low `shouldBe` True

--        describe "pairs: two cards of same rank are a pair" $ do
--          it "pairs are stronger than high card" $ do
--            let akjt9 = Hand ace king jack ten nine
--            let kjt99 = Hand nine nine king jack ten
--            kjt99 > akjt9 `shouldBe` True
--          it "pairs are ordered by the rank of the pair cards" $ do
--            let qjtt9 = Hand queen jack ten ten nine
--            let kjt99 = Hand nine nine king jack ten
--            qjtt9 > kjt99 `shouldBe` True
--          it "when pairs are equal, revert to high card rule" $ do
--            let qjt99 = Hand queen jack ten nine nine
--            let kjt99 = Hand nine nine king jack ten
--            kjt99 > qjt99 `shouldBe` True

--        describe "double-pairs : two different pairs in a hand" $ do
--          it "double pairs are stronger than pairs" $ do
--            let tt998 = Hand ten ten nine nine eight
--            let aakq9 = Hand ace ace king queen nine
--            tt998 > aakq9 `shouldBe` True
--          it "double pairs are ordered by rank of high pair" $ do
--            let aa998 = Hand ace ace nine nine eight
--            let kkqq9 = Hand king king queen queen nine
--            aa998 > kkqq9 `shouldBe` True
--          it "if high pair is equal, order by rank of low pair" $ do
--            let aa998 = Hand ace ace nine nine eight
--            let aa988 = Hand ace ace nine eight eight
--            aa998 > aa988 `shouldBe` True
--          it "if both pairs are equal, compare the last card" $ do
--            let aa99q = Hand ace ace nine nine queen
--            let aa998 = Hand ace ace nine nine eight
--            aa99q > aa998 `shouldBe` True

--        describe "three of a kind : three cards of same rank" $ do
--          it "three-of-a-kind are better than a double pair" $ do
--            let aaaq9 = Hand ace ace ace queen nine
--            let aakk9 = Hand ace ace king king nine
--            aaaq9 > aakk9 `shouldBe` True
--          it "three-of-a-kind are ordered by order of the triple" $ do
--            let aaaq9 = Hand ace ace ace queen nine
--            let a9888 = Hand ace nine eight eight eight
--            aaaq9 > a9888 `shouldBe` True
--          it "if both triples are equal, order by high card left" $ do
--            let aaak9 = Hand ace ace ace king nine
--            let aaat9 = Hand ace ace ace ten nine
--            aaak9 > aaat9 `shouldBe` True
--
--        describe "color : all cards have same suit" $ do
--          it "colors are better than a three-of-a-kind" $ do
--            let aaak9 = Hand ace ace ace king nine
--            let color = Hand ace king queen ten eight -- all diamonds
--            color > aaak9 `shouldBe` True
--          it "colors are compared by high card" $ do
--            let highcolor = Hand ace king queen jack ten --diamonds
--            let color     = Hand ace king jack ten eight --diamonds
--            highcolor > color `shouldBe` True
