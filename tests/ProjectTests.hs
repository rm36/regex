import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( Assertion, assertEqual, testCase )
import Project
import qualified Data.Set as Set

splitTests = 
    [ ( "returnsAllSplits", assertEqual "" [("", "abc"), ("a", "bc"), ("ab", "c"), ("abc","")] $ split "abc" )
    , ( "splitsEmpty", assertEqual "" [("","")] $ split "" )
    ]

partsTests = 
    [ ( "returnsAllParts", assertEqual "" [["abc"], ["a", "bc"], ["ab", "c"], ["a", "b", "c"]] $ parts "abc" )
    , ( "partsEmpty", assertEqual "" [[]] $ parts "" )
    ]

nocs = Repeated (Alternative (Symbol 'a') (Symbol 'b'))
onec = Sequence nocs (Symbol  'c')
evencs = Sequence (Repeated (Sequence onec onec)) nocs
naiveMatchingTests = 
    [ ( "naiveMatchingEvenNumberOfCs", assertEqual "" True $ accept evencs "acc" )
    ,( "naiveMatchingHardEvenNumberOfCs", assertEqual "" True $ accept evencs "accababcccc" )
    ,( "naiveMatchingFailEvenNumberOfCs", assertEqual "" False $ accept evencs "accc" )
    ,( "naiveMatchingHardFailEvenNumberOfCs", assertEqual "" False $ accept evencs "accababccc" )
    ]

as = Alternative (Symbol 'a') (Repeated (Symbol 'a'))
bs = Alternative (Symbol 'b') (Repeated (Symbol 'b'))
weightedMatchTests = 
    [ ( "weightedMatchSymbol", assertEqual "" True $ acceptW (weighted (Symbol 'a')) "a" )
    , ( "weightedNonMatchSymbol", assertEqual "" False $ acceptW (weighted (Symbol 'a')) "b" )
    , ( "weightedNonMatchHard", assertEqual "" False $ acceptW (weighted evencs) "accababccc" )
    , ( "weightedMatchHard", assertEqual "" True $ acceptW (weighted evencs) "accababcccc" )
    , ( "weightedMatchIntHard", assertEqual "" 1 $ (acceptW (weighted evencs) "accababcccc" :: Int ) )
    , ( "weightedMatchIntMultiple", assertEqual "" 4 $ (acceptW (weighted (Sequence as bs)) "ab" :: Int ) )
    ]

gnocs = RepeatedG (AlternativeG (SymbolG False 'a') (SymbolG False 'b'))
gonec = SequenceG gnocs (SymbolG False 'c')
gevencs = SequenceG (RepeatedG (SequenceG gonec gonec)) gnocs
glushkovMatchingTests =
    [ ( "glushkovMatchingEvenNumberOfCs", assertEqual "" True $ match gevencs "acc" )
    ,( "glushkovMatchingHardEvenNumberOfCs", assertEqual "" True $ match gevencs "accababcccc" )
    ,( "glushkovMatchingFailEvenNumberOfCs", assertEqual "" False $ match gevencs "accc" )
    ,( "glushkovMatchingHardFailEvenNumberOfCs", assertEqual "" False $ match gevencs "accababccc" )
    ]

weightedGlushkovMatchTests = 
    [ ( "weightedGlushkovMatchSymbol", assertEqual "" True $ matchW (weightedG (Symbol 'a')) "a" )
    , ( "weightedGlushkovNonMatchSymbol", assertEqual "" False $ matchW (weightedG (Symbol 'a')) "b" )
    , ( "weightedGlushkovNonMatchHard", assertEqual "" False $ matchW (weightedG evencs) "accababccc" )
    , ( "weightedGlushkovMatchHard", assertEqual "" True $ matchW (weightedG evencs) "accababcccc" )
    , ( "weightedGlushkovMatchIntHard", assertEqual "" 1 $ (matchW (weightedG evencs) "accababcccc" :: Int ) )
    , ( "weightedGlushkovMatchIntMultiple", assertEqual "" 4 $ (matchW (weightedG (Sequence as bs)) "ab" :: Int ) )
    ]

a1 = symbolIndex 'a' :: RegExpCached (Int, Char) Leftmost
ab1 = repeatedW (alternativeW a1 (symbolIndex 'b')) 
aaba1 = sequenceW a1 (sequenceW ab1 a1)
leftmostTests =
    [ ( "leftmostSubmatch", assertEqual "" (Leftmost (Start 3)) $ submatchW aaba1 "...ababa" )
    , ( "leftmostNoSubmatch", assertEqual "" NoLeft $ submatchW aaba1 "abbb" )
    ]

a2 = symbolIndex 'a' :: RegExpCached (Int, Char) LeftLong
ab2 = repeatedW (alternativeW a2 (symbolIndex 'b')) 
aaba2 = sequenceW a2 (sequenceW ab2 a2)
leftlongTests =
    [ ( "leftlongSubmatch", assertEqual "" (LeftLong (Range 3 7)) $ submatchW aaba2 "...ababa" )
    , ( "leftlongNoSubmatch", assertEqual "" NoLeftLong $ submatchW aaba2 "abbb" )
    ]

a3 = symbolIndex 'a' :: RegExpCached (Int, Char) AllMatches
b3 = repeatedW (symbolIndex 'b')
aba3 = sequenceW a3 (sequenceW b3 a3)
ab3 = repeatedW (alternativeW a3 (symbolIndex 'b'))
aaba3 = sequenceW a3 (sequenceW ab3 a3)
allMatchesTests = 
    [ ( "allMatchesNonOverlapping", assertEqual ""
        (AllMatches (Matches (Set.fromList [(1, 3), (3, 5), (8, 11), (15, 19)])))
        $ submatchW aba3 ".ababa..abba...abbba" )
    , ( "allMatchesOverlapping", assertEqual ""
        (AllMatches (Matches (Set.fromList [(1, 3), (1, 5), (8, 11), (8,14), (18, 22)])))
        $ submatchW aaba3 ".ababa..abbabba...abbba" )
    , ( "allMatchesNoSubmatch", assertEqual "" NoMatches $ submatchW aaba3 "abbb" )
    ]

main :: IO ()
main = putStrLn "" >> defaultMain (testGroup "Project Tests" [ unitTests ])

buildTestCase :: [(String, Assertion)] -> [TestTree]
buildTestCase = map $ uncurry testCase

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testGroup "Split text into 2 parts" $ buildTestCase splitTests
  , testGroup "Split text into n parts" $ buildTestCase partsTests
  , testGroup "Naive matching" $ buildTestCase naiveMatchingTests
  , testGroup "Weighted matching" $ buildTestCase weightedMatchTests
  , testGroup "Glushkov automaton matching" $ buildTestCase glushkovMatchingTests
  , testGroup "Glushkov automaton weighted matching" $ buildTestCase weightedGlushkovMatchTests
  , testGroup "Leftmost submatching" $ buildTestCase leftmostTests
  , testGroup "Leftlong submatching" $ buildTestCase leftlongTests
  , testGroup "Returns all matches" $ buildTestCase allMatchesTests
  ]
