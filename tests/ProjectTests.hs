import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( Assertion, assertEqual, testCase )
import Project
import ReTree
import qualified Data.Set as Set

splitTests = 
    [ ( "Splits abc", assertEqual "" [("", "abc"), ("a", "bc"), ("ab", "c"), ("abc","")] $ split "abc" )
    , ( "Splits ''", assertEqual "" [("","")] $ split "" )
    ]

partsTests = 
    [ ( "Splits abc into n parts", assertEqual "" [["abc"], ["a", "bc"], ["ab", "c"], ["a", "b", "c"]] $ parts "abc" )
    , ( "Splits ''", assertEqual "" [[]] $ parts "" )
    ]

nocs = Repeated (Alternative (Symbol 'a') (Symbol 'b'))
onec = Sequence nocs (Symbol  'c')
evencs = Sequence (Repeated (Sequence onec onec)) nocs
naiveMatchingTests = 
    [ ( "Naively matches even # of c's", assertEqual "" True $ accept evencs "acc" )
    , ( "Naively matches even # of c's - harder", assertEqual "" True $ accept evencs "accababcccc" )
    , ( "Naively doesn't match even # of c's", assertEqual "" False $ accept evencs "accc" )
    , ( "Naively doesn't match even # of c's - harder", assertEqual "" False $ accept evencs "accababccc" )
    ]

as = Alternative (Symbol 'a') (Repeated (Symbol 'a'))
bs = Alternative (Symbol 'b') (Repeated (Symbol 'b'))
weightedMatchTests = 
    [ ( "Matches with weighted regex (semirings)", assertEqual "" True $ acceptW (weighted (Symbol 'a')) "a" )
    , ( "Matches with weighted regex - harder", assertEqual "" True $ acceptW (weighted evencs) "accababcccc" )
    , ( "Doesn't match with weights", assertEqual "" False $ acceptW (weighted (Symbol 'a')) "b" )
    , ( "Doesn't match with weights - harder", assertEqual "" False $ acceptW (weighted evencs) "accababccc" )
    , ( "Counts match with weights", assertEqual "" 1 $ (acceptW (weighted evencs) "accababcccc" :: Int ) )
    , ( "Counts matches with weights", assertEqual "" 4 $ (acceptW (weighted (Sequence as bs)) "ab" :: Int ) )
    ]

gnocs = RepeatedG (AlternativeG (SymbolG False 'a') (SymbolG False 'b'))
gonec = SequenceG gnocs (SymbolG False 'c')
gevencs = SequenceG (RepeatedG (SequenceG gonec gonec)) gnocs
glushkovMatchingTests =
    [ ( "Matches with Glushkov construction", assertEqual "" True $ match gevencs "acc" )
    , ( "Matches with Glushkov - harder", assertEqual "" True $ match gevencs "accababcccc" )
    , ( "Doesn't match with Glushkov", assertEqual "" False $ match gevencs "accc" )
    , ( "Doesn't match with Glushkov - harder", assertEqual "" False $ match gevencs "accababccc" )
    ]

weightedGlushkovMatchTests = 
    [ ( "Matches with Glushkov and weights (semirings)", assertEqual "" True $ matchW (weightedG (Symbol 'a')) "a" )
    , ( "Matches with Glushkov and weights - harder", assertEqual "" True $ matchW (weightedG evencs) "accababcccc" )
    , ( "Doesn't match with Glushkov and weights", assertEqual "" False $ matchW (weightedG (Symbol 'a')) "b" )
    , ( "Doesn't match with Glushkov and weights - harder", assertEqual "" False $ matchW (weightedG evencs) "accababccc" )
    , ( "Counts match with Glushkov and weights", assertEqual "" 1 $ (matchW (weightedG evencs) "accababcccc" :: Int ) )
    , ( "Counts matches with Glushkov and weights", assertEqual "" 4 $ (matchW (weightedG (Sequence as bs)) "ab" :: Int ) )
    ]

a1 = symbolIndex 'a' :: RegExpCached (Int, Char) Leftmost
ab1 = repeatedW (alternativeW a1 (symbolIndex 'b')) 
aaba1 = sequenceW a1 (sequenceW ab1 a1)
leftmostTests =
    [ ( "Finds leftmost match in string (G + W)", assertEqual "" (Leftmost (Start 3)) $ submatchW aaba1 "...ababa" )
    , ( "Doesn't find any match (G + W)", assertEqual "" NoLeft $ submatchW aaba1 "abbb" )
    , ( "Full string matches (G + W)", assertEqual "" (Leftmost (Start 0)) $ matchGW aaba1 "ababa" )
    , ( "Full string doesn't match (G + W)", assertEqual "" NoLeft $ matchGW aaba1 "abbb" )
    ]

a2 = symbolIndex 'a' :: RegExpCached (Int, Char) LeftLong
ab2 = repeatedW (alternativeW a2 (symbolIndex 'b')) 
aaba2 = sequenceW a2 (sequenceW ab2 a2)
leftlongTests =
    [ ( "Finds longest left match in string (G + W)", assertEqual "" (LeftLong (Range 3 7)) $ submatchW aaba2 "...ababa" )
    , ( "Doesn't find any match (G + W)", assertEqual "" NoLeftLong $ submatchW aaba2 "abbb" )
    , ( "Full string matches (G + W)", assertEqual "" (LeftLong (Range 0 4)) $ matchGW aaba2 "ababa" )
    , ( "Full string doesn't match (G + W)", assertEqual "" NoLeftLong $ matchGW aaba2 "abbb" )
    ]

a3 = symbolIndex 'a' :: RegExpCached (Int, Char) Bool
ab3 = repeatedW (alternativeW a3 (symbolIndex 'b')) 
aaba3 = sequenceW a3 (sequenceW ab3 a3)
boolTests =
    [ ( "Finds a match in string (G + W)", assertEqual "" True $ submatchW aaba3 "...ababa" )
    , ( "Doesn't find any match (G + W)", assertEqual "" False $ submatchW aaba3 "abbb" )
    , ( "Full string matches (G + W)", assertEqual "" True $ matchGW aaba3 "ababa" )
    , ( "Full string doesn't match (G + W)", assertEqual "" False $ matchGW aaba3 "...ababa" )
    ]

a4 = symbolIndex 'a' :: RegExpCached (Int, Char) Int
ab4 = repeatedW (alternativeW a4 (symbolIndex 'b')) 
aaba4 = sequenceW a4 (sequenceW ab4 a4)
intTests =
    [ ( "Counts the matches in string (G + W)", assertEqual "" 3 $ submatchW aaba4 "...ababa" )
    , ( "Doesn't find any match (G + W)", assertEqual "" 0 $ submatchW aaba4 "abbb" )
    , ( "Full string matches (G + W)", assertEqual "" 1 $ matchGW aaba4 "ababa" )
    , ( "Full string doesn't match (G + W)", assertEqual "" 0 $ matchGW aaba4 "...ababa" )
    ]

a5 = symbolIndex 'a' :: RegExpCached (Int, Char) AllMatches
b5 = repeatedW (symbolIndex 'b')
aba5 = sequenceW a5 (sequenceW b5 a5)
ab5 = repeatedW (alternativeW a5 (symbolIndex 'b'))
aaba5 = sequenceW a5 (sequenceW ab5 a5)
allMatchesTests = 
    [ ( "Finds all matches in string (G + W)", assertEqual ""
        (AllMatches (Matches [(1, 3), (3, 5), (8, 11), (15, 19)]))
        $ submatchW aba5 ".ababa..abba...abbba" )
    , ( "Finds all overlapping matches in string (G + W)", assertEqual ""
        (AllMatches (Matches [(1,3), (1,5), (3,5), (8,11), (8,14), (11,14), (8,17), (11,17), (14,17)]))
        $ submatchW aaba5 ".ababa..abbabbabba.." )
    , ( "Doesn't find any match (G + W)", assertEqual "" NoMatches $ submatchW aaba5 "abbb" )
    , ( "Full string matches (G + W)", assertEqual ""
        (AllMatches (Matches [(0, 4)]))
        $ matchGW aaba5 "ababa" )
    , ( "Full string doesn't match (G + W)", assertEqual "" NoMatches $ matchGW aaba5 "abax" )
    , ( "Finds explicit matches in string (G + W)", assertEqual ""
        [(0,"aba"), (0,"ababa"), (2,"aba"), (7,"abba"), (17,"aa")]
        $ findMatches (Seq [Str "a", Star $ OrChr "ab", Str "a"]) "ababa..abba..ab..aa" ) -- a(a|b)*a
    ]

aaba = Seq [(Str "a"), (Star (OrChr "ab")), (Str "a")] -- a(a|b)*a
aabaLeftmost = toLeftmostRe aaba
aabaLeftLong = toLeftLongRe aaba
aabaBool = toBoolRe aaba
aabaInt = toIntRe aaba
aabaAllMatches = toAllMatchesRe aaba
testString = ".ababa..abbabba...abbba"
reTreeTests =
    [ ( "Simple regex works for partial match Leftmost", assertEqual "" (submatchW aaba1 testString) $ (submatchW aabaLeftmost testString) )
    , ( "Works for LeftLong", assertEqual "" (submatchW aaba2 testString) $ (submatchW aabaLeftLong testString) )
    , ( "Works for Bool", assertEqual "" (submatchW aaba3 testString) $ (submatchW aabaBool testString) )
    , ( "Works for Int", assertEqual "" (submatchW aaba4 testString) $ (submatchW aabaInt testString) )
    , ( "Works for AllMatches", assertEqual "" (submatchW aaba5 testString) $ (submatchW aabaAllMatches testString) )
    , ( "Simple regex works for full match Leftmost", assertEqual "" (matchGW aaba1 testString) $ (matchGW aabaLeftmost testString) )
    , ( "Works for LeftLong", assertEqual "" (matchGW aaba2 testString) $ (matchGW aabaLeftLong testString) )
    , ( "Works for Bool", assertEqual "" (matchGW aaba3 testString) $ (matchGW aabaBool testString) )
    , ( "Works for Int", assertEqual "" (matchGW aaba4 testString) $ (matchGW aabaInt testString) )
    , ( "Works for AllMatches", assertEqual "" (matchGW aaba5 testString) $ (matchGW aabaAllMatches testString) )
    ]

perfTests =
    [ ( ">1B ways to match a(a|b)*a on 50k a's", assertEqual ""
        1249975000
        $ submatchW (toIntRe (Seq [Str "a", Star $ OrChr "ab", Str "a"])) (replicate 50000 'a') ) -- a(a|b)*a
    ]

main :: IO ()
main = putStrLn "" >> defaultMain (testGroup "Project Tests" [ allTests ])

buildTestCase :: [(String, Assertion)] -> [TestTree]
buildTestCase = map $ uncurry testCase

allTests :: TestTree
allTests = testGroup "All Tests"
  [ testGroup "Split text into 2 parts" $ buildTestCase splitTests
  , testGroup "Split text into n parts" $ buildTestCase partsTests
  , testGroup "Naive matching" $ buildTestCase naiveMatchingTests
  , testGroup "Weighted matching" $ buildTestCase weightedMatchTests
  , testGroup "Glushkov automaton matching" $ buildTestCase glushkovMatchingTests
  , testGroup "Glushkov automaton weighted matching" $ buildTestCase weightedGlushkovMatchTests
  , testGroup "Leftmost submatching" $ buildTestCase leftmostTests
  , testGroup "Leftlong submatching" $ buildTestCase leftlongTests
  , testGroup "Bool submatching" $ buildTestCase boolTests
  , testGroup "Int submatching" $ buildTestCase intTests
  , testGroup "Returns all matches" $ buildTestCase allMatchesTests
  , testGroup "Simpler regex" $ buildTestCase reTreeTests
  , testGroup "Performance timing" $ buildTestCase perfTests
  ]
