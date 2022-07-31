module ReTree where
import Project

-- Minimal syntax to make it easy to build a regex tree.
data Re = Eps
        | Chr Char
        | Or [Re]
        | OrChr String
        | Seq [Re]
        | Str String
        | Star Re
    deriving (Eq, Show)

toRegExpCached :: SemiringIndex s => Re -> RegExpCached (Int, Char) s
toRegExpCached Eps = epsilonW
toRegExpCached (Chr chr) = symbolIndex chr
toRegExpCached (Or (r:rs)) = if rs == [] then toRegExpCached r
                                         else alternativeW (toRegExpCached r) (toRegExpCached (Or rs))
toRegExpCached (OrChr (c:cs)) = if cs == [] then toRegExpCached (Chr c)
                                            else alternativeW (toRegExpCached (Chr c)) (toRegExpCached (OrChr cs))
toRegExpCached (Seq (r:rs)) = if rs == [] then toRegExpCached r
                                          else sequenceW (toRegExpCached r) (toRegExpCached (Seq rs))
toRegExpCached (Star r) = repeatedW (toRegExpCached r)
toRegExpCached (Str (c:cs)) = if cs == [] then toRegExpCached (Chr c)
                                          else toRegExpCached (Seq [(Chr c), Str cs])

toLeftmostRe x = toRegExpCached x :: RegExpCached (Int, Char) Leftmost
toLeftLongRe x = toRegExpCached x :: RegExpCached (Int, Char) LeftLong
toBoolRe x = toRegExpCached x :: RegExpCached (Int, Char) Bool
toIntRe x = toRegExpCached x :: RegExpCached (Int, Char) Int
toAllMatchesRe x = toRegExpCached x :: RegExpCached (Int, Char) AllMatches

-- Helper function for below. Should only be used with Star param.
innerOrTransformStar (Star x) = [Seq [x, Star x], Star x]

transformStartingStar :: Re -> Re
transformStartingStar (Star x) = Or $ innerOrTransformStar $ Star x
transformStartingStar (Seq ((Star x):xs)) = Or [transformStartingStar (Seq xs), Seq ([x, Star x] ++ xs)]
transformStartingStar (Or x) = Or $ transformed x where
    transformed ((Star x):xs) = (innerOrTransformStar $ Star x) ++ (transformed xs)
    transformed (x:xs) = [x] ++ (transformed xs) -- Skip elements that aren't Star
    transformed [] = []
transformStartingStar x = x

substr :: Int -> Int -> String -> String
substr a b = (take (b - a + 1)) . (drop a)

findMatches :: Re -> String -> [(Int, String)]
findMatches re str = let matches = submatchW (toAllMatchesRe (transformStartingStar re)) str in
                    case matches of (AllMatches (Matches m)) -> [(i, substr i j str) | (i,j) <- m]
                                    _ -> []
