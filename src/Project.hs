module Project where
import qualified Data.Set as Set

data RegExp = Epsilon
            | Symbol Char
            | Alternative RegExp RegExp
            | Sequence RegExp RegExp
            | Repeated RegExp


-- Returns all possible splits (a split is a pair of lists) of the list/str passed.
split :: [a] -> [([a], [a])]
split [] = [([], [])]
split str@(chr:rest) = let splits = split rest in
                         ([], str) : (nonemptySplits chr splits) where

    -- nonemptySplits are all the splits (into 2 parts) that start with the first parameter.
    nonemptySplits :: a -> [([a], [a])] -> [([a], [a])]
    nonemptySplits chr [] = []
    nonemptySplits chr ((s1, s2):otherSplits) = ((chr:s1), s2) : (nonemptySplits chr otherSplits)


-- Return all possible splits into multiple (1 or more) parts. Generalization of split.
parts :: [a] -> [[[a]]]
parts [] = [[]]
parts [chr] = [[[chr]]]
parts (chr:rest) = let splits = parts rest in
                 (nonemptySplits chr splits) where

    -- nonemptySplits are all the splits (into 1+ parts) that start with the first parameter.
    nonemptySplits :: a -> [[[a]]] -> [[[a]]]
    nonemptySplits chr [] = []
    nonemptySplits chr ((p:ps):otherSplits) = [(chr:p):ps, [chr]:(p:ps)] ++ (nonemptySplits chr otherSplits)


accept :: RegExp -> String -> Bool
accept Epsilon str = null str
accept (Symbol s) str = str == [s]
accept (Alternative r1 r2) str = (accept r1 str) || (accept r2 str)
accept (Sequence r1 r2) str = let splits = split str in
                            acceptAny r1 r2 splits where
    acceptAny :: RegExp -> RegExp -> [(String, String)] -> Bool
    acceptAny r1 r2 ((s1, s2):otherSplits) = (accept r1 s1 && accept r2 s2) || (acceptAny r1 r2 otherSplits)
    acceptAny r1 r2 [] = False
accept (Repeated regExp) str = let splits = parts str in
                    acceptAny regExp splits where
    acceptAny :: RegExp -> [[String]] -> Bool
    acceptAny regExp [] = False
    acceptAny regExp (ps:otherSplits) = (acceptAll regExp ps) || (acceptAny regExp otherSplits)
    acceptAll :: RegExp -> [String] -> Bool
    acceptAll regExp [] = True
    acceptAll regExp (p:ps) = (accept regExp p) && (acceptAll regExp ps)


-- Implementation with Semirings (weighted construction)
-----------------------------------------------------------------------
-----------------------------------------------------------------------
-- 0 and 1 must be defined for type s as well as the + and * operations.
class Semiring s where
    zero :: s
    one :: s
    plus :: s -> s -> s
    mult :: s -> s -> s


data RegExpW chr s = EpsilonW
                   | SymbolW (chr -> s)
                   | AlternativeW (RegExpW chr s) (RegExpW chr s)
                   | SequenceW (RegExpW chr s) (RegExpW chr s)
                   | RepeatedW (RegExpW chr s)


sym :: Semiring s => Char -> RegExpW Char s
sym chr = SymbolW (\x -> if x == chr then one else zero)


weighted :: Semiring s => RegExp -> RegExpW Char s
weighted Epsilon = EpsilonW
weighted (Symbol chr) = sym chr
weighted (Alternative r1 r2) = AlternativeW (weighted r1) (weighted r2)
weighted (Sequence r1 r2) = SequenceW (weighted r1) (weighted r2)
weighted (Repeated r) = RepeatedW (weighted r)


acceptW :: Semiring s => RegExpW Char s -> [Char] -> s
acceptW EpsilonW str = if null str then one else zero
acceptW (SymbolW symFunc) str = case str of [chr] -> symFunc chr
                                            _ -> zero
acceptW (AlternativeW r1 r2) str = (acceptW r1 str) `plus` (acceptW r2 str)
acceptW (SequenceW r1 r2) str = let splits = split str in
                                acceptAny r1 r2 splits where
    acceptAny :: Semiring s => (RegExpW Char s) -> (RegExpW Char s) -> [(String, String)] -> s
    acceptAny r1 r2 ((s1, s2):otherSplits) = ((acceptW r1 s1) `mult` (acceptW r2 s2)) `plus` (acceptAny r1 r2 otherSplits)
    acceptAny r1 r2 [] = zero
acceptW (RepeatedW regExp) str = let splits = parts str in
                    acceptAny regExp splits where
    acceptAny :: Semiring s =>( RegExpW Char s) -> [[String]] -> s
    acceptAny regExp [] = zero
    acceptAny regExp (ps:otherSplits) = (acceptAll regExp ps) `plus` (acceptAny regExp otherSplits)
    acceptAll :: Semiring s => (RegExpW Char s) -> [String] -> s
    acceptAll regExp [] = one
    acceptAll regExp (p:ps) = (acceptW regExp p) `mult` (acceptAll regExp ps)


instance Semiring Bool where
    zero = False
    one = True
    plus = (||)
    mult = (&&)

instance Semiring Int where
    zero = 0
    one = 1
    plus = (+)
    mult = (*)


-- Glushkov automaton. Each state is marked to keep track of which states the last 
-- character read has potentially transitioned to. Once that's done, its relatively
-- straight-forward to test for a match.
-----------------------------------------------------------------------
-----------------------------------------------------------------------
data RegExpG = EpsilonG
             | SymbolG Bool Char  -- The symbol is true if the symbol is final
             | AlternativeG RegExpG RegExpG
             | SequenceG RegExpG RegExpG
             | RepeatedG RegExpG


shift :: Bool -> RegExpG -> Char -> RegExpG
shift _ EpsilonG _ = EpsilonG

-- Shifting a symbol 1 character is marked if it matches and it was already marked.
shift marked (SymbolG _ x) chr = SymbolG (marked && (x == chr)) x

shift marked (AlternativeG r1 r2) chr = AlternativeG (shift marked r1 chr) (shift marked r2 chr)

shift marked (SequenceG r1 r2) chr = SequenceG (shift marked r1 chr)
                                               (shift (shouldMark) r2 chr) where
    -- Mark the second part of the sequence if it's already marked and
    -- either the first part was empty or it had a final character.
    shouldMark = marked && (empty r1) || (final r1)

-- Shifting a repeated regex should shift the regex or get marked if it has a final character.
shift marked (RepeatedG r) chr = RepeatedG (shift (marked || final r) r chr)

empty :: RegExpG -> Bool
empty EpsilonG = True
empty (SymbolG _ _) = False
empty (AlternativeG r1 r2) = empty r1 || empty r2
empty (SequenceG r1 r2) = empty r1 && empty r2
empty (RepeatedG r) = True -- Because a* could be 0 instances of a.

final :: RegExpG -> Bool
final EpsilonG = False
final (SymbolG isFinal _) = isFinal
final (AlternativeG r1 r2) = final r1 || final r2
final (SequenceG r1 r2) = (final r1 && empty r2) || final r2
final (RepeatedG r) = final r

match :: RegExpG -> String -> Bool
match r [] = empty r
match r (chr:restString) = final (foldl shiftingOperation startingRegExp restString) where
    -- Shift true with the first char on the regexp to get the starting marked regexp.
    startingRegExp = shift True r chr
    -- On every shift on the rest of the string, shift false as to not mark the first symbols again.
    -- Simply shift the marks that are already there.
    shiftingOperation = (shift False)


-- Glushkov automaton with semirings and cache
-----------------------------------------------------------------------
-----------------------------------------------------------------------
-- This data type is used to cache the values of empty and final.
-- regexpW is the actual implementation (below).
data RegExpCached chr s = RegExpCached {
    emptyW :: s,
    finalW :: s,
    regexpW :: RegExpGW chr s
}

data RegExpGW chr s = EpsilonGW
                    | SymbolGW (chr -> s)
                    | AlternativeGW (RegExpCached chr s) (RegExpCached chr s)
                    | SequenceGW (RegExpCached chr s) (RegExpCached chr s)
                    | RepeatedGW (RegExpCached chr s)

epsilonW :: Semiring s => RegExpCached chr s
epsilonW = RegExpCached {
    emptyW = one,
    finalW = zero,
    regexpW = EpsilonGW
}

symbolW :: Semiring s => (chr -> s) -> RegExpCached chr s
symbolW symFunc = RegExpCached {
    emptyW = zero,
    finalW = zero,
    regexpW = SymbolGW symFunc
}

alternativeW :: Semiring s => RegExpCached chr s -> RegExpCached chr s -> RegExpCached chr s
alternativeW r1 r2 = RegExpCached {
    emptyW = (emptyW r1) `plus` (emptyW r2),
    finalW = (finalW r1) `plus` (finalW r2),
    regexpW = AlternativeGW r1 r2
}

sequenceW :: Semiring s => RegExpCached chr s -> RegExpCached chr s -> RegExpCached chr s
sequenceW r1 r2 = RegExpCached {
    emptyW = (emptyW r1) `mult` (emptyW r2),
    finalW = (finalW r1) `mult` (emptyW r2) `plus` (finalW r2),
    regexpW = SequenceGW r1 r2
}

repeatedW :: Semiring s => RegExpCached chr s -> RegExpCached chr s
repeatedW r = RegExpCached {
    emptyW = one,
    finalW = finalW r,
    regexpW = RepeatedGW r
}

-- Analogous implementation to match above
matchW :: Semiring s => RegExpCached chr s -> [chr] -> s
matchW r [] = emptyW r
matchW r (chr:restString) = finalW (foldl shiftingOperation startingRegExp restString) where
    startingRegExp = shiftW one (regexpW r) chr
    -- Get the regexpW of the RegExpCached and then do shiftW zero on the result.
    shiftingOperation = (shiftW zero . regexpW)

shiftW :: Semiring s => s -> RegExpGW chr s -> chr -> RegExpCached chr s
shiftW _ EpsilonGW _ = epsilonW
-- Update finalW when shifting just as for shift (not shiftW).
shiftW marked (SymbolGW symFunc) chr = (symbolW symFunc) { finalW = marked `mult` (symFunc chr) }

-- The following are identical to shift except for `mult` and `plus` instead of && and ||.
shiftW marked (AlternativeGW r1 r2) chr =
    alternativeW (shiftW marked (regexpW r1) chr) (shiftW marked (regexpW r2) chr)
shiftW marked (SequenceGW r1 r2) chr =
    sequenceW (shiftW marked (regexpW r1) chr)
              (shiftW shouldMark (regexpW r2) chr) where
    shouldMark = marked `mult` (emptyW r1) `plus` (finalW r1)
shiftW marked (RepeatedGW r) chr = repeatedW (shiftW (marked `plus` (finalW r)) (regexpW r) chr)

-- Same implementation as before, updated with the new types.
symG :: Semiring s => Char -> RegExpCached Char s
symG chr = symbolW (\x -> if x == chr then one else zero)

weightedG :: Semiring s => RegExp -> RegExpCached Char s
weightedG Epsilon = epsilonW
weightedG (Symbol chr) = symG chr
weightedG (Alternative r1 r2) = alternativeW (weightedG r1) (weightedG r2)
weightedG (Sequence r1 r2) = sequenceW (weightedG r1) (weightedG r2)
weightedG (Repeated r) = repeatedW (weightedG r)

-- "Heavy Weights" section
-----------------------------------------------------------------------
-----------------------------------------------------------------------
-- matches x*rx* where x is any symbol.
submatchW :: Semiring s => RegExpCached (Int, chr) s -> [chr] -> s
submatchW r s = matchW xrx positionedString where
    xrx = sequenceW x (sequenceW r x)
    x = repeatedW (symbolW (\_ -> one))
    positionedString = zip [0..] s -- Transforms "ab" to [(0,'a'), (1,'b')]

-- The subclass that allows us to obtain the element from an index.
class Semiring s => SemiringIndex s where
    index :: Int -> s

symbolIndex :: SemiringIndex s => Char -> RegExpCached (Int, Char) s
symbolIndex chr = symbolW weight where
    weight (pos, x) | (x == chr) = index pos
                    | otherwise = zero

-- Leftmost
-----------------------------------------------------------------------
data Leftmost = NoLeft | Leftmost Start deriving (Eq, Show)
data Start = NoStart | Start Int deriving (Eq, Show)

instance Semiring Leftmost where
    zero = NoLeft
    one = Leftmost NoStart
    plus NoLeft x = x
    plus x NoLeft = x
    plus (Leftmost x) (Leftmost y) = Leftmost (leftmost x y) where
        leftmost NoStart NoStart = NoStart
        leftmost NoStart (Start i) = Start i
        leftmost (Start i) NoStart = Start i
        leftmost (Start i) (Start j) = Start (min i j)
    mult NoLeft _ = NoLeft
    mult _ NoLeft = NoLeft
    mult (Leftmost x) (Leftmost y) = Leftmost (start x y) where
        start NoStart s = s
        start s _ = s

instance SemiringIndex Leftmost where
    index = Leftmost . Start

-- LeftLong
-----------------------------------------------------------------------
data LeftLong = NoLeftLong | LeftLong Range deriving (Eq, Show)
data Range = NoRange | Range Int Int deriving (Eq, Show)

instance Semiring LeftLong where
    zero = NoLeftLong
    one = LeftLong NoRange
    plus NoLeftLong x = x
    plus x NoLeftLong = x
    plus (LeftLong x) (LeftLong y) = LeftLong (leftLong x y) where
        leftLong NoRange NoRange = NoRange
        leftLong NoRange (Range i j) = Range i j
        leftLong (Range i j) NoRange = Range i j
        -- Select the first range if the match is earlier or if they're same length.
        leftLong (Range i j) (Range k l) | i < k || (i == k && j >= l) = Range i j
                                         | otherwise = Range k l
    mult NoLeftLong _ = NoLeftLong
    mult _ NoLeftLong = NoLeftLong
    mult (LeftLong x) (LeftLong y) = LeftLong (range x y) where
        range NoRange s = s
        range s NoRange = s
        range (Range i _) (Range _ j) = Range i j

instance SemiringIndex LeftLong where
    index i = LeftLong (Range i i)

-- AllMatches, custom implementation to list all matches
-----------------------------------------------------------------------
data AllMatches = NoMatches | AllMatches Matches deriving (Eq, Show)
data Matches = NoMatchesStart | Matches (Set.Set (Int, Int)) deriving (Eq, Show)

instance Semiring AllMatches where
    zero = NoMatches
    one = AllMatches NoMatchesStart
    plus NoMatches x = x
    plus x NoMatches = x
    plus (AllMatches x) (AllMatches y) = AllMatches (allMatches x y) where
        allMatches NoMatchesStart NoMatchesStart = NoMatchesStart
        allMatches NoMatchesStart (Matches i) = Matches i
        allMatches (Matches i) NoMatchesStart = Matches i
        allMatches (Matches i) (Matches j) = Matches (Set.union i j)
    mult NoMatches _ = NoMatches
    mult _ NoMatches = NoMatches
    mult (AllMatches x) (AllMatches y) = AllMatches (matches x y) where
        matches NoMatchesStart s = s
        matches s NoMatchesStart = s
        matches (Matches i) (Matches j) = Matches (join i j)
        join i j = Set.fromList (pairUp (Set.elems i) (Set.elems j))
        pairUp ((i1, _):is) ((_, j2):js) = [(i1, j2)] ++ (pairUp is js)
        pairUp _ _ = []

instance SemiringIndex AllMatches where
    index i = AllMatches (Matches (Set.fromList [(i, i)]))
