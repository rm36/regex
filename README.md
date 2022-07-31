# Implementation of efficient regex matching in Haskell
by [Rodrigo Mendoza](mailto:rm36@illinois.edu)

## Overview
One of the lessons in CS-421 has been how to translate a regular expression to a non-deterministic finite automaton using Thompson's construction. We have also delved into how to do LL(n) and LR Parsing. Another construction that's often mentioned in formal language theory is Glushkov Construction Algorithm, which has been shown by Sakarovitch to be equivalent to Thompson's when the ε-transitions are removed. This project focuses on an extended version of Glushkov's automaton.

### Research Paper
I implemented the Glushkov Construction Algorithm by using Haskell, using an automaton with weights; extended to work with Semirings, a mathematical abstraction that abstracts addition ⊕ and multiplication ⊗ operations.

The paper I based my implementation on is the following. Note that I only read through the first 10 pages of the paper (Acts I and II), because Act III is an extension for a non context-free example.
Sebastian Fischer, Frank Huch, and Thomas Wilke. 2010. A play on regular expressions: functional pearl. SIGPLAN Not. 45, 9 (September 2010), 357–368. https://doi.org/10.1145/1932681.1863594
And here is a link to the PDF: http://sebfisch.github.io/haskell-regexp/regexp-play.pdf

Despite the play-like format of the paper, the authors explain complex ideas that build on abstract mathematics. They detail how to implement the algorithm in a concise manner with Haskell, using weights/semirings to extend the implementation.
## Implementation
### Major capabilities
The major capabilities of the project are implementations of regex using:
- A. All possible splits where needed.
- B. All possible splits where needed, generalized with semirings.
- C. Glushkov automaton.
- D. Glushkov automaton with semirings and a cache.
- E. Extensions for custom applications.

To prove this is my own work and not a simple transcription, I implemented everything from scratch with better function names to slightly improve readability, adding extra functions, tests, and adding comments on every section so it is easier to understand than the snippets in the paper (which come next to the explanation). For example, the implementation of split and parts included a pipe notation that I wasn't familiar with, so I instead implemented it using helper functions. Afterwards, once I learned its usage, I applied it to simplify the AllMatches implementation.

### Components
The components in the implementation are the following:
- Fully working implementation of the paper behavior from scratch, the major capabilities listed above from points A to D.
- An extension of the Glushkov automaton with semirings and cache for Bool, Int, and AllMatches types, where - AllMatches is an instance of a Semiring where ⊕ and ⊗ are defined to add matches to an array.
- A helper Regex tree definition and transformation functions to simplify the usage. These are found in the ReTree.hs file. For example, to represent the regex `ab(c|d)*`, it would have to be typed as: `Seq (Sym 'a') (Seq (Sym 'b') (Rep (Or (Sym 'c') (Sym 'd'))))`, but with the helper class it could be: `Seq [Str "ab", Star $ OrChr "cd"]`. The helper functions convert from this data type to the other implementations, and transform the output to see matches as strings instead of only indexes.
- Tests as explained in the section below.

### Status of the project
The project works great for detecting matches on every scenario that can be represented with the Regex type, with exception of regexes that start with a Kleene Star for submatches. The code of why this fails is commented in a note in the code, and it happens because when the first character is matched, there's no state which saves the possibility that there isn't a match, so matching the regex `a*a` in "aa" will only result in `[(0, "a"), (0, "aa")]` without returning `(1, "a")`. To fix this issue, I wrote a transformation of every regex tree that has a Kleene Star at the beginning to change explicitly `a*` to `aa*|a*`, such that the first explicit a can be matched at a later index. With this behavior change there is no difference with the proposal.

To test out the project, one can initialize the project with stack init, then run stack ghci and call findMatches with the regex and the string to match the parts to. The result will be a list of indexes where the matches happened, paired with the actual strings matched. We can see in the examples below how the matches work as expected with regexes `a(a|b)a*` and `aa*`, and after transforming regexes that start with a Kleene Star such as `a*` and `b*ab*`, they work properly.
I tried multiple ways of extending the semiring operations, and the implementation for AllMatches ended up being the cleanest and most useful of all.

I recommend the reader to try out any match function implemented with any regex. I spent many hours verifying that they work as expected.

``` haskell
ghci> findMatches (Seq [Str "a", Star $ OrChr "ab", Str "a"]) "ababa..abba..ab..aa" -- a(a|b)*a
[(0,"aba"),(0,"ababa"),(2,"aba"),(7,"abba"),(17,"aa")]
ghci> findMatches (Seq [Star $ Str "b", Str "a", Star $ Str "b"]) "bbabb" -- b*ab*
[(0,"bba"),(1,"ba"),(2,"a"),(0,"bbab"),(1,"bab"),(2,"ab"),(0,"bbabb"),(1,"babb"),(2,"abb")]
ghci> findMatches (Seq [Star $ Str "a", Str "a"]) "..aaaa.." -- a*a
[(2,"a"),(2,"aa"),(3,"a"),(2,"aaa"),(3,"aa"),(4,"a"),(2,"aaaa"),(3,"aaa"),(4,"aa"),(5,"a")]
```

### Comparative with proposal
The end result works just as expected with the exception of regexes starting with a Kleene Star subsection, as mentioned above. The main difference with the proposal was that it took more time than expected. Some issues were unexpected and required deeper understanding than just reading the paper. Concretely, in the proposal I thought it could be possible to print "almost" matches, but that would've required a significant change in how matching happens, rather than just instancing another Semiring type.

## Tests
The full list of tests can be found below, and there are a couple of things to notice: Naive and Naive-weighted (semiring implementation) matching are the slowest of the functions, because they expand on all possible splits of strings. These sections are the only ones that have tests that take 0.01 seconds or more.
The Glushkov automaton implementation is much faster, especially with the cache.
There is one performance test, which runs the regex a(a|b)a* on a string composed of 50,000 a's. There are more than 1 billion matches, and the runtime is close to 1 second on a laptop with an 8th Gen Intel Core i5 processor.

```
λ stack test --test-arguments "-p /Performance/"
cs421-final-project> test (suite: test, args: -p /Performance/)

Project Tests
  All Tests
    Performance timing
      >1B ways to match a(a|b)*a on 50k a's: OK (1.05s)

All 1 tests passed (1.05s)

cs421-final-project> Test suite test passed
```

Finally, it's important to note that the tests for the correctness of the transformation between tree representations of regexes with cache were complicated to implement because each symbol has a function that takes in a character and returns one if it matches the character. Therefore, the tests in `Simpler regex` were done indirectly by their (sub)match behaviors rather than on testing for equality on their data representation.

Here are all the tests:
Below is a list of the tests ran:
```
  All Tests                                                         
    Split text into 2 parts                                         
      Splits abc:                                       OK          
      Splits '':                                        OK          
    Split text into n parts                                         
      Splits abc into n parts:                          OK          
      Splits '':                                        OK          
    Naive matching                                                  
      Naively matches even # of c's:                    OK          
      Naively matches even # of c's - harder:           OK (0.03s)  
      Naively doesn't match even # of c's:              OK          
      Naively doesn't match even # of c's - harder:     OK (0.01s)  
    Weighted matching                                               
      Matches with weighted regex (semirings):          OK          
      Matches with weighted regex - harder:             OK (0.10s)  
      Doesn't match with weights:                       OK          
      Doesn't match with weights - harder:              OK (0.06s)  
      Counts match with weights:                        OK (0.95s)  
      Counts matches with weights:                      OK          
    Glushkov automaton matching                                     
      Matches with Glushkov construction:               OK          
      Matches with Glushkov - harder:                   OK          
      Doesn't match with Glushkov:                      OK          
      Doesn't match with Glushkov - harder:             OK          
    Glushkov automaton weighted matching                            
      Matches with Glushkov and weights (semirings):    OK          
      Matches with Glushkov and weights - harder:       OK          
      Doesn't match with Glushkov and weights:          OK          
      Doesn't match with Glushkov and weights - harder: OK          
      Counts match with Glushkov and weights:           OK          
      Counts matches with Glushkov and weights:         OK          
    Leftmost submatching                                            
      Finds leftmost match in string (G + W):           OK          
      Doesn't find any match (G + W):                   OK          
      Full string matches (G + W):                      OK          
      Full string doesn't match (G + W):                OK          
    Leftlong submatching                                            
      Finds longest left match in string (G + W):       OK          
      Doesn't find any match (G + W):                   OK          
      Full string matches (G + W):                      OK          
      Full string doesn't match (G + W):                OK          
    Bool submatching                                                
      Finds a match in string (G + W):                  OK          
      Doesn't find any match (G + W):                   OK          
      Full string matches (G + W):                      OK          
      Full string doesn't match (G + W):                OK          
    Int submatching                                                 
      Counts the matches in string (G + W):             OK          
      Doesn't find any match (G + W):                   OK          
      Full string matches (G + W):                      OK          
      Full string doesn't match (G + W):                OK          
    Returns all matches                                             
      Finds all matches in string (G + W):              OK          
      Finds all overlapping matches in string (G + W):  OK          
      Doesn't find any match (G + W):                   OK          
      Full string matches (G + W):                      OK          
      Full string doesn't match (G + W):                OK          
      Finds explicit matches in string (G + W):         OK          
    Simpler regex                                                   
      Simple regex works for partial match Leftmost:    OK          
      Works for LeftLong:                               OK          
      Works for Bool:                                   OK          
      Works for Int:                                    OK          
      Works for AllMatches:                             OK          
      Simple regex works for full match Leftmost:       OK          
      Works for LeftLong:                               OK          
      Works for Bool:                                   OK          
      Works for Int:                                    OK          
      Works for AllMatches:                             OK          
    Transform starting Kleene star                                  
      a* is transformed to aa*|a*:                      OK          
      a*b is transformed to b|aa*b:                     OK          
      ab is not transformed:                            OK          
      a|b* is transformed to a|bb*|b*:                  OK          
    Performance timing                                              
      >1B ways to match a(a|b)*a on 50k a's:            OK (1.27s)  

    All 61 tests passed (1.28s)

cs421-final-project> Test suite test passed                         
```

