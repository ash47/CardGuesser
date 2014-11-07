{-
By Ashley Schmid (aschmid, 584770)

This file contains the Cardguess module which is used to guess a set of cards


My method of guessing is as follow:
 - Generate a list of every possible guess
 - Repeat:
    - Pick one of the elements from the list and make that guess
    - Remove any inconsistant guesses from the guess list
-}

module Cardguess (initialGuess, nextGuess, GameState) where
import Card

{-
    The 'GameState' type is used to keep track of the current game state.
    It simply contains a list of all the guesses left to make.
-}
type GameState = [[Card]]

{-
    The 'initialGuess' function creates our inital GameState version, and
        makes our initial guess.
-}
initialGuess :: Int -> ([Card],GameState)
initialGuess n
    | n == 2    = ([Card Heart R5, Card Diamond R10], initalGameState)
    | otherwise = (head initalGameState, tail initalGameState)
    where
    initalGameState = buildGameState n

{-
    The 'nextGuess' function processes the result of a given guess, removing
        impossible guesses from the GameState, then returning the next guess
-}
nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess (guess,gameState)
    (totalCorrect,totalLower,shared,totalHigher,correctSuits) =
    (head newGameState, tail newGameState)
    where
        -- Find the lowest and highest ranking cards in our guess
        low = minimumRank guess
        high = maximumRank guess

        -- Apply the filters

        -- Ensure the number of correct cards is consistant
        newGS1 = filter (correctFilter totalCorrect guess) gameState

        -- Ensures the number of suits in our guesses is consistant
        newGS2 = filter (suitFilter correctSuits guess) newGS1

        -- Ensures the number of cards lower than is consistant
        newGS3 = filter (lowerFilter low totalLower) newGS2

        -- Ensures the number of cards higher than is consistant
        newGS4 = filter (upperFilter high totalHigher) newGS3

        -- Ensures the number shared is consistant
        newGameState = filter (rankFilter shared guess) newGS4

{-
    The 'buildGameState' function generates a GameState containing every
        possible guess.
-}
buildGameState :: Int -> GameState
buildGameState n = filter dupFilter initalGameState
    where
    initalGameState = sequence (buildSequenceable n)

{-
    The 'buildSequenceable' function builds a list of n lists, the inner lists
        contain every possible card. This can be used with the sequence
        function to generate every permutation of cards.
-}
buildSequenceable :: Int -> [[Card]]
buildSequenceable 1 = [allCards]
buildSequenceable n = allCards:buildSequenceable (n-1)

-- Contains a list of all the possible cards
allCards = [minBound..maxBound]

{-
    The 'dupFilter' function returns False if a given list contains the same
        element more than once, returning True if the list is unique.
-}
dupFilter :: Eq a => [a] -> Bool
dupFilter []        = True
dupFilter (x:xs)
    | xs == []      = True
    | x == head xs  = False
    | otherwise     = not (x `elem` xs) && dupFilter xs

{-
    The 'correctFilter' function checks if at least the given number of
        elements are the same in the two lists
-}
correctFilter :: Eq a => Int -> [a] -> [a] -> Bool
correctFilter n guess compare = sharedElementCount guess compare >= n

{-
    The 'sharedElementCount' function returns the number of elements that
        are the same, assuming there are NO DUPLICATES!
-}
sharedElementCount :: Eq a => [a] -> [a] -> Int
sharedElementCount _ []         = 0
sharedElementCount [] _         = 0
sharedElementCount (x:xs) ys    =
    if x == head ys
        then 1 + sharedElementCount xs (tail ys)
        else sharedElementCount (x:xs) (tail ys) +
            sharedElementCount xs [head ys]

{-
    The 'suitFilter' function checks if at least the given number of
        suits are in the given Card list
-}
suitFilter :: Int -> [Card] -> [Card] -> Bool
suitFilter n guess compare
    | n == 0    = sharedTotal == 0
    | otherwise = sharedTotal == n
    where sharedTotal = sharedSuits guess compare

{-
    The 'sharedSuits' function returns how many suits two Card lists
        have in common
-}
sharedSuits :: [Card] -> [Card] -> Int
sharedSuits xs ys = compareSuitTupple (suitCount xs) (suitCount ys)

{-
    The 'compareSuitTupple' function returns how many suits in the two
        given suit tupples are the same

    A suit tupple is just a tupple of 4 Ints, each coorosponding to
        a certain suit
-}
compareSuitTupple :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Int
compareSuitTupple (a, b, c, d) (e, f, g, h) =
    (min a e) + (min b f) + (min c g) + (min d h)

{-
    The 'suitCount' function counts the number of each suit in the
        given Card list
-}
suitCount :: [Card] -> (Int, Int, Int, Int)
suitCount []        = (0, 0, 0, 0)
suitCount (x:xs)    = addSuitTupple (suitTupple x) (suitCount xs)

{-
    The 'suitTupple' function creates a tupple containing info on which suit
        the given card is.
-}
suitTupple :: Card -> (Int, Int, Int, Int)
suitTupple x =
    if suit x == Club
        then (1, 0, 0, 0)
        else
            if suit x == Diamond
            then (0, 1, 0, 0)
            else
                if suit x == Heart
                    then (0, 0, 1, 0)
                    else (0, 0, 0, 1)

{-
    The 'addSuitTupple' function adds two tupples containing suit data
-}
addSuitTupple :: (Int, Int, Int, Int) ->
    (Int, Int, Int, Int) -> (Int, Int, Int, Int)
addSuitTupple (a, b, c, d) (e, f, g, h) = (a+e, b+f, c+g, d+h)

{-
    The 'lowerFilter' function return True when the total number of cards
        in the given list with a rank lower than the given rank is equal
        to the given number
-}
lowerFilter :: Rank -> Int -> [Card] -> Bool
lowerFilter r lowerCards xs = lowerCards == (totalLowerThan r xs)

{-
    The 'upperFilter' function return True when the total number of cards
        in the given list with a rank lower than the given rank is equal
        to the given number
-}
upperFilter :: Rank -> Int -> [Card] -> Bool
upperFilter r upperCards xs = upperCards == (totalHigherThan r xs)

{-
    The 'totalLowerThan' function returns the number of cards in the given
        list that are lower than the given rank
-}
totalLowerThan :: Rank -> [Card] -> Int
totalLowerThan r []     = 0
totalLowerThan r (x:xs) =
    if rank x < r
        then 1 + (totalLowerThan r xs)
        else 0 + (totalLowerThan r xs)

{-
    The 'totalHigherThan' function returns the number of cards in the given
        list that are higher than the given rank
-}
totalHigherThan :: Rank -> [Card] -> Int
totalHigherThan r []        = 0
totalHigherThan r (x:xs)    =
    if rank x > r
        then 1 + (totalHigherThan r xs)
        else 0 + (totalHigherThan r xs)

{-
    The 'minimumRank' function finds the lowest ranking card in the
        given set of cards

    This function assumes the Card list is never empty
-}
minimumRank :: [Card] -> Rank
minimumRank (x:xs)
    | xs == []                  = rank x
    | rank x < rank (head xs)   = minimumRank (x:(tail xs))
    | otherwise                 = minimumRank xs

{-
    The 'maximumRank' function finds the lowest ranking card in the
        given set of cards

    This function assumes the Card list is never empty
-}
maximumRank :: [Card] -> Rank
maximumRank (x:xs)
    | xs == []                  = rank x
    | rank x > rank (head xs)   = maximumRank (x:(tail xs))
    | otherwise                 = maximumRank xs



{-
    The 'totalRankOf' function returns the total number of cards with
        the given rank
-}
totalRankOf :: Rank -> [Card] -> Int
totalRankOf r [] = 0
totalRankOf r (x:xs) =
    if r == rank x
        then 1 + (totalRankOf r xs)
        else 0 + (totalRankOf r xs)

{-
    The 'rankFilter' function returns True if the two lists have the
        given number of cards in common
-}
rankFilter :: Int -> [Card] -> [Card] -> Bool
rankFilter shared xs ys = (totalShared xs ys) == shared

{-
    The 'totalShared' function returns how many Cards in the given lists
        are common between the two, assuming no duplicate Cards
-}
totalShared :: [Card] -> [Card] -> Int
totalShared [] _    = 0
totalShared _ []    = 0
totalShared xs ys =
    -- Add the sum of minimal number of total shared ranks
    (min x1 y1) +
    (min x2 y2) +
    (min x3 y3) +
    (min x4 y4) +
    (min x5 y5) +
    (min x6 y6) +
    (min x7 y7) +
    (min x8 y8) +
    (min x9 y9) +
    (min x10 y10) +
    (min x11 y11) +
    (min x12 y12) +
    (min x13 y13)

    -- Workout the shared rankings
    where
    x1  = totalOfRank Ace xs
    x2  = totalOfRank R2 xs
    x3  = totalOfRank R3 xs
    x4  = totalOfRank R4 xs
    x5  = totalOfRank R5 xs
    x6  = totalOfRank R6 xs
    x7  = totalOfRank R7 xs
    x8  = totalOfRank R8 xs
    x9  = totalOfRank R9 xs
    x10 = totalOfRank R10 xs
    x11 = totalOfRank Jack xs
    x12 = totalOfRank Queen xs
    x13 = totalOfRank King xs

    y1  = totalOfRank Ace ys
    y2  = totalOfRank R2 ys
    y3  = totalOfRank R3 ys
    y4  = totalOfRank R4 ys
    y5  = totalOfRank R5 ys
    y6  = totalOfRank R6 ys
    y7  = totalOfRank R7 ys
    y8  = totalOfRank R8 ys
    y9  = totalOfRank R9 ys
    y10 = totalOfRank R10 ys
    y11 = totalOfRank Jack ys
    y12 = totalOfRank Queen ys
    y13 = totalOfRank King ys

{-
    Returns the total number of a given rank each list of cards has
-}
totalOfRank :: Rank -> [Card] -> Int
totalOfRank r []    = 0
totalOfRank r (x:xs)  =
    if r == (rank x)
        then 1 + (totalOfRank r xs)
        else totalOfRank r xs
