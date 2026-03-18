{- 
    DomsMatch: code to play a dominoes match between two players.
    
    The top level function is domsMatch - it takes five arguments:
        games - the number of games to play
        target - the target score to reach
        player1, player2 - two DomsPlayer functions, representing the two players
        seed - an integer to seed the random number generator
    The function returns a pair showing how many games were won by each player.
 -}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module DomsMatch where
    import System.Random
    import Data.List
    import Data.Ord (comparing)

    -- types used in this module
    type Domino = (Int, Int) -- a single domino
    {- Board data type: either an empty board (InitState) or the current state as represented by
        * the left-most domino (such that in the tuple (x,y), x represents the left-most pips)
        * the right-most domino (such that in the tuple (x,y), y represents the right-most pips)
        * the history of moves in the round so far
     -}
    data Board = InitState | State Domino Domino History deriving (Eq, Show)
    {- History should contain the *full* list of dominos played so far, from leftmost to
       rightmost, together with which player played that move and when they played it
     -}
    type History = [(Domino, Player, MoveNum)]
    data Player = P1 | P2 deriving (Eq, Show)
    data End = L | R deriving (Eq, Show)
    type Scores = (Int, Int) -- P1’s score, P2’s score
    type MoveNum = Int
    type Hand = [Domino]
    type PossDomsList = ([Domino], [Domino])
    {- DomsPlayer is a function that given a Hand, Board, Player and Scores will decide
       which domino to play where. The Player information can be used to "remember" which
       moves in the History of the Board were played by self and which by opponent
     -}
    type DomsPlayer = Hand -> Board -> Player -> Scores -> (Domino, End)

    {- domSet: a full set of dominoes, unshuffled -}
    domSet = [ (l,r) | l <- [0..6], r <- [0..l] ]

    {- shuffleDoms: returns a shuffled set of dominoes, given a number generator
       It works by generating a random list of numbers, zipping this list together
       with the ordered set of dominos, sorting the resulting pairs based on the random
       numbers that were generated, then outputting the dominos from the resulting list.
     -}
    shuffleDoms :: StdGen -> [Domino]
    shuffleDoms gen = [ d | (r,d) <- sort (zip (randoms gen :: [Int]) domSet)]

    {- domsMatch: play a match of n games between two players, 
        given a seed for the random number generator
       input: number of games to play, number of dominos in hand at start of each game,
              target score for each game, functions to determine the next move for each
              of the players, seed for random number generator
       output: a pair of integers, indicating the number of games won by each player
     -}
    domsMatch :: Int -> Int -> Int -> DomsPlayer -> DomsPlayer -> Int -> (Int, Int)
    domsMatch games handSize target p1 p2 seed
        = domsGames games p1 p2 (mkStdGen seed) (0, 0)
          where
          domsGames 0 _  _  _   wins               = wins
          domsGames n p1 p2 gen (p1_wins, p2_wins)
            = domsGames (n-1) p1 p2 gen2 updatedScore
              where
              updatedScore
                | playGame handSize target p1 p2 (if odd n then P1 else P2) gen1 == P1 = (p1_wins+1,p2_wins)
                | otherwise                                            = (p1_wins, p2_wins+1)
              (gen1, gen2) = split gen
              {- Note: the line above is how you split a single generator to get two generators.
                 Each generator will produce a different set of pseudo-random numbers, but a given
                 seed will always produce the same sets of random numbers.
               -}

    {- playGame: play a single game (where winner is determined by a player reaching
          target exactly) between two players
       input: functions to determine the next move for each of the players, player to have
              first go, random number generator 
       output: the winning player
     -}
    playGame :: Int -> Int -> DomsPlayer -> DomsPlayer -> Player -> StdGen -> Player
    playGame handSize target p1 p2 firstPlayer gen
        = playGame' p1 p2 firstPlayer gen (0, 0)
          where
          playGame' p1 p2 firstPlayer gen (s1, s2)
            | s1 == target = P1
            | s2 == target = P2
            | otherwise
                = let
                      newScores = playDomsRound handSize target p1 p2 firstPlayer currentG (s1, s2)
                      (currentG, nextG) = split gen
                  in
                  playGame' p1 p2 (if firstPlayer == P1 then P2 else P1) nextG newScores

    {- playDomsRound: given the starting hand size, two dominos players, the player to go first,
        the score at the start of the round, and the random number generator, returns the score at
        the end of the round.
        To complete a round, turns are played until either one player reaches the target or both
        players are blocked.
     -}
    playDomsRound :: Int -> Int -> DomsPlayer -> DomsPlayer -> Player -> StdGen -> (Int, Int) -> (Int, Int)
    playDomsRound handSize target p1 p2 first gen scores
        = playDomsRound' p1 p2 first (hand1, hand2, InitState, scores)
          where
          -- shuffle the dominoes and generate the initial hands
          shuffled = shuffleDoms gen
          hand1 = take handSize shuffled
          hand2 = take handSize (drop handSize shuffled)
          {- playDomsRound' recursively alternates between each player, keeping track of the game state
             (each player's hand, the board, the scores) until both players are blocked -}
          playDomsRound' p1 p2 turn gameState@(hand1, hand2, board, (score1,score2))
            | (score1 == target) || (score2 == target) || (p1_blocked && p2_blocked) = (score1,score2)
            | turn == P1 && p1_blocked = playDomsRound' p1 p2 P2 gameState
            | turn == P2 && p2_blocked = playDomsRound' p1 p2 P1 gameState
            | turn == P1               = playDomsRound' p1 p2 P2 newGameState
            | otherwise                = playDomsRound' p1 p2 P1 newGameState
              where
              p1_blocked = blocked hand1 board
              p2_blocked = blocked hand2 board
              (domino, end)          -- get next move from appropriate player
                  | turn == P1 = p1 hand1 board turn (score1, score2)
                  | turn == P2 = p2 hand2 board turn (score1, score2)
                                     -- attempt to play this move
              maybeBoard             -- try to play domino at end as returned by the player
                  | turn == P1 && not (domInHand domino hand1) = Nothing -- can't play a domino you don't have!
                  | turn == P2 && not (domInHand domino hand2) = Nothing
                  | otherwise = playDom turn domino board end
              newGameState           -- if successful update board state (exit with error otherwise)
                 | maybeBoard == Nothing = error ("Player " ++ show turn ++ " attempted to play an invalid move.")
                 | otherwise             = (newHand1, newHand2, newBoard,
                                              (limitScore score1 newScore1, limitScore score2 newScore2))
              (newHand1, newHand2)   -- remove the domino that was just played
                 | turn == P1 = (hand1\\[domino], hand2)
                 | turn == P2 = (hand1, hand2\\[domino])
              score = scoreBoard newBoard (newHand1 == [] || newHand2 == [])
              (newScore1, newScore2) -- work out updated scores
                 | turn == P1 = (score1+score,score2)
                 | otherwise  = (score1,score2+score)
              limitScore old new     -- make sure new score doesn't exceed target
                 | new > target = old
                 | otherwise    = new
              Just newBoard = maybeBoard -- extract the new board from the Maybe type

    {- domInHand: check if a particular domino is contained within a hand -}
    domInHand :: Domino -> Hand -> Bool
    domInHand (l,r) hand = [ 1 | (dl, dr) <- hand, (dl == l && dr == r) || (dr == l && dl == r) ] /= []

    {- scoreBoard: given the board and a bool for if the last domino in hand was played, 
       returns the score of the boars at that state
     -}
    scoreBoard :: Board -> Bool -> Int
    scoreBoard (State (l1, l2) (r1, r2) _) isLast = calculateScore
        where
        -- add the left and right sides addressing doubles
        total
          | lefSideIsDouble && rightSideIsDouble = l1 * 2 + r1 * 2
          | lefSideIsDouble = l1 * 2 + r2
          | rightSideIsDouble = l1 + r1 * 2
          | otherwise = l1 + r2
        lefSideIsDouble = l1 == l2
        rightSideIsDouble = r1 == r2
        {- calculate score addressing the total being divisible by 3 and/or 5 and 
            adding 1 if its the last domino in the hand
        -}
        calculateScore
          | total `mod` 3 == 0 && total `mod` 5 == 0 = total `div` 5 + total `div` 3 + ifLast
          | total `mod` 5 == 0 = total `div` 5 + ifLast
          | total `mod` 3 == 0 = total `div` 3 + ifLast
          | otherwise = 0 + ifLast
        ifLast = if isLast then 1 else 0

    {- blocked: given the dominos in a hand and the board, 
       returns true if there are not moves available a domino to play
     -}
    blocked :: Hand -> Board -> Bool
    blocked _ InitState = False
    blocked [] _ = True
    blocked ((x1, x2) : rest) (State (l1, l2) (r1, r2) h)
        | x1 == l1 || x1 == r2 || x2 == l1 || x2 == r2 = False
        | null rest = True
        | otherwise = blocked rest (State (l1, l2) (r1, r2) h)

    {- playDom: given the player that moves, a domino to play, a board to add to and 
       the end to add the domino to, it maybe returns an updated board or maybe nothing.
       The return depends on if canPlay passes. If so a new board is returned, with 
       the updated state including the new move. 
     -}
    playDom :: Player -> Domino -> Board -> End -> Maybe Board
    playDom player (x1, x2) InitState end = Just (State (x1, x2) (x1, x2) [((x1, x2), player, 1)])
    playDom player (x1 ,x2) (State (l1, l2) (r1, r2) h) end
        -- check if the domino can be played
        | canPlay (x1, x2) (State (l1, l2) (r1, r2) h) = newBoard
        | otherwise = Nothing
          where
          orientateDomino -- turn domino if needed
            | end == L && l1 == x1 = (x2, x1)
            | end == L && l1 == x2 = (x1, x2)
            | end == R && r2 == x1 = (x1, x2)
            | end == R && r2 == x2 = (x2, x1)
          newHistory -- append the new move to the history
            | end == L = (orientateDomino, player, length h + 1) : h
            | end == R = h ++ [(orientateDomino, player, length h + 1)]
          newBoard --update the board with the domino on the corresponding side
            | end == L = Just (State orientateDomino (r1, r2) newHistory)
            | end == R = Just (State (l1, l2) orientateDomino newHistory)

    {- canPlay: given a domino to play and a board to play it on, it returns if the move can be made -}
    canPlay :: Domino -> Board -> Bool
    canPlay _ InitState = True
    canPlay (x, y) (State (l, _) (_, r) _) = l == x || l == y || r == x || r == y

    {- simplePlayer: provide a domino that can be played.
       input: dominos in hand, board to play, player to act and the score
       output: domino to use
     -}
    simplePlayer :: DomsPlayer
    simplePlayer ((x1, x2): rest) InitState player score = ((x1, x2), L)
    simplePlayer ((x1, x2): rest) (State (l1, l2) (r1, r2) h) player score
        -- if any domino matches with any end of the board, use it
        | l1 == x1 || l1 == x2 = ((x1, x2), L)
        | r2 == x1 || r2 == x2 = ((x1, x2), R)
        | otherwise = simplePlayer rest (State (l1, l2) (r1, r2) h) player score

    {- smartPlayer: provide a domino that will result in a higher probability of winning the game. 
       input: dominos in hand, board to play, player to act and the score
       output: domino to use
     -}
    smartPlayer :: DomsPlayer
    smartPlayer hand board player scores 
        | firstTurn = case firstDrop hand of 
          Just goodStartingDomino -> (goodStartingDomino, L)
          Nothing -> getHighestScore
        | otherwise = getHighestScore
        where
        getHighestScore = highestScore board (possPlay hand board)
        firstTurn = board == InitState


    {- possPlay: given a hand and a board it returns a two lists as a pair of 
       left and right possible playable dominos.
     -}
    possPlay :: Hand -> Board -> PossDomsList
    possPlay hand InitState = (hand, hand)
    possPlay hand (State (l1, l2) (r1, r2) _)
        -- filter the lists down to acceptable values
        = (filter (compatibleWith l1) hand, filter (compatibleWith r2) hand)
          where
          compatibleWith endValue (xSelected, ySelected) = 
              endValue == xSelected || endValue == ySelected

    {- highestScore: STRATEGY - given a board and a list of possible dominos to play,
       return the highest scoring domino and the side to put the domino on
     -}
    highestScore :: Board -> PossDomsList -> (Domino, End)
    highestScore board (leftDominos, rightDominos)
        -- select a domino from the provided none empty lists
        | null leftDominos = (bestRightDomino, R)
        | null rightDominos = (bestLeftDomino, L)
        -- if both lists contain items, select the one with a higher score
        | testScore bestLeftDomino L >= testScore bestRightDomino R =  (bestLeftDomino, L)
        | otherwise = (bestRightDomino, R)
          where
          -- named values to increase readability
          bestLeftDomino = getBest leftDominos (head leftDominos) (head leftDominos) L
          bestRightDomino = getBest rightDominos (head leftDominos) (head rightDominos) R
          -- given a list of dominos and a side to use, return the highest scorer from that list
          getBest [] leftDomino rightDomino side
            | side == L = leftDomino
            | side == R = rightDomino
          getBest ((x1, x2): rest) leftDomino rightDomino side
            | side == L && testScore (x1, x2) side > testScore leftDomino side = getBest rest (x1, x2) rightDomino side
            | side == R && testScore (x1, x2) side > testScore rightDomino side = getBest rest leftDomino (x1, x2) side
            | otherwise = getBest rest leftDomino rightDomino side
          -- get the test scores by applying the values to test boards 
          testBoard (x, y) end = playDom P1 (x, y) board end
          testScore (x, y) end = case testBoard (x, y) end of
            Just successfulBoard -> scoreBoard successfulBoard False
            Nothing -> -1

    {- firstDrop: STRATEGY - given a hand if it contains a domino with a value of (4,5) 
       it returns the domino, otherwise it returns Nothing
     -}
    firstDrop :: Hand -> Maybe Domino
    firstDrop ((x, y) : rest) 
        | x == 4 && y == 5 || x == 5 && y == 4 = Just (x, y)
        | otherwise = firstDrop rest 
    firstDrop _ = Nothing
