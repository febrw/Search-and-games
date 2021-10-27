-- Inf2d Assignment 1 2018-2019
-- Matriculation number: S1735009
-- {-# OPTIONS -Wall #-}


module Inf2d1 where

import Data.List (sortBy)
import Debug.Trace
import TTTGame

gridLength_search::Int
gridLength_search = 6
gridWidth_search :: Int
gridWidth_search = 6


{- NOTES:

-- DO NOT CHANGE THE NAMES OR TYPE DEFINITIONS OF THE FUNCTIONS!
You can write new auxillary functions, but don't change the names or type definitions
of the functions which you are asked to implement.

-- Comment your code.

-- You should submit this file, and only this file, when you have finished the assignment.

-- The deadline is the  13th March 2018 at 3pm.

-- See the assignment sheet and document files for more information on the predefined game functions.

-- See the README for description of a user interface to test your code.

-- See www.haskell.org for haskell revision.

-- Useful haskell topics, which you should revise:
-- Recursion
-- The Maybe monad
-- Higher-order functions
-- List processing functions: map, fold, filter, sortBy ...

-- See Russell and Norvig Chapters 3 for search algorithms,
-- and Chapter 5 for game search algorithms.

-}

-- Section 1: Uniform Search

-- 6 x 6 grid search states

-- The Node type defines the position of the robot on the grid.
-- The Branch type synonym defines the branch of search through the grid.
type Node = (Int,Int)
type Branch = [(Int,Int)]

badNodesList::[Node]
-- This is your list of bad nodes. You should experimet with it to make sure your algorithm covers different cases. 
badNodesList = []
somebadNodess = [(1,3),(1,5),(3,1),(3,2),(4,4),(4,6),(5,2),(6,4)]

-- The maximum depth this search can reach
-- TODO: Fill in the maximum depth and justify your choice
maxDepth::Int
maxDepth= 23
-- Why did you choose this number?
-- YOUR ANSWER GOES HERE

-- The configuration with the furthest optimal solution forces the search to take 23 steps, any depth less than this will return a solution if available
-- any solutions that take more than 23 steps are guaranteed to be non-optimal


-- The next function should return all the possible continuations of input search branch through the grid.
-- Remember that the robot can only move up, down, left and right, and can't move outside the grid.
-- The current location of the robot is the head of the input branch.
-- Your function should return an empty list if the input search branch is empty.
-- This implementation of next function does not backtrace branches.

next::Branch -> [Branch]
next [] = []
next branch = [ 
    node:branch 
    | node <- potentialSuccessors,
-- check within 6x6 grid
    boundary_conditions node, 
-- check we do not backtrack
    not $ elem node branch,
-- check not a faulty state
    not $ elem node badNodesList]

  where 
-- assign predecessor and current nodes            
    (x,y) = head branch
    potentialSuccessors = 
        [ (x-1, y)
        , (x, y+1)
        , (x+1, y)
        , (x, y-1)
        ]
    boundary_conditions (x,y) = and [x>=1 , x <= gridWidth_search ,y >= 1 ,y <= gridLength_search]

-- |The checkArrival function should return true if the current location of the robot is the destination, and false otherwise.
 -- Note that this is the right type declaration for this function. You might have an old version of the Assignment PDF that names this wrongly.
checkArrival::Node -> Node -> Bool
checkArrival destination curNode = destination == curNode

-- Section 3 Uniformed Search
-- | Breadth-First Search
-- The breadthFirstSearch function should use the next function to expand a node,
-- and the checkArrival function to check whether a node is a destination position.
-- The function should search nodes using a breadth first search order.

breadthFirstSearch::Node->(Branch -> [Branch])->[Branch]->[Node]->Maybe Branch
breadthFirstSearch destination next [] exploredList = Nothing
breadthFirstSearch destination next (b:bs) exploredList
-- terminal test: if true, returns the branch which gives a solution to the search
    | checkArrival destination (head b) = Just b
-- new branches appended to the end of our search agenda to implement a FIFO queue for BFS
    | otherwise = breadthFirstSearch destination next
        (bs ++ next(
-- check we are not revisiting search paths
            if (head b `elem` exploredList)
                then  []
            else b)
        )
-- update the explored list
        (head b : exploredList)
-- | Depth-First Search
-- The depthFirstSearch function is similiar to the breadthFirstSearch function,
-- except it searches nodes in a depth first search order.
depthFirstSearch::Node->(Branch -> [Branch])->[Branch]-> [Node]-> Maybe Branch
depthFirstSearch destination next [] exploredList = Nothing
depthFirstSearch destination next (b:bs) exploredList
-- terminal test: if true, returns the branch which gives a solution to the search
    | checkArrival destination (head b) = Just b
-- new branches appended to the start of our search agenda to implement a LIFO queue for DFS
    | otherwise = depthFirstSearch destination next
        ( next (
            if (head b `elem` exploredList)
                then  []
            else b) ++ bs )
-- update the explored list
        (head b : exploredList)

-- | Depth-Limited Search
-- The depthLimitedSearch function is similiar to the depthFirstSearch function,
-- except its search is limited to a pre-determined depth, d, in the search tree.
depthLimitedSearch::Node->(Branch -> [Branch])->[Branch]-> Int-> Maybe Branch
depthLimitedSearch destination next [] _ = Nothing
depthLimitedSearch destination next (b:bs) d
-- terminal test: if true, returns the branch which gives a solution to the search
    | checkArrival destination (head b) = Just b
    | otherwise = depthLimitedSearch destination next
        (next (
-- same LIFO queue for new branches, but checks if within initial depth d
            if ( length b - 1 >= d)
                then []
            else b) ++ bs ) d
-- | Iterative-deepening search
-- The iterDeepSearch function should initially search nodes using depth-first to depth d,
-- and should increase the depth by 1 if search is unsuccessful.
-- This process should be continued until a solution is found.
-- Each time a solution is not found the depth should be increased.

iterDeepSearch:: Node-> (Branch -> [Branch])->Node -> Int-> Maybe Branch
iterDeepSearch destination next initialNode 0 = Nothing
iterDeepSearch destination next initialNode d
-- return nothing if no solution found up to depth d
    | maxDepth >= d = Nothing
    | solution == Nothing =  iterDeepSearch destination next initialNode (d+1)
    | otherwise = solution
-- call depthLimitedSearch, making a branch from our intitialNode to a depth d
-- if the search is unsuccessful, increase the depth by one and repeat
  where solution = depthLimitedSearch destination next [[initialNode]] d

-- | Section 4: Informed search

-- Manhattan distance heuristic
-- This function should return the manhattan distance between the 'position' point and the 'destination'.
manhattan::Node->Node->Int
manhattan (a,b) (c,d) = abs (a - c) + abs (b - d)
-- | Best-First Search
-- The bestFirstSearch function uses the checkArrival function to check whether a node is a destination position,
-- and the heuristic function (of type Node->Int) to determine the order in which nodes are searched.
-- Nodes with a lower heuristic value should be searched before nodes with a higher heuristic value.

bestFirstSearch::Node->(Branch -> [Branch])->(Node->Int)->[Branch]-> [Node]-> Maybe Branch
bestFirstSearch destination next heuristic [] _ = Nothing
bestFirstSearch destination next heuristic (b:bs) exploredList
-- terminal test
    | checkArrival destination (head b) = Just b
    | otherwise = bestFirstSearch destination next heuristic
-- nodes are expanded by next, the list of expanded nodes are then sorted by the heuristic, so as to prioritise smallest manhattan distance
-- this sorted list is then taken as the new search agenda with each iteration
        (sortBy (\x y -> compare (heuristic (head x)) (heuristic (head y)))
            (
-- check we are not revisiting search paths
                (if (head b `elem` exploredList)
                    then  next ([])
                else
                    next b
                ) ++ bs))
-- update the explored list
        ((head b) : exploredList)
-- | A* Search
-- The aStarSearch function is similar to the bestFirstSearch function
-- except it includes the cost of getting to the state when determining the value of the node.

aStarSearch::Node->(Branch -> [Branch])->(Node->Int)->(Branch->Int)->[Branch]-> [Node]-> Maybe Branch
aStarSearch destination next heuristic cost [] exploredList = Nothing
aStarSearch destination next heuristic cost (b:bs) exploredList
-- terminal test
    | checkArrival destination (head b) = Just b
    | otherwise = aStarSearch destination next heuristic cost
-- nodes are expanded by next, the list of expanded nodes are then sorted,
-- by the smallest combined heuristic, so as to prioritise smallest total cost of the branch to the goal
        
        (sortBy (\x y -> compare ((heuristic (head x)) + cost x) ((heuristic (head y)) + cost y))
            (
-- check we are not revisiting search paths
                (if (head b `elem` exploredList)
                    then  next ([])
                else
                    next b
                ) ++ bs))
-- update the explored list
        ((head b) : exploredList)
 
-- | The cost function calculates the current cost of a trace, where each movement from one state to another has a cost of 1.

-- if the branch contains only one node, it is the initial node and so no search steps have been taken.
-- for any nodes after that, it costs us exactly one step to get to each one, it then follow that:

cost :: Branch  -> Int
cost [] = 0
cost b = length b - 1
-- | Section 5: Games
-- See TTTGame.hs for more detail on the functions you will need to implement for both games' minimax and alphabeta searches. 
-- | Section 5.1 Tic Tac Toe

-- | The eval function should be used to get the value of a terminal state. 
-- A positive value (+1) is good for max player. The human player will be max.
-- A negative value (-1) is good for min player. The computer will be min.
-- A value 0 represents a draw.

    eval :: Game -> Int
    -- simply checks if player 1 has won, and if so returns 1, else check for player 0 and if so returns -1, else returns 0 as draw
    eval game 
        | terminal game && checkWin game 1 = 1
        | terminal game && checkWin game 0 = (-1)
        | otherwise = 0
    -- | The minimax function should return the minimax value of the state (without alphabeta pruning).
    -- The eval function should be used to get the value of a terminal state. 

    minimax:: Game->Player->Int
    minimax game player
    -- if terminal test is true, return utility value for the state (game)
        | terminal game = eval game
        | maxPlayer player = maximum [ minimax x (switch player)| x <- moves game player]
        | minPlayer player = minimum [ minimax x (switch player)| x <- moves game player]

    -- | The alphabeta function should return the minimax value using alphabeta pruning.
    -- The eval function should be used to get the value of a terminal state. 

    alphabeta:: Game->Player->Int
    alphabeta game player = abcontroller game player (-2) 2

    -- abWcontroller is used to keep track of alpha and beta in our recursion
    -- we use helper functions to recurse through the child nodes at any level, updating v and either alpha or beta along the way
    -- if the pruning conditions are met, we return v and don't check further child nodes

    abcontroller :: Game->Player->Int->Int->Int
    abcontroller game player alpha beta
        | maxPlayer player = maxValue (moves game player) alpha beta (-2)
        | minPlayer player = minValue (moves game player) alpha beta 2

    -- maxValue takes a set of game states, and updates v, the largest terminal ulitily value seen yet, as it progresses through the states
    -- abcontroller is called to switch player and look at the game states one level below
    maxValue:: [Game]->Int->Int->Int->Int
maxValue [] alpha beta value = value
maxValue (x:xs) alpha beta value
    | terminal x = eval x
    | v >= beta = v
    | otherwise = maxValue xs (max alpha value) beta v
    where
        v = max value (abcontroller x 0 alpha beta)
-- minValue takes a set of game states, and updates v, the smallest terminal ulitily value seen yet, as it progresses through the states
-- abcontroller is called to switch player and look at the game states one level below
minValue:: [Game]->Int->Int->Int->Int
minValue [] alpha beta value = value
minValue (x:xs) alpha beta value
    | terminal x = eval x
    | v <= alpha = v
    | otherwise = minValue xs alpha (min value beta) v
    where
        v = min value (abcontroller x 1 alpha beta)
-- | Section 5.2 Wild Tic Tac Toe
-- | The evalWild function should be used to get the value of a terminal state. 
-- It should return 1 if either of the move types is in the correct winning position. 
-- A value 0 represents a draw.

evalWild :: Game -> Int
-- simply gives the player who reached(!) the terminal state +1  if either the x's or the o's are in the correct position.
evalWild game
    | terminal game && checkWin game 1 = 1
    | terminal game && checkWin game 0 = 1
    | terminal game = 0

-- | The alphabetaWild function should return the minimax value using alphabeta pruning.
-- The evalWild function should be used to get the value of a terminal state. Note that this will now always return 1 for any player who reached the terminal state.
-- You will have to modify this output depending on the player. If a move by the max player sent(!) the game into a terminal state you should give a +1 reward. 
-- If the min player sent the game into a terminal state you should give -1 reward. 

alphabetaWild:: Game->Player->Int
alphabetaWild game player = abWcontroller game player (-2) 2

-- abWcontroller is used to keep track of alpha and beta in our recursion
-- we use helper functions to recurse through the child nodes at any level, updating v and either alpha or beta along the way
-- if the pruning conditions are met, we return v and don't check further child nodes
    
abWcontroller :: Game->Player->Int->Int->Int
abWcontroller game player alpha beta
-- using movesWild to expand our game state
    | terminal game = (evalWild game) * (if (player == 0) then 1 else -1)
    | maxPlayer player = maxWValue (movesWild game player) alpha beta (-2)
    | minPlayer player = minWValue (movesWild game player) alpha beta 2

-- maxWValue takes a set of game states, and updates v, the largest terminal ulitily value seen yet, as it progresses through the states
-- abWcontroller is called to switch player and look at the game states one level below
maxWValue:: [Game]->Int->Int->Int->Int
maxWValue [] alpha beta value = value
maxWValue (x:xs) alpha beta value
    | v >= beta = v
    | otherwise = maxWValue xs (max alpha v) beta v
    where
        v = max value (abWcontroller x 0 alpha beta)

-- minValue takes a set of game states, and updates v, the smallest terminal ulitily value seen yet, as it progresses through the states
-- abcontroller is called to switch player and look at the game states one level below
minWValue:: [Game]->Int->Int->Int->Int
minWValue [] alpha beta value = value
minWValue (x:xs) alpha beta value
    | v <= alpha = v
    | otherwise = minWValue xs alpha (min v beta) v
    where
        v = min value (abWcontroller x 1 alpha beta)
-- | End of official assignment. However, if you want to also implement the minimax function to work for Wild Tic Tac Toe you can have a go at it here. This is NOT graded.


-- | The minimaxWild function should return the minimax value of the state (without alphabeta pruning). 
-- The evalWild function should be used to get the value of a terminal state. 

minimaxWild:: Game->Player->Int
minimaxWild game player
-- if terminal test is true, return utility value for the state (game)
    | terminal game = (evalWild game) * (if (player == 0) then 1 else -1)
    | terminal game = 0
    | maxPlayer player = maximum [ minimax x (switch player)| x <- movesWild game player]
    | minPlayer player = minimum [ minimax x (switch player)| x <- movesWild game player]

-- | Auxiliary Functions
-- Include any auxiliary functions you need for your algorithms here.
-- For each function, state its purpose and comment adequately.
-- Functions which increase the complexity of the algorithm will not get additional scores

