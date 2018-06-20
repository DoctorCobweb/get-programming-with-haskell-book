-- things learned 
-- 1. use the do-notation to simplify working with Monads
-- 2. translate from Monad methods and lambdas, to do-notation
-- 3. generate code from one instance of Monad to all Monads
--
-- it quickly becomes cumbersome when you use all the methods, >>=, >>, and return, of Monad typeclass together. 
--
-- allthough it's important to understand the methods of the Monad typeclass,
-- in practice most of the work you'll do with Monads will done using the
-- following 2 ways
-- 1. do-notation
-- 2. list comprehensions: see how List works as a Monad (HARDER TO UNDERSTAND)


import qualified Data.Map as Map




----------------------------------------
-- 31.1 do-notation revisited
----------------------------------------
-- do-notation is syntactic sugar for using >>=, >>, and (\x -> return (func x))
--
echo :: IO ()
echo = getLine >>= putStrLn

----------------------------------------
-- 31.2 using do-notation to reuse the same code in different contexts
----------------------------------------
--
-- look at a series of examples of using the same code in
-- different context.
--
-- the core task is to process data on job candidates for your company.
-- you want to determine  if they pass or failed the interview.
--
-- what you will see is that the same code can hangle candidates in the 
-- context of IO, Maybe, and even List.
--
-- in the end, you will be able to refactor the code you've reused in 
-- each section to a single function that works on all instances 
-- of Monad.
--
--
-- each Candidate is tracked via a unique ID. during the interview the
-- candidates are given a code review and a culture fit interview.
-- each of these scores are given a grade.
data Grade = F | D | C | B | A deriving (Eq, Ord, Enum, Show, Read)

-- Candidates education level
data Degree = HS | BA | MS | PhD deriving (Eq, Ord, Enum, Show, Read)

data Candidate = Candidate { candidateId :: Int
                           , codeReview :: Grade
                           , cultureFit :: Grade
                           , education :: Degree
                           } deriving Show


viable :: Candidate -> Bool
viable candidate = all ( == True) tests
    where passCoding = codeReview candidate > B
          passCultureFit = cultureFit candidate > C
          educationMin = education candidate >= MS
          tests = [ passCoding
                  , passCultureFit
                  , educationMin
                  ]


-- the IO context 
readInt :: IO Int
readInt = getLine >>= (return . read)


readGrade :: IO Grade
readGrade = do
    grade <- getLine 
    return (read grade)
-- the above is equivalent to doing this:
-- readGrade = getLine >>= (return . read)


readDegree :: IO Degree
readDegree = getLine >>= (return . read)


readCandidate :: IO Candidate
readCandidate = do
    putStrLn "enter Id"
    cId <- readInt
    putStrLn "ender grade"
    cGrade <- readGrade
    putStrLn "enter culture fit"
    cCulture <- readGrade
    putStrLn "enter degree"
    cDegree <- readDegree
    let candidate = Candidate {
          candidateId=cId
        , codeReview=cGrade  
        , cultureFit=cCulture
        , education=cDegree
        }
    return candidate


assessCandidateIO :: IO String
assessCandidateIO = do
    candidate <- readCandidate
    let passed = viable candidate
    let statement = if passed
                    then "passed"
                    else "failed"
    return statement



----------------------------------------
-- 31.2.3 the maybe context - working with a map of candidates
----------------------------------------
candidate1 :: Candidate
candidate1 = Candidate { candidateId = 1
                       , codeReview = A
                       , cultureFit = A
                       , education = BA }

candidate2 :: Candidate
candidate2 = Candidate { candidateId = 2
                       , codeReview = C
                       , cultureFit = A
                       , education = PhD }

candidate3 :: Candidate
candidate3 = Candidate { candidateId = 3
                       , codeReview = A
                       , cultureFit = B
                       , education = MS }

candidateDB :: Map.Map Int Candidate
candidateDB = Map.fromList candidates
    where candidates = zip [1 .. 3] [candidate1, candidate2, candidate3]


feedbackMaybe :: Maybe String -> String
feedbackMaybe Nothing = "error id not fount"
feedbackMaybe (Just a) = a


assessCandidateMaybe :: Int -> Maybe String
assessCandidateMaybe cId = do
    candidate <- Map.lookup cId candidateDB
    let passed = viable candidate
    let statement = if passed
                    then "passed"
                    else "failed"
    return statement


-- notice that this code is pretty much identical. that's because
-- in do-notation, after assigning a variable with <-, you get
-- to pretend it's an orginary type that's NOT IN A PARTICULAR CONTEXT.
--
-- the Monad typeclass and the do-notation have abstracted away the context
-- that you're working in. 
--
-- in the Maybe context above that means you don't have to orry about
-- missing values at all. 
--
-- ( in the Maybe context above, if you ask for a non-existent Candidate
--   from candidateDB you first get Nothing returned. and because all
--   the operations are chained together, which is once again abstracted
--   away in do-notation, the Nothing will 'fall thru' to the last statement.
--   and thus Nothing is returned.
--
-- you can start to think of designing programs that work in any context.





----------------------------------------
-- 31.2.4 the List context - processing a list of candidates
----------------------------------------
candidates :: [Candidate]
candidates = [ candidate1
             , candidate2
             , candidate3]


-- because List is also an instance of Monad, you should be able to convert
-- you other assessCandidate function into an addessCandidateList function.
assessCandidateList :: [Candidate] -> [String]
assessCandidateList candidates = do
    candidate <- candidates
    let passed = viable candidate
    let statement = if passed
                    then "passed"
                    else "failed"
    return statement

-- comments about the above function for List monad:
-- we treat the entire list as a single value. then just
-- churn through the lines in the do-notation as though we
-- have a sinlge value. the monadic typeclass then handles
-- the computation of all elements in the list, under
-- the hood for us. pretty neat.
--

-- now here is the powerful idea that these examples have been hinting towards:
--
-- thinking in terms of Monads and what it represents, we can write a very 
-- general solution which handles all three of these contexts AT ONCE
--
-- 
----------------------------------------
-- 31.2.5 putting it all together and writing a monadic function
----------------------------------------
assessCandidate :: Monad m => m Candidate -> m String
assessCandidate candidates = do
    candidate <- candidates
    let passed = viable candidate
    let statement = if passed
                    then "passed"
                    else "failed"
    return statement



----------------------------------------
-- Summary questions
----------------------------------------
-- Q 31.1
-- main :: IO ()
-- main = putStrLn "what is the size of pizza 1" >>
--        getLine >>=
--        (\size1 -> 
--          putStrLn "what is the cost of pizza 1" >>
--          getLine >>=
--          (\cost1 -> 
--            putStrLn "what is the size of  pizza 2" >>
--            getLine >>=
--            (\size2 ->
--              putStrLn "what is the cost of pizza 2" >>
--              getLine >>=
--              (\cost2 ->
--                (\pizza1 -> 
--                  (\pizza2 -> 
--                    (\betterPizza -> 
--                      putStrLn (describePizza betterPizza)
--                    ) (comparePizzas pizza1 pizza2)
--                  ) (read size2, read cost2)
--                ) (read size1, read cost1) 
--              )
--            )
--          )
--        )




-- Q 31.2
-- listMain :: [String] -> [String] -> String
-- listMain sizes costs = do
--     size <- sizes
--     cost <- costs
--     let 
--     GAH....
--
-- Q. 31.3
--   even less understanding. keep learning and comeback to these 2 questions
