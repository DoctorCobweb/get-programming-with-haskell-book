-- this captston covers
-- 1. using the Monad typeclass to create SQL-like queries on a list
-- 2. generalizing functions written for one Monad (eg List) to many
-- 3. organizing functions with types
--
-- the preceding lesson showed how List as a Monad can also be understood
-- as a list comprehension.
--
-- we will build a set of tools which we'll call HINQ, that will allow
-- us to query our data *relationally*.
--
-- this is the end product tool
-- 1. provide a familiar interface for querying relational data in haskell
-- 2. is stongly typed
-- 3. use lazy evaluation to allow you to pass around queries without
--    executing them
-- 4. can be used seamlessly with other haskell functions


import Control.Monad -- needed for using the guard function
import Control.Applicative


data Name = Name { firstName :: String
                 , lastName :: String
                 }


-- make Name type an instance of Show type
instance Show Name where
    show (Name first last) = mconcat [first, " ", last]


-- each student has a grade level
data GradeLevel = Freshman
                | Sophomore
                | Junior
                | Senior
                deriving (Eq,Ord,Enum,Show)


-- student type
data Student = Student { studentId :: Int
                       , gradeLevel :: GradeLevel
                       , studentName :: Name
                       } deriving Show


-- dummy data
students :: [Student]
students = [(Student 1 Senior (Name "Audre" "Lorde"))
           ,(Student 2 Junior (Name "Leslie" "Silko"))
           ,(Student 3 Freshman (Name "Judith" "Butler"))
           ,(Student 4 Senior (Name "Guy" "Debord"))
           ,(Student 5 Sophomore (Name "Jean" "Baudrillard"))
           ,(Student 6 Junior (Name "Julia" "Kristeva"))]


-- build the 'select' and 'where' and 'join' operation
--
-- 'select' function type sig
-- (a -> b) -> [a] -> [b]
--
-- 'where' function type sig
-- (a -> Bool) -> [a] -> [a]
--
-- 'join' function type sig
--  it takes two lists, of possibly different types, and
--  two functions which act on each list to extract a property will
--  be used to compare and equate during the join. it's important
--  that the type c is an instace of Eq so they can be compared.
-- [a] -> [b] -> (a -> c) -> (b -> c) -> [(a,b)]


----------------------------------------
-- 33.2 basic queries for your list: select and where
----------------------------------------
-- need to preface all functions for HINQ with an underscore. this avoids name
-- collisions and also where is a reserved keywork in haskell.

-- implementing _select
-- leverage the monadic nature of lists, so we make use of do-notation
_select :: Monad m => (a -> b) -> m a -> m b
_select props vals = do
    val <- vals
    let prop = props val
    return prop


-- implementing _where
-- the guard func used in the do-notation body works on types of
-- the Alternative typeclass, hence it's a constraint whereever
-- we use guard
_where :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
_where predicate vals = do
    -- treat your list like a single value
    val <- vals
    -- filter out the results that don't pass your predicate func
    guard(predicate val)
    return val


-- helper function that tests if a String starts with a specific letter
startsWith :: Char -> String -> Bool
startsWith c val = if (head val) == c
                   then True
                   else False

----------------------------------------
-- 33.3 joining Course and Teacher data types
----------------------------------------
-- the core of relational queries is the _join functionality

-- need to create somemore data types before moving onto _join implementation
data Teacher = Teacher { teacherId :: Int
                       , teacherName :: Name 
                       } deriving Show


teachers :: [Teacher]
teachers = [ Teacher 100 (Name "Simone" "De Beauvior")
           , Teacher 200 (Name "Susan" "Sontag")
           ]


data Course = Course { courseId :: Int
                     , courseTitle :: String
                     , teacher :: Int
                     } deriving Show


courses :: [Course]
courses = [ Course 101 "French" 100
          , Course 201 "English" 200
          ]


-- now we want to join these two datasets aka inner join in SQL parlance.
-- => only care about *matching* pairs.
--
-- so, you'll be checking to see whether a given property of data in one list
-- is equal to another property in another list.
-- => pass into our _join function:
-- 1. two lists
-- 2. a function to select the property to join those lists on
--
-- _join will return those lists combined


_join :: (Monad m, Alternative m, Eq c) => m a -> m b -> (a -> c) -> (b -> c) -> m (a,b)
_join list1 list2 prop1 prop2 = do
    val1 <- list1
    val2 <- list2
    let combination = (val1, val2)
    let actVal1 = prop1 val1
    let actVal2 = prop2 val2
    guard( prop1 (fst combination) == prop2 (snd combination) )
    return combination


----------------------------------------
-- 33.4 building your HINQ interface and example queries
----------------------------------------
-- now with _select, _where, and _join, we will build a nice interface
-- for the user. this will constitute HINQ.


joinData = (_join teachers courses teacherId teacher)
whereResult = _where ((== "English") . courseTitle . snd) joinData
selectResult = _select (teacherName . fst ) whereResult


-- nice but not really what we want for SQL-looking queries.
-- want it to read something like this:
-- (_select (teacherName . fst ))
-- (_join teachers courses teacherId teacher)
-- (_where ((== "English") . courseTitle . snd))
--
-- to achieve this we can define a function which takes parameters 
-- in the order of how one usually writes SQL queries, then in the 
-- body of the function we reorder the query using good-ol-lambdas


_hinq selectQuery joinQuery whereQuery  = (\joinData -> 
                                            (\whereResult ->
                                              selectQuery whereResult) (whereQuery joinData)
                                          ) joinQuery

                        
finalResult :: [Name]
finalResult = _hinq (_select (teacherName . fst ))
                    (_join teachers courses teacherId teacher)
                    (_where ((== "English") . courseTitle . snd))


teacherFirstName :: [String]
teacherFirstName = _hinq (_select firstName)
                         finalResult
                         (_where (\_ -> True))


----------------------------------------
-- 33.5 making a HINQ type for your queries
----------------------------------------
-- passing in the default value of True in the above example is annoying.
-- you shouldn't have to do this in order to get all ppls first names.
--
-- so, how can you make it easier to deal with cases with missing where clauses?
-- => create a HINQ type, representing a query, that will have 2 data constructors

-- a query can be a 
-- 1. select clause
-- 2. join/data clause
-- 3. a where clause
-- 4. or just 1. and 2.
--
-- this will allow you to run queries with/without _where statements.
--
-- need to make changes to the _join, _select, and _where functions before 
-- moving on. they work on lists, but we can generalize them to work on Monads.
-- => change type signatures. consequently, need to add a typeclass constraint...
--
-- the guard function works on types of the Altrernative typeclass. 
-- Alternative is a subclass of Applicative. so for _where and _join we add this
-- as a typeclass constraint (because they use the guard func. _select does not)


data HINQ m a b = HINQ (m a -> m b) (m a) (m a -> m a)
                | HINQ_ (m a -> m b) (m a)


runHINQ :: (Monad m, Alternative m) => HINQ m a b -> m b
runHINQ (HINQ sClause jClause wClause) = _hinq sClause jClause wClause
runHINQ (HINQ_ sClause jClause) = _hinq sClause jClause (_where (\_ -> True))


----------------------------------------
-- 33.6 running your HINQ queries
----------------------------------------

query1 :: HINQ [] (Teacher, Course) Name
query1 = HINQ (_select (teacherName . fst))
              (_join teachers courses teacherId teacher)
              (_where (( == "English") . courseTitle . snd))


-- select teacher names from the teachers dataset i.e the teacher list
query2 :: HINQ [] Teacher Name
query2 = HINQ_ (_select teacherName)
               teachers

----------------------------------------
-- 33.6.1 using HINQ with Maybe types
----------------------------------------
-- because we've refactored our functions to work with all members 
-- of Monad and Alternative, we can use our setup to query Maybe
-- datatypes.
possibleTeacher :: Maybe Teacher
possibleTeacher = Just (head teachers)

possibleCourse :: Maybe Course
possibleCourse = Just (head courses)

missingCourse :: Maybe Course
missingCourse = Nothing


-- running a query with a Maybe type means that you will get results
-- only is the query doesn't fail. it can fail from missing data OR
-- because it doesn't find a match.
maybeQuery1 :: HINQ Maybe (Teacher, Course) Name
maybeQuery1 = HINQ (_select (teacherName . fst))
                   (_join possibleTeacher possibleCourse teacherId teacher)
                   (_where (( == "French") . courseTitle . snd))

-- can handle missing courses 
maybeQuery2 :: HINQ Maybe (Teacher, Course) Name
maybeQuery2 = HINQ (_select (teacherName . fst))
                   (_join possibleTeacher missingCourse teacherId teacher)
                   (_where (( == "French") . courseTitle . snd))


----------------------------------------
-- 33.6.2 joining multiple lists to get all enrollments
----------------------------------------
-- first, need another datatype to represent an enrollment
data Enrollment = Enrollment { student :: Int
                             , course :: Int
                             } deriving Show


-- some dummy data
enrollments :: [Enrollment]
enrollments = [ (Enrollment 1 101)
              , (Enrollment 2 101)
              , (Enrollment 2 201)
              , (Enrollment 3 101)
              , (Enrollment 4 201)
              , (Enrollment 4 101)
              , (Enrollment 5 101)
              , (Enrollment 6 201)]

-- join students to enrollments
studentEnrollmentsQuery = HINQ_ (_select (\(st, en) -> (studentName st, course en)))
                                (_join students enrollments studentId student)


studentEnrollments :: [(Name, Int)]
studentEnrollments = runHINQ studentEnrollmentsQuery


-- joining studentEnrollments list with courses.
-- this allows us to get a list of all English students
englishStudentsQuery = HINQ (_select (\(aStudentEnrollmentPair, aCourse) -> (fst aStudentEnrollmentPair) )) 
                            (_join studentEnrollments courses snd courseId) 
                            (_where ((== "English") . courseTitle . snd))


englishStudents :: [Name]
englishStudents = runHINQ englishStudentsQuery


-- make course query a little more general by creating a wrapper func
getEnrollments :: String -> [Name]
getEnrollments courseName = runHINQ courseQuery
    where courseQuery = HINQ (_select (\(aStudentEnrollmentPair, aCourse) -> (fst aStudentEnrollmentPair) )) 
                            (_join studentEnrollments courses snd courseId) 
                            (_where ((== courseName) . courseTitle . snd))







