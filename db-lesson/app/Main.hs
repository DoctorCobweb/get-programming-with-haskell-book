module Main where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Time


data Tool = Tool { toolId :: Int 
                 , name :: String
                 , description :: String
                 , lastReturned :: Day
                 , timesBorrowed :: Int
                 }

data User = User { userId :: Int
                 , userName :: String
                 }

instance Show User where
    show user = mconcat [ show $ userId user
                        , ".) "
                        , userName user
                        ]

instance Show Tool where
    show tool = mconcat [ show $ toolId tool
                        , ".) "
                        , name tool
                        , "\n description: "
                        , description tool
                        , "\n last returned: "
                        , show $ lastReturned tool
                        , "\n times borrowed: "
                        , show $ timesBorrowed tool
                        , "\n"
                        ]


-- add a user to the db
-- the Only constructor is used to create single-element tuples. this is needed
-- because execute expects you to pass in a tuple of a particular size for your
-- values ie. the tuple represents a row in the table.
--
-- NOTE: this original version has been refactored below to use the withConn
--       helper func
-- addUser :: String -> IO ()
-- addUser userName = do
--     conn <- open "tools.db"
--     execute conn "INSERT INTO users (username) VALUES (?)" (Only userName)
--     print "user added"
--     close conn

-- write a helper func to handle repetitive tasks
withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
    conn <- open dbName
    action conn
    close conn

addUser :: String -> IO ()
addUser userName = withConn "tools.db" $
                   \conn -> do
                     execute 
                       conn 
                       "INSERT INTO users (username) VALUES (?)" 
                       (Only userName)

-- allow a user to borrow aka checkout a tool
checkout :: Int -> Int -> IO ()
checkout userId toolId = withConn "tools.db" $
                         \conn -> do
                           execute 
                             conn
                             "INSERT INTO checkedout (user_id,tool_id) VALUES (?,?)"
                             (userId,toolId)


------------------------------------------------------------
-- 41.4 reading data from the database and FromRow
------------------------------------------------------------
-- the challenge when working with SQL data in haskell is that you need an easy way
-- to make instance of haskell data type from raw data.
--
-- to achieve this, the sqlite-simple library includes a type class call FromRow
--
-- class FromRow a where
--  fromRow :: RowParser a
--
-- the fromRow method returns a RowParser of type 'a', where 'a' is the same
-- type as whatever type you're making an instance of FromRow.
-- 
-- you won't directly use fromRow, but it will be used by functions
-- to query your data. the result is that if you implement FromRow,
-- you can easily transform queries into lists of your data types.

-- here's how we make the User and Tool types an instance of FromRow
-- after that, we can make queries against our database and translate them
-- directly into lists of users and tools
instance FromRow User where
    fromRow = User <$> field
                   <*> field

instance FromRow Tool where
    fromRow = Tool <$> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field

-- and this is how we query our database
--
-- query :: (FromRow r, ToRow q) => Connection -> Query -> q -> IO [r]
-- query_ :: FromRow r => Connection -> Query -> IO [r]
--
-- the query func assumes you're passing in a query string and a
-- parameter for that query.
--
-- the query_ func takes queries that have no parameters as arguments.
--
----------------------------------------------------
-- Query is its _own_ type. up until now we've been treating our queries
-- as strings, but this is all thanks to the OverloadedStrings extension
-- which is doing all the automatic translating for us.
--
-- Question: why do we need two funcs, namely query & query_
-- Answer: primarily because of the way haskell handles types; haskell
--         doesn't support variable arguments (for the same function)
----------------------------------------------------

printUsers :: IO ()
printUsers = withConn "tools.db" $
             \conn -> do
               resp <- query_ conn "SELECT * FROM users;" :: IO [User]
               mapM_ print resp


printToolQuery :: Query -> IO ()
printToolQuery q = withConn "tools.db" $
                   \conn -> do
                     resp <- query_ conn q :: IO [Tool]
                     mapM_ print resp

printTools :: IO ()
printTools = printToolQuery "SELECT * FROM tools;"

printAvailable :: IO ()
printAvailable = printToolQuery $ mconcat [ "SELECT * FROM tools " 
                                          , "WHERE id not in "
                                          , "(SELECT tool_id FROM checkedout);"
                                          ]

printCheckedout :: IO ()
printCheckedout = printToolQuery "SELECT * FROM tools WHERE id in (SELECT tool_id FROM checkedout);"

-- two major steps are left until you can put together your project:
-- 1. check tools back in
-- 2. after a tool is checked back in, you need to a) update its 'times borrowed' field
--                                                 b) update is 'lastReturned' date to current date


--------------------------------------------------
-- 41.5 updating existing data
--------------------------------------------------
-- need to update an existing row in our database.
-- this is the most complex step in our process because
-- we want to avoid errors. errors, blasted errors.

-- first we need to select a tool. this is what 'selectTool'
-- func is for.
-- 
-- it returns type 'IO (Maybe Tool)'
-- => the IO indicates that our database operations always occur in an
--    IO context.
-- => the Maybe type is used because it's possible to pass in an
--    incorrect ID for which we will get an empty query result aka Nothing value.
selectTool :: Connection -> Int -> IO (Maybe Tool)
selectTool conn toolId_ = do
    resp <- query conn "SELECT * from tools WHERE id = (?)" (Only toolId_) :: IO [Tool]
    return $ firstOrNothing resp

firstOrNothing :: [a] -> Maybe a
firstOrNothing [] = Nothing
firstOrNothing (x:_) = Just x

-- after getting our tool, we need to update it and that means
-- we need to get the current day. this requires an IO action
updateTool :: Tool -> Day -> Tool
updateTool tool date = updatedTool
    -- using record syntax, update only the fields need updating
    where updatedTool = tool { lastReturned = date
                             , timesBorrowed = 1 + timesBorrowed tool
                             }

updateOrWarn :: Maybe Tool -> IO ()
updateOrWarn Nothing = putStrLn "couldnt update the tool. Got Nothing as input, id not found"
updateOrWarn (Just tool) = withConn "tools.db" $
                           \conn -> do
                             let q = mconcat ["UPDATE tools SET "
                                              , "lastReturned = ?,"
                                              , " timesBorrowed = ?"
                                              , " WHERE ID = ?;"
                                              ]
                             execute conn q (lastReturned tool
                                            , timesBorrowed tool
                                            , toolId tool
                                            )
                             print "tool updated"

-- finally we need to tie all these steps together
updateToolTable :: Int -> IO ()
updateToolTable toolId = withConn "tools.db" $
                         \conn -> do
                           tool <- selectTool conn toolId
                           currentDate <- utctDay <$> getCurrentTime
                           let updatedTool = updateTool <$> tool
                                                        <*> pure currentDate
                           updateOrWarn updatedTool

-- when a tool gets checked in, you want to delete the row in checkedout table that
-- corresponds to that tool
checkin :: Int -> IO ()
checkin toolId = withConn "tools.db" $
                 \conn -> do
                   execute conn "DELETE FROM checkedout WHERE tool_id= (?);" (Only toolId)
    
-- you never want to checkin a tool in isolation ie you want to update the 
-- tools fields i) timesBorrowed ii) lastReturned, AND remove the row in checkedout table
checkinAndUpdate :: Int -> IO ()
checkinAndUpdate toolId = do
    updateToolTable toolId
    checkin toolId


--------------------------------------------------
-- 41.7 putting it all together
--------------------------------------------------
-- we now have all the code needed to interact with the database.
-- just need to wrap these actions into a useable interface.
-- most database interaction require prompting the user for
-- either a username or an ID.

promptAndAddUser :: IO ()
promptAndAddUser = do
    print "enter new user name"
    userName <- getLine
    addUser userName

promptAndAddTool :: IO ()
promptAndAddTool = do
    print "enter new tool name"
    toolName <- getLine
    print "enter the new tool description"
    toolDesc <- getLine
    addTool toolName toolDesc 

promptAndCheckout :: IO ()
promptAndCheckout = do
    print "enter the id of the user"

    -- because we want an Int type we use <*> and pure
    userId <- pure read <*> getLine 
    print "enter the id of the tool"
    toolId <- pure read <*> getLine
    checkout userId toolId

promptAndCheckin :: IO ()
promptAndCheckin = do
    print "enter the id of tool"
    toolId <- pure read <*> getLine 
    checkinAndUpdate toolId



addTool :: String -> String -> IO ()
addTool name description  = withConn "tools.db" $
                            \conn -> do
                              currentDate <- utctDay <$> getCurrentTime
                              let q = mconcat [ "INSERT INTO tools "
                                              , "(name, description, lastReturned, timesBorrowed) "
                                              , "VALUES (?,?,?,?)"
                                              ]
                              execute conn q ( name
                                             , description
                                             , currentDate 
                                             , 0 :: Int
                                             )


-- data Tool = Tool { toolId :: Int 
--                  , name :: String
--                  , description :: String
--                  , lastReturned :: Day
--                  , timesBorrowed :: Int
--                  }



-- ask user for what they want to do, show it, then call 
-- main again. this allows continuous interaction with the 
-- program => program wont quit after just completing only
-- 1 command.
performCommand :: String -> IO ()
performCommand command
   | command == "users" = printUsers >> main
   | command == "tools" = printTools >> main
   | command == "adduser" = promptAndAddUser >> main
   | command == "addtool" = promptAndAddTool >> main
   | command == "checkout" = promptAndCheckout >> main
   | command == "checkin" = promptAndCheckin >> main
   | command == "in" = printAvailable >> main
   | command == "out" = printCheckedout >> main
   | command == "quit" = print "bye"
   | otherwise = print "sorry command not found" >> main







main :: IO ()
main = do
    print "enter command (users, tools, adduser, addtool, checkout, checkin, in, out, quit: "
    command <- getLine
    performCommand command
