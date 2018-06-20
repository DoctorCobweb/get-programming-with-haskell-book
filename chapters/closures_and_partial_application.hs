-- LESSON 5
-- closures and partial application
--
-- closures are the consequence of having lambda functions and 
-- first-class functions.
--
-- this means we can dynamically create functions
--
-- haskell makes closures much easier to work with by 
-- allowing for partial application.
--

inc x = x + 1
double x = x * 2
square x = x ^ 2

ifEven myFunction x = if even x
                      then myFunction x
                      else x

genIfEven f = (\x -> ifEven f x)
genIfXEven x = (\f -> ifEven f x)

-- anytime you might want to use a closure, you wnat
-- to order your arguments from most to least general.
getRequestURL host apiKey resource id =
    host ++ 
    "/" ++
    resource ++
    "/" ++
    id ++
    "?token=" ++
    apiKey

genHostRequestBuilder host = (\ apiKey resource id ->
                                getRequestURL host apiKey resource id)

genApiRequestBuilder hostBuilder apiKey = (\resource id ->
                                            hostBuilder apiKey resource id)

-- when you use partial application, the arguments are applied first to last.


















