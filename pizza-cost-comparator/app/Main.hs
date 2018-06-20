module Main where

import Pizza

main :: IO ()
main = do
    putStrLn "what is the size of pizza 1?"
    p1Size <- getLine
    putStrLn "what is the cost of pizza 1?"
    p1Cost <- getLine
    putStrLn "what is the size of pizza 2?"
    p2Size <- getLine
    putStrLn "what is the cost of pizza 2?"
    p2Cost <- getLine
    let p1 = (read p1Size, read p1Cost)
    let p2 = (read p2Size, read p2Cost)
    let cheaperPizza = comparePizzas p1 p2
    putStrLn (describePizza cheaperPizza)
