simple x = x

calcChange owed given = if change > 0
                        then change
                        else 0
    where change = given - owed

-- can't reassign variable value in GHC. (you can in GHCi, though)
-- x = "dlah"
-- x = 12
--

sumSquareOrSquareSum x y = body (x^2 + y^2) ((x+y)^2)

body =  (\sumSquare squareSum ->
          if sumSquare > squareSum
          then sumSquare
          else squareSum)

doubleDouble x = (\yadda -> yadda*2) x*2

blah x y = let sumSquare = (x^2 + y^2)
               squareSum = (x + y)^2
           in
               if sumSquare > squareSum
               then sumSquare
               else squareSum

overwrite x = let x = 2
              in
                  let x = 3
                  in
                      let x = 4
                      in
                          x

overwriteUsingLambdas x = (\blah -> (\yadda -> (\har -> (\lol -> lol) 4) 3) 2 ) x
