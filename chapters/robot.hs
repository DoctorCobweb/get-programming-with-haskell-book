

-- all objects can be viewed as a collection of attributes that you send
-- messages to.
-- the tuple here serves as a minimum viable data structure.
robot (name, attack, hp) = (\message -> message (name, attack, hp))

-- some accessor functions
name (n,_,_) = n
attack (_,a,_) = a
hp (_,_,hp) = hp

getName aRobot = aRobot name
getAttack aRobot = aRobot attack
getHP aRobot = aRobot hp


-- here's some setters too. like for the case of the coffe cup example,
-- we return a new instance of a robot when we change aka set, any of 
-- the internal values.
-- setName aRobot newName = robot (newName, currentAttack, currentHP)
--     where currentAttack = getAttack aRobot
--           currentHP = getHP aRobot

-- or this way...
setName aRobot newName = aRobot ( \(_, a, hp) -> robot(newName, a, hp) )
setAttack aRobot newAttack = aRobot ( \(n, _, hp) -> robot(n, newAttack, hp) )
setHP aRobot newHP = aRobot ( \(n, a, _) -> robot(n, a, newHP) )

-- a function to print out all the robot's stats.
printRobot aRobot = aRobot (\(n,a,hp) -> n ++
                                         " attack: " ++ (show a) ++
                                         " hp: " ++ (show hp))

-- damage function for a robot
damage aRobot attackDamage = aRobot (\(n,a,hp) -> robot (n,a,hp-attackDamage))

fight aRobot defender = damage defender attack
    where attack = if getHP aRobot > 0
                   then getAttack aRobot
                   else 0


-- order has no importance in execution of haskell code










