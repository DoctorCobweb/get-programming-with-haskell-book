





-- constructor for a basic cup object
--
-- to send a message to the object we use the pattern:
-- start car   
-- here, the message is start and the receving object is car
-- cf. this to usual OOP where we'd write car.start()
cup f10z = (\message -> message f10z)

-- adding accessors to the cup object
-- make simple messages to 'get' and 'set' values inside the object.
getOz aCup = aCup (\volume -> volume)

drink aCup amountDrank = if ozDiff >= 0
                         then cup ozDiff
                         else cup 0
    where f10z = getOz aCup
          ozDiff = f10z - amountDrank

-- helper method to check where the cup is empty
isEmpty aCup = getOz aCup == 0


