import Data.List


ifEven myFunction x = if even x
                      then myFunction x
                      else x

inc n = n + 1
double n = n*2
square n = n^2

ifEvenInc n = ifEven inc n
ifEvenDouble n = ifEven double n 
ifEvenSquare n = ifEven square n

add x y = x + y


names = [ ("Ian", "Curtis"),
          ("Bernard", "Summer"),
          ("Peter", "Hook"),
          ("Stephen", "Morris"),
          ("Andre", "Morris")]

compareLastNames name1 name2 = if lastName1 > lastName2
                               then GT
                               else if lastName1 < lastName2
                                   then LT
                                   else if firstName1 < firstName2
                                        then LT 
                                        else if firstName1 > firstName2
                                             then GT
                                             else EQ

    where lastName1 = snd name1
          lastName2 = snd name2
          firstName1 = fst name1
          firstName2 = fst name2


-- returning functions example. postal addresses
sfOffice name = if lastName < "L"
                then nameText ++ " - PO Box 1234 - San Francisco, CA, 94111"
                else nameText ++ " - PO Box 1010 - San Francisco, CA, 94109"
    where lastName = snd name
          nameText = (fst name) ++ " " ++ lastName

nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
    where nameText = (fst name) ++ " " ++ (snd name)

renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
    where nameText = snd name

washingtonOffice name = nameText ++ ": PO Box 111 - Washington, DC, 11111" 
    where nameText = (fst name) ++ " " ++ (snd name) ++ " Esq"

getLocationFunction location = case location of
    "ny" -> nyOffice
    "sf" -> sfOffice
    "reno" -> renoOffice
    "wash" -> washingtonOffice
    _ -> (\name -> (fst name) ++ " " ++ (snd name))

addressLetter name location = locationFunction name
    where locationFunction = getLocationFunction location



