-- will be able to do this stuff after the lession:
-- 1. define type synonyms to clarify code
-- 2. create your own data type
-- 3. build types from other types
-- 4. work with complex types by using record syntax
--
-- creating types in haskell is more important than in most other
-- programming languages; nearly every problem you solve will
-- come down to the types you're using.
-- even when using an existing type, you'll often want to 
-- rename it to make understanding your programs easier.
--
-- areaOfCircle :: Double -> Double 
--
--conveys less information that this:
--
-- areaOfCircle :: Diameter -> Area
-- 
--
-- creating types for data in haskell is as important as creating
-- classes in object-oriented language.
--
--
-- type synonyms using the `type` keyword
type FirstName = String
type LastName = String
type Age = Int
type Height = Int

type PatientName = (String,String)

type MiddleName = String
-- a more flexible Name type
data Name = Name FirstName LastName
          | NameWithMiddle FirstName MiddleName LastName

showName :: Name -> String
showName (Name fname lname) = lname ++ ", " ++ fname 
showName (NameWithMiddle fname mname lname) = lname ++ ", " ++ fname ++ " " ++ mname

firstName :: PatientName -> String
firstName patient = fst patient

lastName :: PatientName -> String
lastName patient = snd patient

patientInfo :: PatientName -> Age -> Height -> String
patientInfo pname age height = name ++ " " ++ ageHeight
    where name = firstName pname ++ ", " ++ lastName pname 
          ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"




-- creating NEW types using the `data` keyword
data Sex = Male | Female

sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'

-- model a person's blood type
-- antibodies are A, B, AB or O
-- the presence or absence of a particular antigen is Pos or Neg,
-- called a person's Rhesus group
--
data RhType = Pos | Neg
data ABOType = A | B | AB | O

-- finally define your blood type
data BloodType = BloodType ABOType RhType

patient1BT :: BloodType
patient1BT = BloodType A Pos

patient2BT :: BloodType
patient2BT = BloodType O Neg

patient3BT :: BloodType
patient3BT = BloodType AB Pos

-- let's make some helper functions for displaying the values
showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

-- pattern matching with the new type makes this much easier to do.
-- pay attention to how this is done.
showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

-- determine whether one BloodType can donate to another BloodType
-- (forget about RhType dependencies for now..)
canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True -- O can donate to every type
canDonateTo _ (BloodType AB _) = True -- AB is a universal receiver
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False


-- new version using record syntax and pattern matchin in the function
canDonateToV2 :: Patient -> Patient -> Bool
canDonateToV2 Patient{bloodType = BloodType O _ } _ = True
canDonateToV2 _ Patient{bloodType = BloodType AB _ } = True -- AB is a universal receiver
canDonateToV2 Patient{bloodType = BloodType A _} Patient{bloodType = BloodType A _} = True
canDonateToV2 Patient{bloodType = BloodType B _} Patient{bloodType = BloodType B _} = True
canDonateToV2 _ _ = False

-- using record syntax
-- provides a handy way to define types with many values.
-- Patient v.2 using record syntax
data Patient = Patient { name :: Name
                       , sex :: Sex
                       , age :: Int
                       , weight :: Int
                       , height :: Int
                       , bloodType :: BloodType }

-- record syntax makes it much easier to read and understand the data type. also, it much
-- easier to set/create each field name (and order no longer matters).
jackieSmith :: Patient
jackieSmith = Patient { name = Name "Jackie" "Smith"
                      , age = 43
                      , sex = Female
                      , height = 43
                      , weight = 1110
                      , bloodType = BloodType O Neg}

-- this is how you set values in record syntax
jackieSmithUpdated = jackieSmith { age = 4444}

dre :: Patient
dre = Patient { name = Name "dre" "trosky"
                      , age = 35
                      , sex = Male 
                      , height = 143
                      , weight = 110
                      , bloodType = BloodType AB Pos}
