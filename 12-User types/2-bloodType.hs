type FirstName = String
type MiddleName = String
type LastName = String

data Name = Name FirstName LastName | NameWithMiddle FirstName MiddleName LastName

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

data Gender = Male | Female

genderInitial :: Gender -> Char
genderInitial Male = 'M'
genderInitial Female = 'F'

data BloodType = BloodType ABOType RhType
data ABOType = A | B | AB | O
data RhType = Pos | Neg

bloodTest :: BloodType
bloodTest = BloodType A Pos

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

canDonate :: BloodType -> BloodType -> Bool
canDonate (BloodType O _) _ = True
canDonate _ (BloodType AB _) = True
canDonate (BloodType A _) (BloodType A _) = True
canDonate (BloodType B _) (BloodType B _) = True
canDonate _ _ = False

data Patient = Patient { name :: Name
                       , sex :: Gender
                       , age :: Int
                       , height :: Int
                       , weight :: Int
                       , bloodType :: BloodType }

johnDoe :: Patient
johnDoe = Patient (Name "John" "Doe") Male 43 188 92 (BloodType AB Pos)

jackieSmith :: Patient
jackieSmith = Patient { name = Name "Jackie" "Smith"
                      , age = 100
                      , height = 150
                      , weight = 100
                      , sex = Female
                      , bloodType = BloodType B Neg}

--age jackieSmith
--100
--
--showBloodType (bloodType jackieSmith)
--"B-"

jackieSmithUpdated = jackieSmith{age = 10}