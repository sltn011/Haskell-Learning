type FirstName = String
type LastName = String
type Age = Int
type Height = Int

personInfo :: FirstName -> LastName -> Age -> Height -> String
personInfo firstName lastName age height =
    firstName ++ " " ++ lastName ++ " " ++ show age ++ " " ++ show height