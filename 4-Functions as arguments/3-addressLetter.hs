name1 = ("Bob", "Smith")
name2 = ("Bob", "Johns")
name3 = ("Dazy", "Smith")

location1 = "ny"
location2 = "sf"
location3 = "reno"
location4 = "la"

addressLetter_v1 name location =
    nameText ++ " - " ++ location
    where nameText = (fst name) ++ " " ++ (snd name)

-- Пусть в sf для фамилий начиная с Л - другой адрес почты
-- В ny после отправителя должно идти двоеточие
-- В reno используют только фамилии

nyAddress name =
    nameText ++ ": " ++ "1234, NewYork 10013"
    where nameText = (fst name) ++ " " ++ (snd name)

sfAddress name =
    if lastName < "L"
        then nameText ++ " - " ++ "1122, SanFrancisco 12301"
        else nameText ++ " - " ++ "1122, SanFrancisco 12302"
    where lastName = snd name
          nameText = (fst name) ++ " " ++ (snd name)

renoAddress name =
    lastName ++ " - " ++ "2341, Reno 11500"
    where lastName = snd name

addressLetter_v2 name location =
    (getLocationFunction location) name
    -- или locationFunction name
    --where locationFunction = (getLocationFunction location)

getLocationFunction location =
    case location of
        "ny" -> nyAddress
        "sf" -> sfAddress
        "reno" -> renoAddress
        _ -> (\name -> (fst name) ++ " " ++ (snd name))