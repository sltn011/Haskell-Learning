import Data.List

listNames = [("Def", "Abc"), ("Klm", "Bcc"), ("Abc", "Def"), ("Def", "Def")]

defaultSort list =
    sort list

customSort_v1 list =
    sortBy sortBySecond_v1 list

customSort_v2 list =
    sortBy sortBySecond_v2 list

sortBySecond_v1 entry1 entry2 =
    if second1 > second2
        then GT
        else if second1 < second2
            then LT
            else if first1 > first2
                then GT
                else if first1 < first2
                    then LT
                    else EQ
    where first1 = fst entry1
          first2 = fst entry2
          second1 = snd entry1
          second2 = snd entry2

sortBySecond_v2 entry1 entry2 =
  if compareSecond /= EQ
      then compareSecond
      else compareFirst
  where first1 = fst entry1
        first2 = fst entry2
        second1 = snd entry1
        second2 = snd entry2
        compareFirst = compare first1 first2
        compareSecond = compare second1 second2