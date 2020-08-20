list = [1 .. 10]
word = "Haskell"

elementAt i =
    list !! i

badElementAccess =
    list !! 10

listLenth list =
    length list

reverseList list =
    reverse list

isPalindrome word =
    word == reverse word

isInList n =
    n `elem` list --такая вот дебильная запись elem n list   - так для всех бинарных функций можно

getFirstN n =
    take n list

getLastN n =
    reverse (take n (reverse list))

getWithoutFirstN n =
    drop n list

zipLists list1 list2 =
    zip list1 list2

getListOfNZeros n =
    take n (cycle [0]) -- cycle - создает бесконечный список