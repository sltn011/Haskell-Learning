getRequestUrl host api targetName targetID =
    host ++ "/" ++ targetName ++ "/" ++ targetID ++ "?token=" ++ api

foo = getRequestUrl "HOST"

bar = foo "API"