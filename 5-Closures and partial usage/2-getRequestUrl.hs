getRequestUrl host api targetName targetID =
    host ++ "/" ++ targetName ++ "/" ++ targetID ++ "?token=" ++ api

genHostRequestUrl host =
    (\api targetName targetID -> getRequestUrl host api targetName targetID)