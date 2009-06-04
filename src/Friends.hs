{-# LANGUAGE DeriveDataTypeable #-}
module Friends where 

import Client.Facebook
import Text.JSON.Generic

type UID = Integer
data Friend = Friend
    {name:: String,
     uid:: UID} deriving (Data, Typeable, Show)

fetchFriends ::  UID -> FacebookM [Friend]
fetchFriends uid = do 
    friends <- fql_query $ "select name, uid from user where uid in (select uid2 from friend where uid1 = "++ show uid
    return $ decodeJSON friends