{-# LANGUAGE DeriveDataTypeable #-}
module Friends where 

import Client.Facebook
import Client.Login
import Text.JSON.Generic

type UID = Integer
data Friend = Friend
    {name:: String,
     uid:: UID,
     pic_big:: String} deriving (Data, Typeable, Show)

fetchFriends ::  UID -> FacebookM [Friend]
fetchFriends uid = do  
    friends <- fql_query $ "select name, uid, pic_big from user where uid in (select uid2 from friend where uid1 = "++ show uid ++ ")"
    return $ decodeJSON friends

fetchFriend ::  String -> FacebookM Friend
fetchFriend uid = do 
    friends <- fql_query $ "select name, uid, pic_big from user where uid ="++ uid
    let [friend] = decodeJSON friends
    return friend 