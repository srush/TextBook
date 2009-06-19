{-# LANGUAGE DeriveDataTypeable #-}
module Friends where 

import Client.Facebook
import Client.Login
import Text.JSON
import Text.JSON.Generic
import FUtil


data Friend = Friend
    {name:: String,
     pic_big:: Maybe String,
     uid:: UID,
     birthday:: Maybe String,
     music:: Maybe String,
     tv:: Maybe String,
     movies:: Maybe String,
     books:: Maybe String,
     quotes:: Maybe String,
     about_me:: Maybe String
    } deriving (Data, Typeable, Show)



fetchFriends ::  UID -> FacebookM [Friend]
fetchFriends uid = do 
    friends <- fql_query $ "SELECT name, uid, pic_big, birthday, music, tv, movies, books, quotes, about_me FROM user where uid in (select uid2 from friend where uid1 = "++ show uid ++ "LIMIT 500)"
    return $ decodeJSON friends


fetchFriend ::  UID -> FacebookM Friend
fetchFriend uid = do 
    friends <- fql_query $ "select name, uid, pic_big, birthday, music, tv, movies, books, quotes, about_me from user where uid ="++ show uid
    let [friend] = decodeJSON friends
    return friend 