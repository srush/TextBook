{-# LANGUAGE DeriveDataTypeable #-}
module FriendFast where 

import qualified Client.Facebook as C
import Client.Login
import Text.JSON
import Text.JSON.Generic
import FUtil
import Data.Char
import qualified Data.Trie as T
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString as BS

data FriendFast = FriendFast {
      name :: String,
      uid :: C.UID}
    deriving (Data, Typeable, Show)

fetchFriendsFast ::  C.UID -> C.FacebookM [FriendFast]
fetchFriendsFast uid = do 
    friends <- C.fql_query $ "SELECT name, uid FROM user where uid in (select uid2 from friend where uid1 = "++ show uid ++ ")"
    return $ decodeJSON friends

fetchFriendStorage :: C.UID -> C.FacebookM (T.Trie FriendFast)
fetchFriendStorage f_uid = do
  friends <- fetchFriendsFast f_uid
  let assoc = concat $ map (\f-> zip (BS.splitWith (== (BSI.c2w ' ')) $ BS.pack $ map (BSI.c2w) $ map toLower $ name f) $ repeat $ f) friends 
  return $ T.fromList $ assoc


getFriendsWithName :: (T.Trie FriendFast) -> String -> [(String, FriendFast)]
getFriendsWithName trie name =
    T.toListBy justNames subtrie
    where
      justNames name f = (map BSI.w2c $ BS.unpack $ name, f) 
      subtrie = T.submap bsName trie 
      bsName = BS.pack $ map (BSI.c2w) $ map toLower $ name 