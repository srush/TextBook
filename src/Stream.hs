{-# LANGUAGE DeriveDataTypeable #-}
module Stream where 

import Client.Facebook
import Client.Login
import Text.JSON
import Text.JSON.Generic
import Text.JSON.Types
import qualified Data.Map as M
import Data.Maybe
import FUtil


(!*) :: JSObject JSValue -> String -> String
(!*) obj key = fromJSString jsstring
    where JSString jsstring = fromJust $ get_field obj key

(!^) :: JSObject JSValue -> String -> Integer
(!^) obj key = round rat
    where JSRational _ rat = fromJust $ get_field obj key

 
fetchStream :: UID -> FacebookM ([JSValue], M.Map Integer String)
fetchStream uid =  do
    stream <- fql_query $ "SELECT actor_id, message FROM stream WHERE "
               ++ "source_id in (SELECT target_id FROM connection "
               ++ " WHERE source_id="++ (show uid)++") LIMIT 50"
    name <- fql_query $ "SELECT name, id FROM profile where id in  (SELECT actor_id FROM stream WHERE "
               ++ "source_id in (SELECT target_id FROM connection "
               ++ " WHERE source_id="++ (show uid)++") LIMIT 50)"
    let Ok a =  decode stream
    let Ok names =  decode name
    let idmap = foldr (\obj -> M.insert (obj!^ "id") (obj !* "name"))  M.empty  names
    return (a, idmap) 

fetchUserStream :: UID -> FacebookM ([JSValue], M.Map Integer String)
fetchUserStream uid =  do
    stream <- fql_query $ "SELECT actor_id, message FROM stream WHERE "
               ++ "source_id="++ show uid++" LIMIT 50"
    name <- fql_query $ "SELECT name, id FROM profile where id in  (SELECT actor_id FROM stream WHERE "
               ++ "source_id = "++show uid++"LIMIT 50)"
    let Ok a =  decode stream
    let Ok names =  decode name
    let idmap = foldr (\obj -> M.insert (obj!^ "id") (obj !* "name"))  M.empty  names
    return (a, idmap) 


