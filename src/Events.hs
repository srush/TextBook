{-# LANGUAGE DeriveDataTypeable #-}
module Events where 

import Data.Time.Clock.POSIX
import Client.Facebook
import Client.Login
import Text.JSON
import Text.JSON.Generic
import FUtil


data Event = Event
    {name:: String,
     tagline:: Maybe String,
     description:: Maybe String
    } deriving (Data, Typeable, Show)


fetchEvents ::  UID -> FacebookM [Event]
fetchEvents uid = do 
    time <- io getPOSIXTime
    events <- fql_query $ "SELECT name, tagline, description FROM event where eid in (select eid from event_member where uid = "++ show uid ++ " ) and end_time > " ++ (show $ round time) 
    return $ decodeJSON events
