{-# LANGUAGE OverloadedStrings #-}

module Discogs.Request.User where

import Discogs.Tools

import Data.Aeson (object, (.=), encode)
import Prelude hiding (concat)
import Data.Text hiding (concat, pack, append)
import Network.HTTP.Client
import Data.ByteString hiding (pack)


-- IDENTITY SECTION --

getIdentity :: Request
getIdentity = secureReq { path = "/oauth/identity" }

getProfile :: Text -> Request
getProfile usr = secureReq { path = append "/users/" $ toBS usr }

postProfile :: Text -> Maybe Params -> Request
postProfile usrname ps = let reqBody = object $ optParams ps
                         in secureReq { path = concat [ "/users/" , toBS usrname ]
                          , method = "POST"
                          , requestHeaders = [("Content-Type", "application/json")]
                          , requestBody = RequestBodyLBS $ encode reqBody }

getUserSubmissions :: Text -> Request
getUserSubmissions usr = secureReq { path = concat [ "/users/" , toBS usr , "/submissions" ] }

getUserContributions :: Text -> Request
getUserContributions usr = secureReq { path = concat [ "/users/" , toBS usr , "/contributions" ] }


-- COLLECTION SECTION --

getCollection :: Text -> Request
getCollection usr = secureReq { path = concat [ "/users/" , toBS usr , "/collection/folders" ] }

postFolder :: Text -> Maybe Params -> Request
postFolder usr ps = let reqBody = object $ optParams ps
                     in secureReq { path = concat [ "/users/" , toBS usr , "/collection/folders" ]
                       , method = "POST"
                       , requestHeaders = [("Content-Type", "application/json")]
                       , requestBody = RequestBodyLBS $ encode reqBody }

getCollectionFolder :: Text -> Int -> Request
getCollectionFolder usr fid = secureReq { path = concat [ "/users/" , toBS usr , "/collection/folders/" , intToBs fid ] }

renameFolder :: Text -> Int -> Request
renameFolder usr fid = secureReq { path = concat [ "/users/" , toBS usr , "/collection/folders/" , intToBs fid ] , method = "POST" }

deleteFolder :: Text -> Int -> Request
deleteFolder usr fid = secureReq { path = concat [ "/users/" , toBS usr , "/collection/folders/" , intToBs fid ] , method = "DELETE" }

getCollectionRels :: Text -> Int -> Maybe Params -> Request
getCollectionRels usr fid ps = let reqBody = object $ optParams ps
                               in secureReq { path = concat [ "/users/" , toBS usr , "/collection/folders/" , intToBs fid , "/releases" ]
                                  , method = "GET"
                                  , requestHeaders = [("Content-Type", "application/json")]
                                  , requestBody = RequestBodyLBS $ encode reqBody }

postReleaseToFolder :: Text -> Int -> Int -> Request
postReleaseToFolder usr fid rid = secureReq { method = "POST"
                                , requestHeaders = [("Content-Type", "application/json")]
                                , path = concat [ "/users/" , toBS usr , "/collection/folders/"
                                , intToBs fid , "/releases/" , intToBs rid ] }

postReleaseRating :: Text -> Int -> Int -> Int -> Maybe Params -> Request
postReleaseRating usr fid rid iid ps = let reqBody = object $ optParams ps
                                       in secureReq { path = concat [ "/users/" , toBS usr , "/collection/folders/" , intToBs fid
                                                                    , "/releases/" , intToBs rid , "/instances/" , intToBs iid ]
                                         , method = "POST"
                                         , requestHeaders = [("Content-Type", "application/json")]
                                         , requestBody = RequestBodyLBS $ encode reqBody }

deleteReleaseFromFolder :: Text -> Int -> Int -> Int -> Request
deleteReleaseFromFolder usr fid rid iid = secureReq { method = "DELETE"
                                         , requestHeaders = [("Content-Type", "application/json")]
                                         , path = concat [ "/users/" , toBS usr , "/collection/folders/" , intToBs fid
                                                         , "/releases/" , intToBs rid , "/instances/" , intToBs iid ] }

getCollectionNotes :: Text -> Request
getCollectionNotes usr = secureReq { path = concat [ "/users/" , toBS usr , "/collection/fields" ] }

postInstanceFields :: Text -> Text -> Int -> Int -> Int -> Int -> Request
postInstanceFields usr val fid rid iid fid' = secureReq { method = "POST"
                                             , requestBody = RequestBodyLBS . encode $ object [ "value" .= val ]
                                             , requestHeaders = [("Content-Type", "application/json")]
                                             , path = concat [ "/users/" , toBS usr , "/collection/folders/" , intToBs fid
                                                             , "/releases/" , intToBs rid , "/instances/" , intToBs iid
                                                             , "/fields/" , intToBs fid' ] }


-- WANTLIST SECTION --

getUserWantList :: Text -> Request
getUserWantList usr = secureReq { path = concat [ "/users/" , toBS usr , "/wants" ] }

putReleaseNotes :: Text -> Int -> Maybe Params -> Request
putReleaseNotes usr rid ps = let reqBody = object $ optParams ps
                             in secureReq { method = "PUT"
                              , requestBody = RequestBodyLBS $ encode reqBody
                              , requestHeaders = [("Content-Type", "application/json")]
                              , path = concat [ "/users/" , toBS usr , "/wants/" , intToBs rid ] }

postReleaseToWantlist :: Text -> Int -> Maybe Params -> Request
postReleaseToWantlist usr rid ps = let reqBody = object $ optParams ps
                                   in secureReq { method = "POST"
                                    , requestBody = RequestBodyLBS $ encode reqBody
                                    , requestHeaders = [("Content-Type", "application/json")]
                                    , path = concat [ "/users/" , toBS usr , "/wants/" , intToBs rid ] }

deleteReleaseFromWantlist :: Text -> Int -> Request
deleteReleaseFromWantlist usr rid = secureReq { method = "DELETE"
                                   , requestHeaders = [("Content-Type", "application/json")]
                                   , path = concat [ "/users/" , toBS usr , "/wants/" , intToBs rid ] }
