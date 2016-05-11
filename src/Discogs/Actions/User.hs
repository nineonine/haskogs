{-# LANGUAGE OverloadedStrings #-}

module Discogs.Actions.User where

import Discogs.Types.Discogs
import Discogs.Tools

import Data.Aeson
import Data.Text

import Discogs.Types.User.Folder
import Discogs.Types.User.Identity
import Discogs.Types.User.Submission
import Discogs.Types.User.Collection
import Discogs.Types.User.Contribution
import Discogs.Types.User.Release
import Discogs.Types.User.Field
import Discogs.Types.User.Want
import qualified Discogs.Types.User as User
import qualified Discogs.Request.User as Req


-- IDENTITY SECTION --

identity :: Monad m => DiscogsT m Identity
identity = runRequest Req.getIdentity

profile :: Monad m => Text -> DiscogsT m User.User
profile usr = runRequest $ Req.getProfile usr

editProfile :: Monad m => Text -> Maybe Params -> DiscogsT m User.User
editProfile usr ps = runRequest $ Req.postProfile usr ps

submissions :: Monad m => Text -> DiscogsT m Submissions
submissions usr = runRequest $ Req.getUserSubmissions usr

contributions :: Monad m => Text -> DiscogsT m Contributions
contributions usr = runRequest $ Req.getUserContributions usr


-- COLLECTION SECTION --

collection :: Monad m => Text -> DiscogsT m Collection
collection usr = runRequest $ Req.getCollection usr

createFolder :: Monad m => Text -> Text -> DiscogsT m Folder
createFolder usr fname = runRequest $ Req.postFolder usr fname

folder :: Monad m => Text -> Int -> DiscogsT m Folder
folder usr fname = runRequest $ Req.getCollectionFolder usr fname

editFolder :: Monad m => Text -> Int -> Maybe Params -> DiscogsT m Folder
editFolder usr fid ps = runRequest $ Req.editFolder usr fid ps

deleteFolder :: Monad m => Text -> Int -> DiscogsT m ()
deleteFolder usr fid = runRequest $ Req.deleteFolder usr fid

collectionReleases :: Monad m => Text -> Int -> Maybe Params -> DiscogsT m Releases
collectionReleases usr fid ps = runRequest $ Req.getCollectionRels usr fid ps

addReleaseToFolder :: Monad m => Text -> Int -> Int -> DiscogsT m Value
addReleaseToFolder usr fid rid = runRequest $ Req.postReleaseToFolder usr fid rid

rateRelease :: Monad m => Text -> Int -> Int -> Int -> Maybe Params -> DiscogsT m ()
rateRelease usr fid rid iid ps = runRequest $ Req.postReleaseRating usr fid rid iid ps

deleteReleaseFromFolder :: Monad m => Text -> Int -> Int -> Int -> DiscogsT m ()
deleteReleaseFromFolder usr fid rid iid = runRequest $ Req.deleteReleaseFromFolder usr fid rid iid

collectionNotesFields :: Monad m => Text -> DiscogsT m Fields
collectionNotesFields usr = runRequest $ Req.getCollectionNotes usr

editInstanceFields :: Monad m => Text -> Text -> Int -> Int -> Int -> Int -> DiscogsT m ()
editInstanceFields usr val fid rid iid fid' = runRequest $ Req.postInstanceFields usr val fid rid iid fid'


-- WANTLIST SECTION --

wantlist :: Monad m => Text -> DiscogsT m Wants
wantlist usr = runRequest $ Req.getUserWantList usr

addToWantlist :: Monad m => Text -> Int -> Maybe Params -> DiscogsT m Release
addToWantlist usr rid ps = runRequest $ Req.putReleaseNotes usr rid ps

editReleaseInWantlist :: Monad m => Text -> Int -> Maybe Params -> DiscogsT m Release
editReleaseInWantlist usr rid ps = runRequest $ Req.putReleaseNotes usr rid ps

removeFromWantlist :: Monad m => Text -> Int -> DiscogsT m ()
removeFromWantlist usr rid = runRequest $ Req.deleteReleaseFromWantlist usr rid
