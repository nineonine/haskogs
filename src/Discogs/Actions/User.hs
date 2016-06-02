-- | Contains Database related actions

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
import qualified Discogs.Request as Req


-- IDENTITY SECTION --

-- | Retrieve basic information about the authenticated user.
--   <https://www.discogs.com/developers/#page:user-identity,header:user-identity-identity>
identity :: Monad m => DiscogsT m Identity
identity = runRequest Req.getIdentity

-- | Retrieve a user by username.
--   <https://www.discogs.com/developers/#page:user-identity,header:user-identity-profile-get>
profile :: Monad m
        => Text -- ^ The username of whose profile you are requesting.
        -> DiscogsT m User.User
profile usr = runRequest $ Req.getProfile usr

-- | Edit a user’s profile data.
--   Authentication as the user is required.
--   <https://www.discogs.com/developers/#page:user-identity,header:user-identity-profile-post>
editProfile :: Monad m
            => Text -- ^ The username of the user.
            -> Maybe Params -- ^ Optional Params
            -> DiscogsT m User.User
editProfile usr ps = runRequest $ Req.postProfile usr ps

-- | Retrieve a user’s submissions by username.
--   Accepts Pagination parameters.
--   <https://www.discogs.com/developers/#page:user-identity,header:user-identity-user-submissions-get>
submissions :: Monad m
            => Text -- ^ The username of the submissions you are trying to fetch.
            -> DiscogsT m Submissions
submissions usr = runRequest $ Req.getUserSubmissions usr

-- | Retrieve a user’s contributions by username.
--   Accepts Pagination parameters.
--   <https://www.discogs.com/developers/#page:user-identity,header:user-identity-user-contributions-get>
contributions :: Monad m
              => Text -- ^ The username of the submissions you are trying to fetch.
              -> DiscogsT m Contributions
contributions usr = runRequest $ Req.getUserContributions usr


-- COLLECTION SECTION --

-- | Retrieve a list of folders in a user’s collection.
--   If the collection has been made private by its owner, authentication as the collection owner is required.
--   If you are not authenticated as the collection owner, only folder ID 0 (the “All” folder) will be visible (if the requested user’s collection is public).
--   <https://www.discogs.com/developers/#page:user-collection,header:user-collection-collection-get>
collection :: Monad m
           => Text -- ^ The username of the collection you are trying to retrieve.
           -> DiscogsT m Collection
collection usr = runRequest $ Req.getCollection usr

-- | Create a new 'Folder' in a user’s collection.
--   Authentication as the collection owner is required.
--   <https://www.discogs.com/developers/#page:user-collection,header:user-collection-collection-post>
createFolder :: Monad m
             => Text -- ^ The username of the collection you are trying to retrieve.
             -> Text -- ^ The name of the newly-created folder.
             -> DiscogsT m Folder
createFolder usr fname = runRequest $ Req.postFolder usr fname

-- | Retrieve metadata about a 'Folder' in a user’s collection.
--   If /folder_id/ is not /0/, authentication as the collection owner is required.
--   <https://www.discogs.com/developers/#page:user-collection,header:user-collection-collection-folder-get>
folder :: Monad m
       => Text -- ^ The username of the collection you are trying to request.
       -> Int -- ^ The ID of the folder to request.
       -> DiscogsT m Folder
folder usr fname = runRequest $ Req.getCollectionFolder usr fname

-- | Edit a folder’s metadata. Folders 0 and 1 cannot be renamed.
--   Authentication as the collection owner is required.
--   <https://www.discogs.com/developers/#page:user-collection,header:user-collection-collection-folder-post>
editFolder :: Monad m
           => Text -- ^ The username of the collection you are trying to modify.
           -> Int -- ^ The ID of the folder to modify.
           -> Maybe Params  -- ^ Optional Params
           -> DiscogsT m Folder
editFolder usr fid ps = runRequest $ Req.editFolder usr fid ps

-- | Delete a folder from a user’s collection. A folder must be empty before it can be deleted.
--   Authentication as the collection owner is required.
--   <https://www.discogs.com/developers/#page:user-collection,header:user-collection-collection-folder-delete>
deleteFolder :: Monad m
             => Text -- ^ The username of the collection you are trying to modify.
             -> Int -- ^ The ID of the folder to delete.
             -> DiscogsT m ()
deleteFolder usr fid = runRequest $ Req.deleteFolder usr fid

-- | Returns the list of releases in a folder in a user’s collection.
--   Accepts Pagination parameters.
--   <https://www.discogs.com/developers/#page:user-collection,header:user-collection-get-collection-releases-get>
collectionReleases :: Monad m
                   => Text -- ^ The username of the collection you are trying to request.
                   -> Int -- ^ The ID of the folder to request.
                   -> Maybe Params -- ^ Optional Params.
                   -> DiscogsT m UserReleases
collectionReleases usr fid ps = runRequest $ Req.getCollectionRels usr fid ps

-- | Add a release to a folder in a user’s collection.
--   The folder_id must be non-zero – you can use 1 for “Uncategorized”.
--   Authentication as the collection owner is required.
--   <https://www.discogs.com/developers/#page:user-collection,header:user-collection-add-to-collection-folder-post>
addReleaseToFolder :: Monad m
                   => Text -- ^ The username of the collection you are trying to modify.
                   -> Int -- ^ The ID of the folder to modify.
                   -> Int -- ^ The ID of the release you are adding.
                   -> DiscogsT m Value
addReleaseToFolder usr fid rid = runRequest $ Req.postReleaseToFolder usr fid rid

-- | Change the rating on a release and/or move the instance to another folder.
--   Authentication as the collection owner is required.
--   <https://www.discogs.com/developers/#page:user-collection,header:user-collection-change-rating-of-release-post>
rateRelease :: Monad m
            => Text -- ^ The username of the collection you are trying to modify.
            -> Int -- ^ The ID of the folder you are requesting.
            -> Int -- ^ The ID of the release you are modifying.
            -> Int -- ^ The ID of the instance.
            -> Maybe Params -> DiscogsT m ()
rateRelease usr fid rid iid ps = runRequest $ Req.postReleaseRating usr fid rid iid ps

-- | Remove an instance of a release from a user’s collection folder.
--   Authentication as the collection owner is required.
--   <https://www.discogs.com/developers/#page:user-collection,header:user-collection-delete-instance-from-folder-delete>
deleteReleaseFromFolder :: Monad m
                        => Text -- ^ The username of the collection you are trying to modify.
                        -> Int -- ^ The ID of the folder to modify.
                        -> Int -- ^ The ID of the release you are modifying.
                        -> Int -- ^ The ID of the instance.
                        -> DiscogsT m ()
deleteReleaseFromFolder usr fid rid iid = runRequest $ Req.deleteReleaseFromFolder usr fid rid iid

-- | Retrieve a list of user-defined collection notes fields.
--   If the collection has been made private by its owner, authentication as the collection owner is required.
--   If you are not authenticated as the collection owner, only fields with public set to true will be visible.
--   <https://www.discogs.com/developers/#page:user-collection,header:user-collection-list-custom-fields-get>
collectionNotesFields :: Monad m
                      => Text -- ^ The username of the collection you are trying to modify.
                      -> DiscogsT m Fields
collectionNotesFields usr = runRequest $ Req.getCollectionNotes usr

-- | Change the value of a notes field on a particular instance.
--   <https://www.discogs.com/developers/#page:user-collection,header:user-collection-edit-fields-instance-post>
editInstanceFields :: Monad m
                   => Text -- ^ The username of the collection you are trying to modify.
                   -> Text -- ^ The new value of the field.
                   -> Int -- ^ The ID of the folder to modify.
                   -> Int -- ^ The ID of the release you are modifying.
                   -> Int -- ^ The ID of the instance.
                   -> Int -- ^ The ID of the field.
                   -> DiscogsT m ()
editInstanceFields usr val fid rid iid fid' = runRequest $ Req.postInstanceFields usr val fid rid iid fid'


-- WANTLIST SECTION --

-- | Returns the list of releases in a user’s wantlist.
--   Accepts Pagination parameters.
--   The /notes/ field will be visible if you are authenticated as the wantlist owner.
--   <https://www.discogs.com/developers/#page:user-wantlist,header:user-wantlist-wantlist-get>
wantlist :: Monad m
         => Text -- ^ The username of the wantlist you are trying to fetch.
         -> DiscogsT m Wants
wantlist usr = runRequest $ Req.getUserWantList usr

-- | Add a release to a user’s wantlist.
--   Authentication as the wantlist owner is required.
--   <https://www.discogs.com/developers/#page:user-wantlist,header:user-wantlist-add-to-wantlist-put>
addToWantlist :: Monad m
              => Text -- ^ The username of the wantlist you are trying to fetch.
              -> Int -- ^ The ID of the release you are adding.
              -> Maybe Params -- ^ Optional Params.
              -> DiscogsT m UserRelease
addToWantlist usr rid ps = runRequest $ Req.putReleaseNotes usr rid ps

-- | Add a release to a user’s wantlist.
--   <https://www.discogs.com/developers/#page:user-wantlist,header:user-wantlist-add-to-wantlist-put>
editReleaseInWantlist :: Monad m
                      => Text -- ^ The username of the wantlist you are trying to fetch.
                      -> Int -- ^ The ID of the release you are adding.
                      -> Maybe Params -- ^ optional Params.
                      -> DiscogsT m UserRelease
editReleaseInWantlist usr rid ps = runRequest $ Req.putReleaseNotes usr rid ps

-- | Delete a release in a user's wantlist.
--   <https://www.discogs.com/developers/#page:user-wantlist,header:user-wantlist-add-to-wantlist-delete>
removeFromWantlist :: Monad m
                   => Text -- ^ The username of the wantlist you are trying to fetch.
                   -> Int -- ^ The ID of the release you are deleting.
                   -> DiscogsT m ()
removeFromWantlist usr rid = runRequest $ Req.deleteReleaseFromWantlist usr rid
