-- | Contains Database related actions
module Discogs.Actions.Database where

import Discogs.Tools
import Discogs.Types.Discogs
import Discogs.Types.Release
import Discogs.Types.Release.Master
import Discogs.Types.Release.Versions
import Discogs.Types.Artist
import Discogs.Types.Label
import Discogs.Types.Search
import qualified Discogs.Types.Artist.Release as AR
import qualified Discogs.Types.Label.Release as LR
import qualified Discogs.Request as Req

-- | Get a 'Release'
--   <https://www.discogs.com/developers/#page:database,header:database-release>
release :: Monad m
        => Int -- ^ Release ID
        -> DiscogsT m Release
release n = runRequest $ Req.getRelease n

-- | Get an 'Artist'
--   <https://www.discogs.com/developers/#page:database,header:database-artist>
artist :: Monad m
       => Int -- ^ The Artist ID
       -> DiscogsT m Artist
artist n = runRequest $ Req.getArtist n

-- | Get a 'Master' release
--  <https://www.discogs.com/developers/#page:database,header:database-master-release>
master :: Monad m
       => Int -- ^ The Master ID
       -> DiscogsT m Master
master n = runRequest $ Req.getMaster n

-- | Get a 'Label'
--  <https://www.discogs.com/developers/#page:database,header:database-label>
label :: Monad m
      => Int -- ^ The Label ID
      -> DiscogsT m Label
label n = runRequest $ Req.getLabel n

-- | Retrieves a list of all Releases that are versions of this master.
--   Accepts Pagination parameters.
--   <https://www.discogs.com/developers/#page:database,header:database-master-release-versions>
releaseVersions :: Monad m
                => Int -- ^ The Master ID
                -> DiscogsT m Versions
releaseVersions n = runRequest $ Req.getReleaseVersions n

-- | Get an artist’s releases
--   Accepts Pagination parameters.
--   <https://www.discogs.com/developers/#page:database,header:database-artist-releases>
artistReleases :: Monad m
               => Int -- ^ The Artist ID
               -> DiscogsT m AR.ArtistReleases
artistReleases n = runRequest $ Req.getArtistReleases n

-- | Returns a list of Releases associated with the label.
--   Accepts Pagination parameters.
--   <https://www.discogs.com/developers/#page:database,header:database-all-label-releases-get>
labelReleases :: Monad m => Int -> DiscogsT m LR.LabelReleases
labelReleases n = runRequest $ Req.getLabelReleases n

-- | Issue a search query to database.
--   Accepts pagination parameters.
--   <https://www.discogs.com/developers/#page:database,header:database-search-get>
search :: Monad m
       => Params -- ^ 'Params' ~ [(Text, Text)]
       -> DiscogsT m SearchResult
search ps = runRequest $ Req.search ps
