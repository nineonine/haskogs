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
import qualified Discogs.Request.Database as Req

release :: Monad m => Int -> DiscogsT m Release
release n = runRequest $ Req.getRelease n

artist :: Monad m => Int -> DiscogsT m Artist
artist n = runRequest $ Req.getArtist n

master :: Monad m => Int -> DiscogsT m Master
master n = runRequest $ Req.getMaster n

label :: Monad m => Int -> DiscogsT m Label
label n = runRequest $ Req.getLabel n

releaseVersions :: Monad m => Int -> DiscogsT m Versions
releaseVersions n = runRequest $ Req.getReleaseVersions n

artistReleases :: Monad m => Int -> DiscogsT m AR.ArtistReleases
artistReleases n = runRequest $ Req.getArtistReleases n

labelReleases :: Monad m => Int -> DiscogsT m LR.LabelReleases
labelReleases n = runRequest $ Req.getLabelReleases n

search :: Monad m => Params -> DiscogsT m SearchResult
search ps = runRequest $ Req.search ps
