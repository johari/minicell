{-# LANGUAGE OverloadedStrings, StandaloneDeriving, RecordWildCards,
    GADTs, TypeFamilies, MultiParamTypeClasses, DeriveDataTypeable,
    FlexibleInstances #-}
-- QSem was deprecated in 7.6, but no more
{-# OPTIONS_GHC -fno-warn-deprecations #-}
module Piet.DSL.Media.Flickr.DataSource
  ( FlickrReq(..)
  , initGlobalState
  , Credentials(..)
  , UserAccessToken
  , AccessToken(..)
  ) where

import Piet.DSL.Media.Flickr.Remote

import Network.HTTP.Conduit
-- import Facebook as FB
import Control.Monad
import Control.Monad.Trans.Resource
import Data.Hashable
import Data.Typeable
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Conduit
import Data.Conduit.List hiding (mapM, mapM_)
import Data.Monoid
import Data.Aeson
import Control.Concurrent.Async
import Control.Concurrent.QSem
import Control.Exception

import Control.Monad.Trans.Reader (ReaderT(..), ask, mapReaderT)
import Control.Monad.IO.Class (MonadIO)

import Data.Text

import Haxl.Core

data FlickrReq a where
   GetObject      :: Id -> FlickrReq Object
   GetUser        :: UserId -> FlickrReq User
   GetUserFriends :: UserId -> FlickrReq [Friend]
  deriving Typeable


runFlickrT ::
     (MonadIO m)
  => Credentials -- ^ Your app's credentials.
  -> Manager -- ^ Connection manager (see 'H.withManager').
  -> FlickrT Auth m a
  -> m a
runFlickrT creds manager (F act) = do
  apiref <- newIORef defaultApiVersion
  runReaderT act (FlickrData (Just creds) manager)

deriving instance Eq (FlickrReq a)
deriving instance Show (FlickrReq a)

instance ShowP FlickrReq where showp = show

instance Hashable (FlickrReq a) where
  hashWithSalt s (GetObject (Id id))      = hashWithSalt s (0::Int,id)
  hashWithSalt s (GetUser (Id id))        = hashWithSalt s (1::Int,id)
  hashWithSalt s (GetUserFriends (Id id)) = hashWithSalt s (2::Int,id)


instance StateKey FlickrReq where
  data State FlickrReq =
    FlickrState
       { credentials :: Credentials
       , userAccessToken :: UserAccessToken
       , manager :: Manager
       , semaphore :: QSem
       }

instance DataSourceName FlickrReq where
  dataSourceName _ = "Facebook"

instance DataSource u FlickrReq where
  fetch = flickrFetch

initGlobalState
  :: Int
  -> Credentials
  -> UserAccessToken
  -> IO (State FlickrReq)

initGlobalState threads creds token = do
  manager <- newManager tlsManagerSettings
  sem <- newQSem threads
  return FlickrState
    { credentials = creds
    , manager = manager
    , userAccessToken = token
    , semaphore = sem
    }

flickrFetch
  :: State FlickrReq
  -> Flags
  -> u
  -> PerformFetch FlickrReq

flickrFetch FlickrState{..} _flags _user =
  BackgroundFetch $
    mapM_ (fetchAsync credentials manager userAccessToken semaphore)

fetchAsync
  :: Credentials -> Manager -> UserAccessToken -> QSem
  -> BlockedFetch FlickrReq
  -> IO ()
fetchAsync creds manager tok sem (BlockedFetch req rvar) =
  void $ async $ bracket_ (waitQSem sem) (signalQSem sem) $ do
    e <- Control.Exception.try $
           runResourceT $ runFlickrT creds manager $ fetchFlickrReq tok req
    case e of
      Left ex -> putFailure rvar (ex :: SomeException)
      Right a -> putSuccess rvar a

fetchFlickrReq
  :: UserAccessToken
  -> FlickrReq a
  -> FlickrT Auth (ResourceT IO) a

fetchFlickrReq tok (GetObject (Id id)) =
  getObject ("/" <> id) [] (Just tok)

fetchFlickrReq _tok (GetUser id) =
  getUser id [] Nothing

fetchFlickrReq tok (GetUserFriends id) = do
  f <- getUserFriends id [] tok
  source <- fetchAllNextPages f
  source $$ consume