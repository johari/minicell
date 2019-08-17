{-#LANGUAGE CPP#-}
{-#LANGUAGE DeriveDataTypeable#-}
{-#LANGUAGE FlexibleContexts#-}
{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE StandaloneDeriving #-}
{-#LANGUAGE GADTs #-}
{-#LANGUAGE DeriveAnyClass #-}
{-#LANGUAGE DeriveFunctor #-}


module Piet.DSL.Media.Flickr.Remote where

import Control.Monad (MonadPlus, liftM)
import Control.Monad.Fix (MonadFix)

import Control.Monad.Fail (MonadFail(..))

import Data.Time (UTCTime)

import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Data.Typeable (Typeable)

import Network.HTTP.Conduit as H
import qualified UnliftIO.Exception as E
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Resource as R
import qualified Data.Aeson as A
import qualified Data.Attoparsec.ByteString.Char8 as AT
import qualified Data.ByteString as B
import qualified Data.Conduit as C
import Data.Conduit ((.|))
import qualified Data.Conduit.Attoparsec as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT
import qualified Data.ByteString.Lazy as L

import Control.Monad.Trans.Reader (ReaderT(..), ask, mapReaderT)
import Control.Monad.IO.Class (MonadIO)

import Haxl.Core hiding (MonadFail)

-- | Internal data kept inside 'FlickrT'.
data FlickrData =
  FlickrData
    { flickrdCreds :: Maybe Credentials
    , flickrdManager :: !H.Manager
    , flickrdTier :: !FbTier
    -- , flickrdApiVersion :: IORef ApiVersion
    }
  deriving (Typeable)


type Haxl = GenHaxl () ()
newtype Id = Id { idcode :: Text } deriving (Eq, Show, Ord)
type UserId = Id

data User = User { handle :: String, uid :: UserId }
type Friend = User

data Name = Name Text
data Auth = Auth String

data FlickrT auth m a =
  F
    {  unF :: ReaderT FlickrData m a
    }
  deriving ( Functor
           , Applicative
           , Alternative
           , Monad
           , MonadFix
           , MonadPlus
           , MonadIO
           , MonadTrans
           , R.MonadThrow
           , MonadFail
           )

data Credentials = Credentials String String

-- | Phantom type used mark an 'AccessToken' as an user access
-- token.
data UserKind
  deriving (Typeable)

-- | Phantom type used mark an 'AccessToken' as an app access
-- token.
data AppKind
  deriving (Typeable)

-- | An access token.  While you can make some API calls without
-- an access token, many require an access token and some will
-- give you more information with an appropriate access token.
--
-- There are two kinds of access tokens:
--
-- [User access token] An access token obtained after an user
-- accepts your application.  Let's you access more information
-- about that user and act on their behalf (depending on which
-- permissions you've asked for).
--
-- [App access token] An access token that allows you to take
-- administrative actions for your application.
--
-- These two kinds of access tokens are distinguished by the
-- phantom type on 'AccessToken', which can be 'UserKind' or
-- 'AppKind'.


data AccessToken kind where
        UserAccessToken ::
          UserId -> AccessTokenData -> UTCTime -> AccessToken UserKind
        AppAccessToken :: AccessTokenData -> AccessToken AppKind

-- | Type synonym for @'AccessToken' 'UserKind'@.
type UserAccessToken = AccessToken UserKind

-- | Type synonym for @'AccessToken' 'AppKind'@.
type AppAccessToken = AccessToken AppKind

deriving instance Eq (AccessToken kind)

deriving instance Ord (AccessToken kind)

deriving instance Show (AccessToken kind)

deriving instance Typeable AccessToken

-- | The access token data that is passed to Facebook's API
-- calls.
type AccessTokenData = Text


-- | An exception that may be thrown by functions on this
-- package.  Includes any information provided by Facebook.
data FacebookException =
    -- | An exception coming from Facebook.
    FacebookException { fbeType    :: Text
                      , fbeMessage :: Text
                      }
    -- | An exception coming from the @fb@ package's code.
  | FbLibraryException { fbeMessage :: Text }
    deriving (Eq, Ord, Show, Read, Typeable)

instance A.FromJSON FacebookException where
    parseJSON (A.Object v) =
        FacebookException <$> v A..: "type"
                          <*> v A..: "message"
    parseJSON _ = mzero

instance E.Exception FacebookException where

type Argument = (ByteString, ByteString)

-- | Make a raw @GET@ request to Facebook's Graph API.
getObject
  :: (R.MonadResource m, R.MonadUnliftIO m, R.MonadThrow m, A.FromJSON a)
  => Text -- ^ Path (should begin with a slash @\/@)
  -> [Argument] -- ^ Arguments to be passed to Facebook
  -> Maybe (AccessToken anyKind) -- ^ Optional access token
  -> FlickrT anyAuth m a
getObject path query mtoken =
  runResourceInFb $ asJson =<< fbhttp =<< fbreq path mtoken query

-- | Run a 'ResourceT' inside a 'FlickrT'.
runResourceInFb ::
     (R.MonadResource m, MonadUnliftIO m)
  => FlickrT anyAuth (R.ResourceT m) a
  -> FlickrT anyAuth m a
runResourceInFb (F inner) = F $ ask >>= lift . R.runResourceT . runReaderT inner


-- | Which Facebook tier should be used (see
-- <https://developers.facebook.com/support/beta-tier/>).
data FbTier
  = Production
  | Beta
  deriving (Eq, Ord, Show, Read, Enum, Typeable)


-- | Get the 'H.Manager'.
getManager :: Monad m => FlickrT anyAuth m H.Manager
getManager = flickrdManager `liftM` F ask

-- | Try to parse the @WWW-Authenticate@ header of a Facebook
-- response.
wwwAuthenticateParser :: AT.Parser FacebookException
wwwAuthenticateParser =
    FacebookException <$  AT.string "OAuth \"Facebook Platform\" "
                      <*> text
                      <*  AT.char ' '
                      <*> text
    where
      text  = T.pack <$ AT.char '"' <*> many tchar <* AT.char '"'
      tchar = (AT.char '\\' *> AT.anyChar) <|> AT.notChar '"'


-- | Get the user's credentials.
getMCreds :: Monad m => FlickrT anyAuth m (Maybe Credentials)
getMCreds = flickrdCreds `liftM` F ask

-- | Get the 'FbTier'.
getTier :: Monad m => FlickrT anyAuth m FbTier
getTier = flickrdTier `liftM` F ask

-- | Run a pure function that depends on the 'FbTier' being used.
withTier :: Monad m => (FbTier -> a) -> FlickrT anyAuth m a
withTier = flip liftM getTier

-- | @True@ if the the 'Status' is ok (i.e. @2XX@).
isOkay :: HT.Status -> Bool
isOkay status =
  let sc = HT.statusCode status
  in 200 <= sc && sc < 300

-- | A plain 'H.Request' to a Facebook API.  Use this instead of
-- 'def' when creating new 'H.Request'@s@ for Facebook.
fbreq :: MonadIO m
      => Text                        -- ^ Path. Should start from "/".
      -> Maybe (AccessToken anyKind) -- ^ Access token.
      -> HT.SimpleQuery              -- ^ Parameters.
      -> FlickrT anyAuth m H.Request
fbreq path mtoken query = do

    apiVersion <- return $ "0.0.1"
    -- creds      <- getMCreds

    -- let appSecretProofAdder = case creds of
    --       Just c@( Credentials _ _ _ True ) -> addAppSecretProof c
    --       _ -> const id


    withTier $ \tier ->
      let host = case tier of
                   Production -> "graph.facebook.com"
                   Beta ->  "graph.beta.facebook.com"
      in H.defaultRequest { H.secure        = True
             , H.host          = host
             , H.port          = 443
             , H.path          = TE.encodeUtf8 ("/" <> apiVersion <> path)
             , H.redirectCount = 3
             -- , H.queryString   =
                 -- HT.renderSimpleQuery False
                 -- $ appSecretProofAdder mtoken $ maybe id tsq mtoken query
#if MIN_VERSION_http_client(0,5,0)
             , H.responseTimeout = H.responseTimeoutMicro 120000000 -- 2 minutes
#else
             , H.responseTimeout = Just 120000000 -- 2 minutes
#endif
             }


-- | Converts a plain 'H.Response' coming from 'H.http' into a
-- JSON value.
asJson :: (MonadIO m, MonadTrans t, R.MonadThrow m, A.FromJSON a) =>
          H.Response (C.ConduitT () ByteString m ())
       -> t m a
asJson = lift . asJsonHelper

asJsonHelper :: (MonadIO m, R.MonadThrow m, A.FromJSON a) =>
                H.Response (C.ConduitT () ByteString m ())
             -> m a
asJsonHelper response = do
#if DEBUG
  bs <- H.responseBody response C.$$+- fmap L.fromChunks CL.consume
  _ <- liftIO $ printf "asJsonHelper: %s\n" (show bs)
  val <- either (fail . ("asJsonHelper: A.decode returned " ++)) return (A.eitherDecode bs)
#else
  val <- C.runConduit $ (H.responseBody response) .| C.sinkParser A.json'
#endif
  case A.fromJSON val of
    A.Success r -> return r
    A.Error str ->
        E.throwIO $ FbLibraryException $ T.concat
             [ "Facebook.Base.asJson: could not parse "
             , " Facebook's response as a JSON value ("
             , T.pack str, ")" ]


-- | Same as 'H.http', but tries to parse errors and throw
-- meaningful 'FacebookException'@s@.
fbhttp :: (R.MonadResource m, R.MonadUnliftIO m, R.MonadThrow m) =>
          H.Request
       -> FlickrT anyAuth m (H.Response (C.ConduitT () ByteString m ()))
fbhttp req = do
  manager <- getManager
  lift (fbhttpHelper manager req)

fbhttpHelper :: (R.MonadResource m, R.MonadUnliftIO m, R.MonadThrow m) =>
                H.Manager
             -> H.Request
             -> m (H.Response (C.ConduitT () ByteString m ()))
fbhttpHelper manager req = do
#if MIN_VERSION_http_client(0,5,0)
  let req' = req { H.checkResponse = \_ _ -> return () }
#else
  let req' = req { H.checkStatus = \_ _ _ -> Nothing }
#endif
#if DEBUG
  _ <- liftIO $ printf "fbhttp doing request\n\tmethod: %s\n\tsecure: %s\n\thost: %s\n\tport: %s\n\tpath: %s\n\tqueryString: %s\n\trequestHeaders: %s\n" (show $ H.method req') (show $ H.secure req') (show $ H.host req') (show $ H.port req') (show $ H.path req') (show $ H.queryString req') (show $ H.requestHeaders req')
#endif
  response <- H.http req' manager
  let status  = H.responseStatus    response
      headers = H.responseHeaders   response
#if DEBUG
  _ <- liftIO $ printf "fbhttp response status: %s\n" (show status)
#endif
  if isOkay status
    then return response
    else do
#if MIN_VERSION_http_client(0,5,0)
      fullResp <- C.runConduit $ (H.responseBody response) .| CB.sinkLbs
      let res' = fmap (const ()) response
      let statusexc = H.HttpExceptionRequest req $ H.StatusCodeException res' (L.toStrict fullResp)
#else
      let cookies = H.responseCookieJar response
      let statusexc = H.StatusCodeException status headers cookies
#endif

      val <- E.try $ asJsonHelper response
      case val :: Either E.SomeException FacebookException of
        Right fbexc -> E.throwIO fbexc
        Left _ -> do
          case AT.parse wwwAuthenticateParser <$>
               lookup "WWW-Authenticate" headers of
            Just (AT.Done _ fbexc) -> E.throwIO fbexc
            _                      -> E.throwIO statusexc

