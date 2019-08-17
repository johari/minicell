module Piet.DSL.Media.Flickr where

import Spreadsheet.Types

import Piet.DSL.Media.Flickr.DataSource
import Data.Aeson
-- import Facebook (Id(..), Friend(..), User(..))

import Haxl.Core

main = do
  (creds, access_token) <- getCredentials

  facebookState <- initGlobalState 10 creds access_token



  env <- initEnv (stateSet facebookState stateEmpty) ()

eval' :: (Spreadsheet -> Formula -> IO Formula) -> Spreadsheet -> Formula -> IO Formula
eval' eval model expr = case normalizeOp expr of
  EApp "FLICKR" _ -> do

  	r <- runHaxl env $ do
  	  favs <- getObject "me/favs"
  	  mapM getObject (favIds favs)      -- these happen concurrently
  	return $ EList r

  _ -> return $ ENotImplemented



favIds :: Object -> [Id]
favIds favs = do
  Array arr <- [favs ! "data"]
  Object obj <- Vector.toList arr
  String id <- [obj ! "id"]
  return (Id id)

-- | Fetch an arbitrary object in the Facebook graph.
getObject :: Id -> GenHaxl u w Object
getObject id = dataFetch (GetObject id)

-- | Fetch a Facebook user.
getUser :: Id -> GenHaxl u w User
getUser id = dataFetch (GetUser id)

-- | Fetch the friends of a Facebook user that are registered with the
-- current app.
getUserFriends :: Id -> GenHaxl u w [Friend]
getUserFriends id = dataFetch (GetUserFriends id)