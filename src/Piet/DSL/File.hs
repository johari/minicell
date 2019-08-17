
  EApp "DIR" _ -> do
    -- path <- eval model pathE
    -- print path
    listOfFiles <- getDirectoryContents "/minibox/"
    print listOfFiles
    return $ EList (EImage <$> listOfFiles)



  -- EApp "TAR" _ -> do
  --   PietTar.example
  --   return $ EString "success!"
