

  EApp "UNIXEPOCH" _ -> do
    t <- getPOSIXTime
    return $ EILit (round t)
