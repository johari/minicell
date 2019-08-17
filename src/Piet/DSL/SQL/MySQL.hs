
  EApp "MYSQL" [ _databaseE, _queryE ] -> do
    ESLit databaseE <- eval model _databaseE
    ESLit queryE    <- eval model _queryE

    conn <- connect (defaultConnectInfo { connectHost = "127.0.0.1"
                                        , connectPort = 3306
                                        , connectPassword = "pietpietpiet"
                                        , connectUser = "root"
                                        , connectDatabase = databaseE
                                        })


    -- from: the current page title that corresponds to `pl_from`
    -- to: pl_title
    -- value on edge: none at this point.
    -- xs <- query_ conn "SELECT cast(page_title as CHAR), cast(pl_title as CHAR) FROM `thesis_pagelinks` INNER JOIN thesis_page ON thesis_page.page_id = pl_from WHERE 1"

    xs <- query_ conn (fromString queryE)

    close conn

    {-
    newEdges <- forM xs $ \(linkFrom, linkTo) ->
      return $ ((linkFrom, linkTo, 0) :: (String, String, Int))

    let (newNodes, nm) = mkNodes new (concat $ (\(x, y, _) -> [x, y]) <$> newEdges)
    return $ EGraphFGL $ mkGraph newNodes (fromMaybe [] $ mkEdges nm newEdges)
    -}

    let cleanUpMysql x = case x of
                          (Only s) -> s
                          _ -> ""

    return $ EList $ (ESLit <$> cleanUpMysql <$> (xs))