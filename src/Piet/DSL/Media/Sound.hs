  EApp "AUDIO" [ expr ] -> do
    ESLit src <- eval model expr

    return $ EAudio src

  EApp "AOLAY" [ expr1, expr2 ] -> do
    EAudio src1 <- eval model expr1
    EAudio src2 <- eval model expr2
    audioPath <- do
                    h <- getHomeDirectory
                    return (h ++ "/Dropbox/minicell-uploads/audio/")

    -- TODO:
    -- To avoid unnecessary recomputation
    -- Add a cache subsystem
    -- Then calculate md5 checksum of src1 and src2
    -- Then associate ("OLAY", checksum1, checksum2)
    -- to checksum of the output file

    targetPath <- withSystemTempDirectory "minicell-audio" $ \tmp -> do
                    readProcess "ffmpeg" [ "-i", mconcat [ audioPath, src1 ]
                                         , "-i", mconcat [ audioPath, src2 ]
                                         , "-filter_complex", "amerge"
                                         , "-ac", "2"
                                         , "-c:a", "libmp3lame"
                                         , "-q:a", "4"
                                         , "output.mp3"
                                         ] ""
                    fileContent <- LB.readFile "output.mp3"
                    let md5Digest = md5 fileContent
                    let targetPath = mconcat [audioPath, show md5Digest, ".mp3"]
                    copyFile "output.mp3" targetPath
                    return targetPath

    return (EAudio $ targetPath)

  EApp "ACONCAT" [ expr1, expr2 ] -> do
    EAudio src1 <- eval model expr1
    EAudio src2 <- eval model expr2
    audioPath <- do
                    h <- getHomeDirectory
                    return (h ++ "/Dropbox/minicell-uploads/audio/")

    let fullSrc1 = mconcat [ audioPath, src1 ]
        fullSrc2 = mconcat [ audioPath, src2 ]

    fileContent1 <- LB.readFile fullSrc1
    fileContent2 <- LB.readFile fullSrc2
    let cacheKey = mconcat ["ACONCAT", show $ md5 fileContent1, show $ md5 fileContent2 ]

    let targetPath = mconcat [ audioPath, cacheKey, ".mp3" ]

    exists <- doesFileExist targetPath

    if exists then return () else
      withSystemTempDirectory "minicell-audio" $ \tmp -> do
        print tmp
        readProcess "sox" [ fullSrc1
                          , fullSrc2
                          , "output.mp3"
                          ] ""

        copyFile "output.mp3" targetPath
        return ()

    return (EAudio $ targetPath)