
  EApp "PDF" [ urlE, pageE ] -> do
    ESLit url <- eval model urlE
    EILit page <- eval model pageE
    imagePath <- PDF.magicPdf url page
    -- Retrieve the PDF from url
    -- Convert PDF to a collection of PNGs
    -- Pick the appropriate PNG
    -- Save it in Minicell's cache
    -- Return the address so that cells can render it
    return (EImage $ imagePath)

  EApp "CROP" [ imgE, wE, hE, x0E, y0E ] -> do
    EImage sourceImagePath <- eval model imgE

    EILit x0 <- eval model x0E
    EILit y0 <- eval model y0E

    EILit w <- eval model wE
    EILit h <- eval model hE

    imagePath <- PDF.crop sourceImagePath (w, h, x0, y0)
    return (EImage $ imagePath)