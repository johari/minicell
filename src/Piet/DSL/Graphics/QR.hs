

# ifdef darwin_HOST_OS
# else
  EApp "QR" [ _payloadE ] -> do
    payloadE <- eval model _payloadE
    case payloadE of
      ESLit payloadS -> do
        let Just mat = Codec.QRCode.encode (defaultQRCodeOptions M) Iso8859_1 payloadS
        let imageURI = toPngDataUrlS 30 30 mat

        return $ EImage imageURI
# endif