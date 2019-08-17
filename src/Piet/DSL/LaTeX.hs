
  EApp "LATEX" ddd@[ECellRange (rhoL, kappaL) (rhoR, kappaR)] -> do
    let matrix = [ (rho, kappa) | rho <- [rhoL .. rhoR], kappa <- [kappaL .. kappaR] ]

    vals <- sequence [ do val <- eval model (ECellRef addr); return (addr, val) | addr <- matrix ]

    let ccc = "|" <> (intercalate "|" $ map (const "c") [kappaL .. (kappaR + 1)]) <> "|"

    let hBegin = "\\begin{tabular}{ " <> ccc <> " } \\hline % \\hline"
    let hC = (" & " <> (intercalate " & " (addrKappaToExcelStyle <$> [kappaL .. kappaR])))
    -- "cell1 & cell2 & cell3 \\\\"
    -- "cell4 & cell5 & cell6 \\\\"
    -- "cell7 & cell8 & cell9 \\\\"
    let hEnd = "\\end{tabular}"


    -- return $ ESLit $ intercalate "\n" ([h0, h1, hC] ++ [(rows vals)] ++ [h2, h3])
    return $ ESLit $ intercalate " \\\\ \\hline \n" [hBegin, hC, (rows vals), hEnd]

    where
      rows vals = intercalate "\\\\\n" $ map (printRow vals) [rhoL .. rhoR]
      printRow vals rho = intercalate " & " $ [show (rho+1)] <> (eexprToLaTeX <$> ((flip lookup) vals) <$> ((rho,) <$> [kappaL .. kappaR]))
      eexprToLaTeX val =
        case val of
          Just v ->
            case v of
              ESLit s -> s
              EILit i -> show i
              EError _ -> ""
              EGraphFGL _ -> "$\\mathbf{G}$"
              _ -> show v
          _ -> ""