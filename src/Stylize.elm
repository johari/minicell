module Stylize exposing (extractHostName, stylizeHostname, vgrep, vgrepRender)

import Html exposing (Html, b, br, button, code, div, h1, hr, input, li, ol, strong, text, ul)
import Html.Attributes exposing (class)
import Url



-- selectedToken
-- unselectedToken
--
-- stylizeTags ts =


vgrepRender splits needle haystack label =
    case splits of
        [ x ] ->
            Html.span [] [ text x ]

        x :: xs ->
            Html.span []
                [ Html.span [] [ text x ]
                , Html.span [ class label ] [ Html.b [] [ text needle ] ]
                , vgrepRender xs needle haystack label
                ]

        [] ->
            text ""


vgrep haystack needle label =
    if haystack == "" then
        text ""

    else if needle == "" then
        text haystack

    else
        vgrepRender (String.split needle haystack) needle haystack label


stylizeHostname href =
    let
        hn =
            extractHostName href
    in
    vgrep href hn "label_url"


extractHostName : String -> String
extractHostName d =
    case Url.fromString d of
        Just url ->
            url.host

        Nothing ->
            ""
