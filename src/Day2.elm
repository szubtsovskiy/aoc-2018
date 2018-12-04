module Day1 exposing (main)

import Dict
import Html exposing (div, text)
import Parser exposing (..)
import Set


main =
    div []
        [ div [] [ text <| "Part 1: " ++ part1 ]
        , div [] [ text <| "Part 2: " ++ part2 ]
        ]


part1 : String
part1 =
    case boxIDs of
        Ok ids ->
            let
                recordChar c dict =
                    Dict.update c (\maybeCount -> Just <| 1 + Maybe.withDefault 0 maybeCount) dict

                keepDoublesAndTriples c count set =
                    if (count == 2 || count == 3) && not (Set.member count set) then
                        Set.insert count set

                    else
                        set

                toSet id =
                    String.foldl recordChar Dict.empty id
                        |> Dict.foldl keepDoublesAndTriples Set.empty

                updateCounters set ( doublesSoFar, triplesSoFar ) =
                    case ( Set.member 2 set, Set.member 3 set ) of
                        ( True, True ) ->
                            ( doublesSoFar + 1, triplesSoFar + 1 )

                        ( True, False ) ->
                            ( doublesSoFar + 1, triplesSoFar )

                        ( False, True ) ->
                            ( doublesSoFar, triplesSoFar + 1 )

                        ( False, False ) ->
                            ( doublesSoFar, triplesSoFar )

                ( doubles, triples ) =
                    List.map toSet ids
                        |> List.foldl updateCounters ( 0, 0 )
            in
            String.fromInt (doubles * triples)

        Err deadEnds ->
            Debug.toString deadEnds


part2 : String
part2 =
    case boxIDs of
        Ok ids ->
            let
                maybeExtractCommonLetters currentID otherIDs =
                    case otherIDs of
                        nextID :: rest ->
                            let
                                extractCommonLetters l1 l2 result =
                                    case ( l1, l2 ) of
                                        ( c1 :: r1, c2 :: r2 ) ->
                                            if c1 == c2 then
                                                extractCommonLetters r1 r2 (c1 :: result)

                                            else
                                                extractCommonLetters r1 r2 result

                                        _ ->
                                            result

                                commonLetters =
                                    extractCommonLetters (String.toList currentID) (String.toList nextID) []
                            in
                            if List.length commonLetters == String.length currentID - 1 then
                                Just <| String.fromList <| List.reverse commonLetters

                            else
                                maybeExtractCommonLetters currentID rest

                        [] ->
                            Nothing

                findCommonLetters list =
                    case list of
                        first :: rest ->
                            case maybeExtractCommonLetters first rest of
                                Just result ->
                                    Just result

                                Nothing ->
                                    findCommonLetters rest

                        [] ->
                            Nothing
            in
            Maybe.withDefault "<no match>" <|
                findCommonLetters ids

        Err deadEnds ->
            Debug.toString deadEnds


boxIDs : Result (List DeadEnd) (List String)
boxIDs =
    let
        parser =
            loop [] parseStep
    in
    run parser puzzleInput


parseStep : List String -> Parser (Step (List String) (List String))
parseStep ids =
    oneOf
        [ end
            |> map (\_ -> Done <| List.reverse ids)
        , succeed identity
            |. chompUntilEndOr "\n"
            |. chompWhile (\c -> c == '\n')
            |> getChompedString
            |> map (\id -> Loop <| String.trim id :: ids)
        ]


puzzleInput : String
puzzleInput =
    """revtoubfniyhzsgxdowjwkqglp
       revtcgbfniyhzsvxdomjwkqmlp
       cevtcubfniyhqsgxdoakwkqmlp
       revtcubfniyhzsgxdtavwkqmep
       reutcuboniyhzmgxdoajwkqmlp
       revtcubfniyhzsgxxqajmkqmlp
       rwvtcvbfniyhzsgxdozjwkqmlp
       qevtcbbfniyhzsgxdoljwkqmlp
       rnvtcmbfniyhzsuxdoajwkqmlp
       revtcubfsiyhzsgxdaaewkqmlp
       revtcubfpiyhesgxhoajwkqmlp
       revtcubfnivhzsuxdoljwkqmlp
       retpcubwniyhzsgxdoajwkqmlp
       revrcubfniyhzsgxdyajhkqmlp
       revtcbbfniyhzsixdoajwvqmlp
       revtcubfniyhzsgxdoanmkqmpp
       jevtoubfnuyhzsgxdoajwkqmlp
       rpwtcubfniehzsgxdoajwkqmlp
       revhcubfniyhnsgxdoajwkxmlp
       revtcubfniyhzswxdodjwkqvlp
       reotcubfciyhzsgxdnajwkqmlp
       revtcubfniyhzsgxdbatwsqmlp
       rlvbcubfniyhzssxdoajwkqmlp
       rentcubfnyyhzsgxdozjwkqmlp
       revtcuufniyhasgxdohjwkqmlp
       jevtcubfniyhxsgxdoajwkqwlp
       ravtcubfnryhzfgxdoajwkqmlp
       reltcubfnvyhzsgxdoajwkumlp
       revtrubfnschzsgxdoajwkqmlp
       uevtcubanichzsgxdoajwkqmlp
       revtcubfniyhzdpxdoajwwqmlp
       revtcubfhiyhzsgxdoajgkqplp
       revtcubfniyxzygxdoajwkqmld
       revtcunfniyfzsgxdoajwkqwlp
       reqtcubfniyhzsgxdoajwfqmlj
       revtcubfniyhzagedaajwkqmlp
       revthuefniyhzsgxroajwkqmlp
       revtcrbfodyhzsgxdoajwkqmlp
       revtcubfniyhxsgxdlajwuqmlp
       revtrubfnirhzsgxdokjwkqmlp
       revtiubfniyhzagudoajwkqmlp
       jevtcubfniyhusgxsoajwkqmlp
       reetcubfniyhzsgxdoajvmqmlp
       pestcubfniyhzsgxdoajwkqglp
       revtcubfniyhzsgxdoiowkqalp
       revscubfniyhzsgxdoajwkoplp
       revtcubfnoyhzsgxdlajwkymlp
       rkvtcubfniyhzsgxdoajzkqhlp
       revtuubfniyhzsgxdojjwkqglp
       revtcubmniyhzsgydoajwkzmlp
       revtcybfnijhzsgxvoajwkqmlp
       rxftcubfnkyhzsgxdoajwkqmlp
       gertcubfniyhzsgxjoajwkqmlp
       revtcabfniygzdgxdoajwkqmlp
       levgcubfniyhzsgxdoalwkqmlp
       revtcubfniyhzslxdofjwkqxlp
       revtcybwniyhzsgxdoajwkqmlx
       devtcubpniyhzsgqdoajwkqmlp
       pevtcjbfniyhzsbxdoajwkqmlp
       revtcubfeiehzsgxdoafwkqmlp
       revwcubfniyhzsgxdoawekqmlp
       revtcubfniyussgxdoawwkqmlp
       revtcuafnczhzsgxdoajwkqmlp
       revtaubfniyhusgxdoajwkqilp
       revtcubfnidhzxgxdoajwkqmlt
       revtcubfniyhzsexdmajwnqmlp
       revtcubfnhyhzsgxdwxjwkqmlp
       revtalbfniyhzsgxdoajwbqmlp
       revtcubfniyazsgxdoajwkqcvp
       rcvtcubfniyhzwgxdoajwkqmsp
       revthubfniyhzxgxdoalwkqmlp
       revtcubfniyazsoxgoajwkqmlp
       revtcubkriyhzsgtdoajwkqmlp
       revtcubfniyhzsgxgeajwgqmlp
       heftcubfniypzsgxdoajwkqmlp
       revtclbfniyhzsgxdowjnkqmlp
       revtcubfnifhzsgxdoamwkqmhp
       revncubfniyhzsgxdoxjwiqmlp
       reitcurfniyhzsgxdoajwkrmlp
       revtfmbfniyhzsgxdoajwkqmbp
       revtcubfniahzsgxdoajwkqhtp
       rejtcubfhiyhzsgxdoajwkqmfp
       revtcuxfqiyhzsgxdoajwkqmlh
       revtcuzfniwhzsgxdoajwkqmcp
       revtcubfniyhzsmxdotjwkqmlx
       revtcubfniyhzzgxmoajwkqulp
       revtcuaffiyhzsgxdoajwkqmlj
       revtcxbfniyhzsaxdoajwkqflp
       revtjubfniyhzcrxdoajwkqmlp
       revtcunfniyhzsgxdfajwoqmlp
       revtcubfpiytzswxdoajwkqmlp
       revtcubfniyhzsgxdorjwiqmtp
       oevtcubfniyhzsgidoajwkqmlt
       revccubzniyhztgxdoajwkqmlp
       reircubfniwhzsgxdoajwkqmlp
       revtcubfniyhzsgxhyajwkqvlp
       revtcubfnpyhzsgxdoajwkvblp
       revtduvfniyhzsixdoajwkqmlp
       revtcebfniyhzsgydpajwkqmlp
       revtcubftiyhzsgxwkajwkqmlp
       revtcdbfniyuzsgxdoajlkqmlp
       revtcubfnvydjsgxdoajwkqmlp
       cevtcupfniypzsgxdoajwkqmlp
       revtcubfniyhzsgoeonjwkqmlp
       revtcsbfniyhzsgxdoyjwdqmlp
       revtcubfriyhzugxdoakwkqmlp
       revtcadfniohzsgxdoajwkqmlp
       revrcubfniyhzsguxoajwkqmlp
       ruvtcubfniyhzsxxdoahwkqmlp
       aevtcubfniyhzsgcdoajwkqdlp
       revtcubgniyhzwgxdoajpkqmlp
       revtcubfniyhzegxdoajwkumsp
       rlvtcubzniyhzsgxdoajwkqzlp
       revtfubfniyhzxgbdoajwkqmlp
       revtcubfniyszssxdoajwkymlp
       revtcubfniyhzsgxdoarskzmlp
       rewtcubfniyhzsgxdoajwkpmlh
       revtcubbniyhzsfxdxajwkqmlp
       yeitcubfniyhzsgxdrajwkqmlp
       revtcubfniyhzsrxnoajwkemlp
       revtcuefnqyhzsgxdoajwkqmbp
       revtcubfniyhzsuxdoajwdqnlp
       revtcujfnifhzsgxdoaswkqmlp
       revtcuyfniyhzsgxdoaswklmlp
       reeacubfniyhzsgxdoajwkqmfp
       revtcubvniyhzsgxdoauwkqmls
       revtpubkniyhzsgxdoajvkqmlp
       revtcubfnpyhzsgxdoavnkqmlp
       revtcobfnvyhzsfxdoajwkqmlp
       gevtcubfniyhzsgxcoajwkqmld
       rivtcubfniyhzqgxdpajwkqmlp
       rettgubfngyhzsgxdoajwkqmlp
       revtcuhfccyhzsgxdoajwkqmlp
       rertarbfniyhzsgxdoajwkqmlp
       rhftcybfniyhzsgxdoajwkqmlp
       revtcvjfniyhzsgxboajwkqmlp
       reetcubfnikhzsgxdoajwkqmsp
       revtwubfniyhusgxdoajwkqelp
       revtcdbfniyyzsgxdwajwkqmlp
       revtcurfniyhzsgxduajwkqmtp
       revtcuafneyhzsgxduajwkqmlp
       rpvtcubfziyhzsgxdoajwkqmep
       mevtcubfniyhzssxdoaywkqmlp
       reptcubfniypzsgsdoajwkqmlp
       revtcubfniyhnsgxdoajwcqelp
       revtcutfniyhzsdxdoajwkqmlr
       rpvtcuafniyhzsgxqoajwkqmlp
       revncubfniyhzsgxdoajwkqkpp
       rertcabfniyhzsgxdoejwkqmlp
       revockbfniymzsgxdoajwkqmlp
       revtsubfniyczsgxdoajwkqplp
       revrcubpniyhzbgxdoajwkqmlp
       revrculfniyhzsgxdoajwkrmlp
       revtlubfniyhzsgxdiajhkqmlp
       ravtcubfniyhzsgxdoajwftmlp
       revtcunfxiyhzssxdoajwkqmlp
       revscubfniypzsgxroajwkqmlp
       mevtzubfniyhysgxdoajwkqmlp
       reitcubfniyuzogxdoajwkqmlp
       revycubfniyhwsgxdoajwkqmlg
       revtcubfnyyhzsgxdoajwkomqp
       zevtcutfniyhzsgxcoajwkqmlp
       revtwubfniylzsgxdjajwkqmlp
       oevtcubfniyhzsgxdoaowkzmlp
       revtcubfniyhzsgxdxajwwqclp
       revtcuafniyhzsgxdlacwkqmlp
       revtcubfniyhzsgxdqrjlkqmlp
       revmcubfnvyhzsgxduajwkqmlp
       rgvvcubfniyhzxgxdoajwkqmlp
       revtcubfniyhzsgxdoakwiqmlz
       reztcubfniyhzsgxddajwnqmlp
       revtcrbfnayhzsgxdoajwxqmlp
       revtcuboncyxzsgxdoajwkqmlp
       revtczbfniybxsgxdoajwkqmlp
       yevtcubfniyhcsdxdoajwkqmlp
       reztcmbfniyhzsgxcoajwkqmlp
       restcubfliyhzsbxdoajwkqmlp
       restcubkniyhzsgxdomjwkqmlp
       reokhubfniyhzsgxdoajwkqmlp
       rejtiubfniyhzsnxdoajwkqmlp
       revtcubfuiyjzsgxdoajykqmlp
       revscubfniyhzsixdoajwkqhlp
       revtjuzfniyhzsgxdoajwkqilp
       revtcubfziyhzsgxdoajhgqmlp
       revtcubiniyhzsgldoacwkqmlp
       revtcubfngyhisgxdoajwkqmkp
       ruvtcubfniyhzsgxloajwkqplp
       rtvtcubfniqbzsgxdoajwkqmlp
       revtcubfniyhzegxdffjwkqmlp
       revtcumsniyhzsgxdoajwkqmsp
       rmvtcubfnhyhzsgxsoajwkqmlp
       revtcbbfniyhzsgxdoajwkqzgp
       rebtcjufniyhzsgxdoajwkqmlp
       rephcubfniyhzvgxdoajwkqmlp
       revtcpbfniyxzsgxdoajwkqmls
       revjcubfniyizsgxdoajwkqmcp
       revtcuqfniyhzsgxdoavwkqmdp
       rettcubfniyhzsgxdoojwkqmbp
       rkvtcubfmuyhzsgxdoajwkqmlp
       revtcubcniyhzngxdoajlkqmlp
       revxcubfpiyfzsgxdoajwkqmlp
       revtcubfniyhzsgkkoajwklmlp
       revtcubfniyhzsbxdoajwqqslp
       zecycubfniyhzsgxdoajwkqmlp
       revtcubfniyhzsggaoajwksmlp
       revtcubffiyhzspxdoajwkqmmp
       ruvtcubfniyhzsgxdoajwkamlu
       revtcubfnmyhzsgxjoajwuqmlp
       revtcubfniyhisgxdoajwkqjgp
       revthubfniyhzsgxdoajwkeolp
       ryvtgubfniyhzsgidoajwkqmlp
       reitiubfniyhzsgxdoajwkqmbp
       rektcubfniyhzsfxdoajpkqmlp
       revbcubfniykzsgxdoajwkqwlp
       revzyubfniyhzkgxdoajwkqmlp
       ravtcubfniyhzsgxdoajwkhmap
       revtcubfnfyhzsgxdvpjwkqmlp
       rhvtcnbfnibhzsgxdoajwkqmlp
       revtctbfniywzsgxroajwkqmlp
       revtcubfniyhzsfmdoabwkqmlp
       sevtcubfniynzsgxpoajwkqmlp
       revtcnbfniyhzzgxdoajwzqmlp
       revtcoofniyhzsgxdoajwkqmrp
       revtcubfaiynysgxdoajwkqmlp
       revtlubfniyizsnxdoajwkqmlp
       revtcubfnwyzzsgxdoajwkqmzp
       revtqubfjiyhzsgxdoajwkrmlp
       revtaubfniyhpsgxdoajwkqilp
       revncuufniwhzsgxdoajwkqmlp
       revtcubfngyhisgxdoauwkqmlp
       revtcubynqyhzdgxdoajwkqmlp
       revtcubfniykzsgxdoyjwkqmla
       revttubfniytzsghdoajwkqmlp
       rerzcujfniyhzsgxdoajwkqmlp
       revtcubtniydzsgxdoajwkpmlp
       revecubfniyhzsvxsoajwkqmlp
       revtcuvfniyhzsgsdaajwkqmlp
       revtcubfniyxzsgxdoajtkzmlp
       revtcukfxiyhzsgxdofjwkqmlp
       revtcubfnayhzugxdqajwkqmlp
       revtcbbfniyizsgxdoajwkqmop
       revtcubfnzyhzsgxdoajwoqmpp
       reitcnbfniyqzsgxdoajwkqmlp
       rektcubfniyhzsgxdgijwkqmlp
       revtcubfniyhpsaxdoajdkqmlp
       ckvtcubfniyhzsgxeoajwkqmlp
       revtcubfniyhzsgxdhajzknmlp
       revscubfniyhrsgxdoajwwqmlp
       revtcubfilyhzsgxdpajwkqmlp
       fevtcubyniyhzsgxdoajwkqmpp"""
