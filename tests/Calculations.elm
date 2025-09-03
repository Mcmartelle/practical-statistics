module Calculations exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Main


suite : Test
suite =
    describe "Calculating"
        [ test "Chance to Samples 50% 1/100" <| \_ ->
            Expect.equal (Main.chanceToSamples 0.5 1 100) "68.96756393652842"
        , test "Chance to Samples 50% 5/100000" <| \_ ->
            Expect.equal (Main.chanceToSamples 0.5 5 100000) "13862.597034721968"
        , test "Chance to Samples 90% 1/100" <| \_ ->
            Expect.equal (Main.chanceToSamples 0.9 1 100) "229.10528827669427"
        , test "Chance to Samples 99% 1/9001" <| \_ ->
            Expect.equal (Main.chanceToSamples 0.99 1 9001) "41448.8342163405"
        , test "Samples to Chance 10 samples 1/100" <| \_ ->
            Expect.equal (Main.samplesToChance 10 1 100) "9.561792499119559"
        , test "Samples to Chance 100 samples 1/100" <| \_ ->
            Expect.equal (Main.samplesToChance 100 1 100) "63.396765872677086"
        , test "Samples to Chance 41448 samples 1/9001" <| \_ ->
            Expect.equal (Main.samplesToChance 41448 1 9001) "98.99990731014915"
        , test "Samples to Chance 100 samples 5/100000" <| \_ ->
            Expect.equal (Main.samplesToChance 100 5 100000) "0.49876451880153105"
        ]

