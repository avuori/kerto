{-
 -         kerto 0.1 - Kertolaskujen harjoitteluohjelma
 -         Copyright Arto Vuori, 2008
 -
 - Ohjelma kysyy kertolaskuja x*y, jossa x,y väliltä [2..9].
 - Ohjelma antaa lopussa pisteet.
 -
 -            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
 -                  Version 2, December 2004
 -
 - Copyright (C) 2004 Sam Hocevar
 - 14 rue de Plaisance, 75014 Paris, France
 - Everyone is permitted to copy and distribute verbatim or modified
 - copies of this license document, and changing it is allowed as long
 - as the name is changed.
 -
 -           DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
 -  TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
 -
 - 0. You just DO WHAT THE FUCK YOU WANT TO.
 -
 -
 - Build instructions (ghc 6.8.2):
 -   ghc --make Kerto.hs -o kerto
 -}
module Main
    where

import Data.Char (isSpace)
import IO
import System.Random
import Text.Printf
import Text.Regex.Posix

main :: IO ()

main = do
    putStrLn "----------------------------------------"
    putStrLn "Tervetuloa harjoittelemaan kertolaskuja!"
    putStrLn "Lopeta ohjelma komennolla 'q'."
    putStrLn "----------------------------------------"
    loop 0 0 0 0

loop :: Int -> Int -> Int -> Int -> IO ()

loop 0 _ score n = do
    r <- getStdGen
    let (x1,r2) = randomR(2,9) r
        (x2,r3) = randomR(2,9) r2
        in do setStdGen r3
              check x1 x2 score n

loop x1 x2 score n = check x1 x2 score n

check :: Int -> Int -> Int -> Int -> IO ()

check x1 x2 score n = do
    printf "%d*%d = ?\n" x1 x2
    rawAns <- getLine
    let ans = trim rawAns
    case ans of
         'q':_ -> do printf "Näkemiin! Tuloksesi oli %d/%d.\n" score n
                     return ()
         _     -> if (ans =~ "^[0-9]+$" :: Bool) 
                    && (read ans::Int) == rightAnswer
                        then do putStrLn "Oikein!"
                                loop 0 0 (succ score) $ succ n
                        else do putStrLn "Väärin!"
                                loop x1 x2 score $ succ n
    where rightAnswer = x1*x2

-- from http://en.wikipedia.org/wiki/Trim_(programming)#Haskell
trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace
