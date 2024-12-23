{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant where" #-}
module Lib
    ( someFunc
    ) where
import System.Environment (getArgs)
import Interpolation

someFunc :: IO ()

printer :: Show a => [a] -> IO ()
printer [] = putStr ""
printer (x:xs) = do
    print x
    printer xs


computeForLinear :: [b] -> [(b, b)]
computeForLinear a =
    zip a (drop 1 a)

computeForLagrange :: [b] -> [(b, b, b)]
computeForLagrange a =
    zip3 a (drop 1 a) (drop 2 a)

someFunc = do
    args <- getArgs
    if length args /= 2
        then putStrLn "Usage: stack exec haskell-io-exe <step> <method>"
        else do
            let step = read (head args) :: Double
            let method = mod (read (head $ tail args) :: Integer) 2
            let content = fmap lines getContents
            string_content <- content
            let double_content = fmap formatter string_content where
                formatter l = (w1, w2) where
                    w1 = read $ head $ words l          :: Double
                    w2 = read $ head $ tail $ words l   :: Double
            let dataLinear =    computeForLinear    double_content
            let dataLagrange =  computeForLagrange  double_content
            let result = if method == 0 
                then fmap (lineInterpolation step) dataLinear 
                else fmap (lagrInterpolation step) dataLagrange
            mapM_ printer result