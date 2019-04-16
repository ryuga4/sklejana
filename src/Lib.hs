{-# LANGUAGE FlexibleContexts #-}
module Lib
    ( someFunc
    ) where

import Math.LinearEquationSolver

import Plots
import Diagrams.Prelude
import Diagrams.Backend.Rasterific.CmdLine
import Control.Arrow
import Data.Typeable
import Debug.Trace


someFunc :: IO ()
someFunc = do
  let xs = [0,1/20 * pir..2*pir]
  let as = map (toRational . sin . fromRational) xs 
  Just x <- akcja xs as -- obliczenie współczynników wszystkich składowych wielomianów
  let r = fRange (length xs) xs x [0,pir/20..2*pir] -- zmapowanie listy wartości
  -- za pomocą funkcji składanej, utworzonej na podstawie obliczonych współczynników
  r2AxisMain $ plot r -- wygenerowanie wykresu
  return ()



plot :: [(Rational,Rational)] -> Axis B V2 Double
plot d1 = r2Axis &~ do
  linePlot' $ map (\(a,b) -> (fromRational a, fromRational b)) d1 -- czerwony wykres interpolowanej funkcji
  linePlot (map (id &&& sin) [0,0.1..2*pi]) $ do -- niebieski wykres "poprawnej" funkcji, umieszczony dla porównania
               plotColor .= blue



pir = toRational pi






akcja xs ys = uncurry (solveRationalLinearEqs Yices) $ gen xs ys  


fRange n xs as l = map (id &&& getF n xs as) l
getF n xs as x = getF' xs (drop n as) x


getF' [_] as x = sum $ zipWith (\a n -> a*(x^n)) as [0..3]
getF' (h:t) as x | Debug.Trace.trace (show $ map fromRational $ take 4 as) False = undefined
                 | x <= h = sum $ zipWith (\a n -> a*x^n) as [0..3] 
                 | otherwise = getF' t (drop 4 as) x




gen :: [Rational] -> [Rational] -> ([[Rational]],[Rational])
gen xs ys = (part1 ++ part2 ++ part22 ++ part3 ++ part4 ++ part5, replicate (4*(length xs) - 4) 0 ++ ys)
  where part1 = do
          let l = length xs
          n <- [0.. l - 3]
          let prefix = replicate l 0 ++ replicate (n*4) 0
          let suffix = replicate ((l-n-3) * 4) 0
          let mid = map ((xs !! (n + 1))^) [0..3]
          let mid' = 0 : map (\x -> (toRational x+1)*(xs !! (n+1))^x) [0..2]
          let mid'' = [0,0] ++ map (\x -> (toRational x+1)*(toRational x+2)*(xs !! (n+1))^x) [0,1]
          let base =  prefix ++ mid ++ map negate mid ++ suffix
          let prim = prefix ++ mid' ++ map negate mid' ++ suffix
          let bis = prefix ++ mid'' ++ map negate mid'' ++ suffix
          [base,prim,bis]
        part2 = do
          let l = length xs
          n <- [0.. l-2]
          let prefix = replicate n 0 ++ [- (ys !! n)] ++ replicate (l-n-1) 0 ++ replicate (n*4) 0
          let suffix = replicate ((l-n-2) * 4) 0
          let mid = map ((xs !! n)^) [0..3]
          return $ prefix ++ mid ++ suffix
        part22 = do
          let l = length xs
          n <- [l-1]
          let prefix = replicate n 0 ++ [- (ys !! n)] ++ replicate (l-n-1) 0 ++ replicate ((n-1)*4) 0
          let mid = map ((xs !! n)^) [0..3]
          return $ prefix ++ mid
        part3 =
          let l = length xs
              prefix = replicate l 0
              mid = replicate (4*(l-3)) 0
              mid1 = 0 : map (\x -> (toRational x+1)*(xs !! 0)^x) [0..2]
              mid2 = 0 : map (\x -> -(toRational x+1)*(xs !! (l-1))^x) [0..2]
          in return $ prefix ++ mid1 ++ mid ++ mid2
        part4 = 
          let l = length xs
              prefix = replicate l 0
              mid = replicate (4*(l-3)) 0
              mid1 = [0,0] ++ map (\x -> (toRational x+1)*(toRational x+2)*(xs !! 0)^x) [0,1]
              mid2 = [0,0] ++ map (\x -> -(toRational x+1)*(toRational x+2)*(xs !! (l-1))^x) [0,1]
          in return $ prefix ++ mid1 ++ mid ++ mid2
        
        part5 = zipWith (\y n -> replicate n 0 ++ [1] ++ replicate (length ys - n - 1) 0 ++ replicate ((length xs - 1) * 4) 0) ys [0..] 
          
       
