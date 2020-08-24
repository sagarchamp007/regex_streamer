{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}


module Evaluate(
              eval,
              generate
           ) where


import qualified Data.ByteString.Char8 as BC
import           Data.Char
import           Data.List
import           Text.RawString.QQ
import           Text.Regex.PCRE.Heavy
--import Control.Monad.Random.Class
import           Parser
import           Utils
import Control.Monad.IO.Class


--eval  = ""
--g = eval $ parse [r|abc|]

empty_monad = return ""

join x y = do
    x0 <- x
    y0 <- y
    return (x0 ++ y0)

join1 x y = do
   x0 <- x
   y0 <- y
   y1 <- y0
   return (x0 ++ y1)


eval ast = do_eval empty_monad ast

do_eval str []  =  str
do_eval str (Character a: rem_ast)  =  do_eval (fmap (\x-> x ++ [a])  str) rem_ast
          

do_eval str (Range start end: rem_ast)  = do_eval (join str random_char)  rem_ast
    where
      random_num =  Utils.rand(ord start, ord end)
      random_char = fmap (\x -> [chr x]) $ random_num

do_eval str ((Class negation class_ast):rem_ast) =  do_eval (join1 str rand_char)  rem_ast
   where
     eval_class class_elem = case class_elem of
                               Character a     ->  ((ord a), (ord a))
                               Range start end -> ((ord start), (ord end))
     class_ranges ast = map (eval_class) ast
     sanitize_ranges = (Utils.negate_range negation) . Utils.non_overlapping . Utils.sorted_range
     rand_char = fmap (\x -> fmap (\y -> [chr y]) x) $ Utils.pick_one $ map (Utils.rand) $ sanitize_ranges .class_ranges $ class_ast


do_eval str (Dot:rem_ast) = do_eval (join str one_of_ascii)  rem_ast
  where
    one_of_ascii = fmap (\x -> [x]) $ Utils.pick_one $ all_ascii


do_eval str ((Repitition min max rept_ast):rem_ast) = do_eval (join1 (str) rept_str) rem_ast
  where
    rept_value = map (do_eval empty_monad) $ (repeat  [rept_ast])
    rept_str =  Utils.pick_one $ map (mconcat) $ zipWith  ($) (map (take) [min .. max])  (repeat rept_value)



do_eval str ((Lazy ast):rem_ast) =  do_eval (join (str) lazy_str) rem_ast
  where
    lazy_str = (do_eval empty_monad [ast])


do_eval str ((Possesive ast):rem_ast) = do_eval (join str poss_str) rem_ast
    where
      poss_str = (do_eval empty_monad [ast])



do_generate 0 gen = return []
do_generate n gen = do
                    x  <- gen
                    xs <- do_generate (n-1) gen
                    return (x:xs)
                  
  
generate n input =  case validate input of
                    Right c_regex ->  do_generate n (eval $ parse input)
                    Left  c_regex -> error "Invalid Regex supplied@@@@"
  where validate str = compileM (BC.pack str) []

-- gen input = do
--   x <- (generate input)
--   return x

--g = take 5  $ generate [r|[0-9]{1,2}|]
