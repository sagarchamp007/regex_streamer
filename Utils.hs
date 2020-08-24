module Utils(
            sorted_range,
            non_overlapping,
            negate_range,
            rand,
            pick_one,
            all_ascii
             ) where

import           Data.Char
import           Data.List
import           System.IO.Unsafe
import           System.Random


sorted_range range_ls = sortBy (\ x y -> compare (fst x) (fst y)) $ range_ls


non_overlapping range_ls =  reverse . foldl (merge_ranges) (take 1 $ range_ls) $ range_ls
  where
       merge_ranges stack ele = if (stack_head_snd) > (fst ele) then  (stack_head_fst, max stack_head_snd (snd ele)):(drop 1 $ stack)  else  (ele:stack)
          where
            stack_head_fst = fst.head $ stack
            stack_head_snd = snd.head $ stack

negate_range negation range_ls = if negation then (negate_range_ls 0 [] range_ls) else range_ls
  where
    negate_range_ls low acc [] =  reverse $ if low < 126 then (low,126):acc else acc
    negate_range_ls low acc (e: ls) | low < (fst e) = negate_range_ls ((snd e)+1) ((low, (fst e)-1):acc)  ls
                                    | otherwise =  negate_range_ls ((snd e)+1)  acc ls


rand range = randomRIO range

pick_one ls = fmap (ls !!) $ randomRIO (0, length ls - 1)


all_ascii = map (chr) [32..126]
