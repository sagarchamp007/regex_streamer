{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Parser(
             parse,
             AST(..)
             ) where

import qualified Data.ByteString.Char8 as BC
import           Data.Char
import           Data.List
import           Data.Strings
import           Text.RawString.QQ
import           Text.Read
import           Text.Regex.PCRE.Heavy
import           Utils

data AST = Class {negation:: Bool , value:: [AST]} | Repitition {minn:: Int, maxx:: Int, r_value::AST}  |  Character Char| Range Char Char| Dot | Lazy AST | Possesive AST deriving (Show, Eq)

infinity = 100

char_class = ["alnum", "alpha", "ascii", "blank", "cntrl", "digit", "graph", "lower", "print", "punct", "space", "upper", "word", "xdigit"]

end_class = [re|(?<!\[:\^alnum:|\[:\^alpha:|\[:\^ascii:|\[:\^blank:|\[:\^cntrl:|\[:\^digit:|\[:\^graph:|\[:\^lower:|\[:\^print:|\[:\^punct:|\[:\^space:|\[:\^upper:|\[:\^word:|\[:\^xdigit:)(?<!\[:alnum:|\[:alpha:|\[:ascii:|\[:blank:|\[:cntrl:|\[:digit:|\[:graph:|\[:lower:|\[:print:|\[:punct:|\[:space:|\[:upper:|\[:word:|\[:xdigit:)(?<=\\\\|[^\\])]|]

digit = (ord '0' , ord '9')
upcase = (ord 'A' , ord 'Z')
locase = (ord 'a' , ord 'z')
space =  (ord ' ', ord ' ')
tab =   (ord '\t', ord '\t')
whitespaces =  map  (\x -> (ord x, ord x)) ['\n', '\t', '\v', '\r', '\f', ' ']
underscore =  (ord '_', ord '_')
l_hex_digits = (ord 'a' , ord 'f')
u_hex_digits = (ord 'A' , ord 'F')

char_class_map = zip (map (\x -> x ++ ":]") char_class) [[digit, upcase, locase], [upcase, locase], [(0,127)], [space, tab], [(0, 31)], [digit], [(33, 126)], [locase], [(32,126)], [(33, 47), (58, 64), (91, 96), (123, 126)], whitespaces, [upcase], [underscore, digit, upcase, locase], [digit, l_hex_digits, u_hex_digits]]


escape_chars_map = zip ['d', 'h', 's', 'v', 'w'] [[digit], horizontal_spaces, whitespaces, vertical_spaces, [underscore, digit, upcase, locase]]
           where
             horizontal_spaces = map (\x -> (ord x, ord x)) ['\x0009', '\x0020', '\x00A0', '\x1680', '\x180E', '\x2000', '\x2001', '\x2002', '\x2003', '\x2004', '\x2005', '\x2006', '\x2007', '\x2008', '\x2009', '\x200A', '\x202F', '\x205F', '\x3000']
             vertical_spaces =   map (\x -> (ord x, ord x)) ['\x000A', '\x000B', '\x000C', '\x000D', '\x0085', '\x2028', '\x2029']


parse regex_str  = do_parse regex_str []

do_parse "" ast_ls = reverse(ast_ls)

do_parse ('.': rest) ast_ls = do_parse rest (Dot :ast_ls)

do_parse ('{': rest) ast_ls = (\(new_rest, new_ast)-> do_parse new_rest new_ast) $ may_be_repetition '{' rest ast_ls
do_parse ('*': rest) ast_ls = (\(new_rest, new_ast)-> do_parse new_rest new_ast) $ may_be_repetition '*' rest ast_ls
do_parse ('?': rest) ast_ls = (\(new_rest, new_ast)-> do_parse new_rest new_ast) $ may_be_repetition '?' rest ast_ls
do_parse ('+': rest) ast_ls = (\(new_rest, new_ast)-> do_parse new_rest new_ast) $ may_be_repetition '+' rest ast_ls


do_parse ('[': rest) ast = do_parse string_after_class (Class {negation = negation_flag , value = class_ast_ls} : ast)
   where
     class_ast_ls = do_parse_class class_section []
     (negation_flag, new_rest) =
          case rest of
            ('^': rem) -> (True, rem)
            otherwise  -> (False, rest)
     (class_section, string_after_class) = (slice new_rest).fst.head $ (scanRanges end_class new_rest)
       where
          slice = (\str (from, to)->  (take from str, drop to str))


do_parse ('\\' : char: rest) ast_ls = do_parse rest  ((escape_value char False) ++  ast_ls)

do_parse (e:ls) ast_ls = do_parse  ls  ((Character e): ast_ls)





do_parse_class "" ast_ls = reverse(ast_ls)
do_parse_class ('\\' : char: rest) ast_ls = do_parse_class rest  ((escape_value char True) ++  ast_ls)




do_parse_class ("-") ast_ls = do_parse_class "" ((Character '-' ): ast_ls)
do_parse_class ('-' : rest)  ast_ls = case ast_ls of
                                     [] -> do_parse_class rest  [(Character '-' )]
                                     ((Character first ) :old_ast) -> (reverse(old_ast))  ++ [Range first last] ++ rem_ast
                                                                       where
                                                                         (Character last): rem_ast  = do_parse_class rest []


do_parse_class ('[': ':' : rest) ast_ls = do_parse_class after_char_class (char_class_ast ++ ast_ls)
  where
    (negation_flag, new_rest, start) =
      case rest of
        ('^': rem) -> (True, rem, "[:^")
        otherwise  ->  (False, rest, "[:")
    defaults = map (\x -> (ord x, ord x)) start
    range_map = head $ (filter (\(x,y) ->  (isPrefixOf x new_rest)) char_class_map)  ++  [("", defaults)]
    range_ls =  snd range_map
    Just after_char_class = stripPrefix (fst range_map) new_rest
    char_class_ast =  to_range_ast negation_flag range_ls


do_parse_class (char:rest) ast_ls = do_parse_class  rest  ((Character char): ast_ls)





to_range_ast negation [] = []
to_range_ast negation range_ls = range_ast
   where
      range_ast_map ls = map (\(s, e) -> Range (chr s) (chr e)) ls
      range_ast =  range_ast_map . (Utils.negate_range negation) . Utils.non_overlapping . Utils.sorted_range $ range_ls



escape_value char class_flag = case (filter (\(x,y) -> x == (toLower char)) escape_chars_map) of
                                 [] ->  [Character char]
                                 value_ls ->  if class_flag then ast_ls else [Class {negation = False , value = ast_ls}]
                                   where
                                     ast_ls = map (\(s, e) -> Range (chr s) (chr e)) $  snd.head $ value_ls




may_be_repetition char rest [] = (rest, Character char: [])
may_be_repetition char rest (head:rem_ast) =  case (head, char) of
                                                (Repitition x y z, '?') -> (rest, Lazy head : rem_ast)
                                                (Repitition x y z, '+')  -> (rest, Possesive head : rem_ast)
                                                (head , some_char) | some_char `elem` ['*', '?', '+'] ->  case some_char of
                                                                                                                '*' -> (rest, (Repitition {minn= 0, maxx= infinity, r_value = head}: rem_ast))
                                                                                                                '?'-> (rest, (Repitition {minn= 0, maxx= 1, r_value = head}: rem_ast))
                                                                                                                '+' -> (rest, (Repitition {minn= 1, maxx= infinity, r_value = head}: rem_ast))

                                                otherwise ->  case strip_int $ rest of
                                                                 (Just min, '}': new_rest) -> (new_rest, (Repitition {minn= min, maxx= min, r_value = head}: rem_ast))
                                                                 (Just min, ',': '}':  new_rest) -> (new_rest, (Repitition {minn= min, maxx= infinity, r_value = head}: rem_ast))
                                                                 (Just min, ',':  new_rest) -> case strip_int $ new_rest of
                                                                                                   (Just max, '}': rem_str) -> (rem_str, (Repitition {minn= min, maxx= max, r_value = head}: rem_ast))
                                                                                                   (_, _) ->  (rest, Character char: head: rem_ast)
                                                                 (_, _) ->  (rest, Character char: head: rem_ast)


strip_int str = (readMaybe digit_str :: Maybe Int, rem_str)
                   where
                     is_digit char = (filter (\(x, y) -> y== char) $ zip [0..9] "0123456789") /= []
                     digit_str = takeWhile (is_digit) str
                     rem_str = dropWhile (is_digit) str
