{-# LANGUAGE NamedFieldPuns #-}

module Challenges.Day16 (day16) where

import Control.Monad (foldM)
import Data.Array (Array, array, (!))
import Data.Bifunctor (second)
import Utils (chunks)

data Packet = Packet {version :: Int, content :: PacketContent}
data PacketContent = Operator {items :: [Packet], op :: Op} | Literal Int
data Op = Sum | Prod | Min | Max | Gt | Lt | Eq

hexmap :: Array Char [Bool]
hexmap = array ('0', 'F') $ map (second (map (== '1')))
    [ ('0', "0000"), ('1', "0001"), ('2', "0010"), ('3', "0011")
    , ('4', "0100"), ('5', "0101"), ('6', "0110"), ('7', "0111")
    , ('8', "1000"), ('9', "1001"), ('A', "1010"), ('B', "1011")
    , ('C', "1100"), ('D', "1101"), ('E', "1110"), ('F', "1111") ]

opmap :: Array Int Op
opmap = array (0, 7) [(0, Sum), (1, Prod), (2, Min), (3, Max), (5, Gt), (6, Lt), (7, Eq)]

parse :: String -> [Bool]
parse = concatMap (hexmap !) . filter (`elem` "0123456789ABCDEF")

parseInt :: Integral n => [Bool] -> n
parseInt = foldl (\a e -> a * 2 + if e then 1 else 0) 0

-- length type id 0: length is given in bits
parseOp :: Op -> [Bool] -> (PacketContent, Int)
parseOp op bits = (Operator{items = rec len payload, op}, len + 15)
  where
    len = parseInt $ take 15 bits
    payload = drop 15 bits
    rec left cbits
        | left == 0 = []
        | otherwise = pck : rec (left - count) (drop count cbits)
      where
        (pck, count) = parsePacket cbits

-- length type id 1: length is given in number of packets
parseOp' :: Op -> [Bool] -> (PacketContent, Int)
parseOp' op bits = (\(a, items, _) -> (Operator{items, op}, a)) $ (!! pcount) $ iterate rec (11, [], payload)
  where
    pcount = parseInt $ take 11 bits
    payload = drop 11 bits
    rec (count, packets, cbits) = (count + consumed, packets ++ [pck], drop consumed cbits)
      where
        (pck, consumed) = parsePacket  cbits

parseOperatorPacket :: Int -> [Bool] -> (PacketContent, Int)
parseOperatorPacket _ [] = error "Trying to parse empty operator packet"
parseOperatorPacket k (h : xs)
    | h = second (+ 1) $ parseOp' op xs
    | otherwise = second (+ 1) $ parseOp op xs
  where
    op = opmap ! k

parseLiteralPacket :: [Bool] -> (PacketContent, Int)
parseLiteralPacket bits = (Literal (parseInt bin), count)
  where
    (bin, count) = either id id $ foldM f ([], 0) $ chunks 5 bits
    f (l, c) (b : xs)
        | b = Right (l ++ xs, c + 5)
        | otherwise = Left (l ++ xs, c + 5)
    f (l, c) _ = Left (l, c)

parsePacket :: [Bool] -> (Packet, Int)
parsePacket bits = (Packet{version, content}, count + 6)
  where
    version = parseInt $ take 3 bits
    kind = parseInt $ take 3 $ drop 3 bits
    left = drop 6 bits
    (content, count) = if kind == 4 then parseLiteralPacket left else parseOperatorPacket kind left

sumVersion :: Packet -> Int
sumVersion (Packet{version, content = (Operator{items})}) = version + sum (map sumVersion items)
sumVersion (Packet{version}) = version

valueOf :: Packet -> Int
valueOf (Packet{content = (Literal v)}) = v
valueOf (Packet{content = (Operator{op = Sum, items})}) = sum $ map valueOf items
valueOf (Packet{content = (Operator{op = Prod, items})}) = product $ map valueOf items
valueOf (Packet{content = (Operator{op = Min, items})}) = minimum $ map valueOf items
valueOf (Packet{content = (Operator{op = Max, items})}) = maximum $ map valueOf items
valueOf (Packet{content = (Operator{op = Lt, items = [a,b]})}) = if valueOf a < valueOf b then 1 else 0
valueOf (Packet{content = (Operator{op = Gt, items = [a,b]})}) = if valueOf a > valueOf b then 1 else 0
valueOf (Packet{content = (Operator{op = Eq, items = [a,b]})}) = if valueOf a == valueOf b then 1 else 0
valueOf _ = error "Bad packet (Binary operator applied on 0, 1 or 3+ packets)"

day16 :: String -> (String, String)
day16 str = (show $ sumVersion inp, show $ valueOf inp) where inp = fst $ parsePacket $ parse str
