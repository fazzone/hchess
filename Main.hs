module Main where

import Control.Monad (when)

import Control.Concurrent.ParallelIO

import Data.List
import Data.Word (Word64)
import Data.Bits ((.&.))

import qualified Data.Vector.Unboxed as VU (fromList)

import System.Random.MWC

import Magic
import Bitboard

{-
The magic 
numbers that we are looking for will have a relatively (compared to the universe of Word64s,
that is) small number of bits set, so we can use this function to generate such numbers and hopefully
save time by not checking magics that are much more unlikely to work.
Credit: Tord Romstad; this is my translation of a similar function is his C source posted at
http://www.talkchess.com/forum/viewtopic.php?topic_view=threads&p=175834&t=19699
-}
randomIOFewBits :: GenIO -> IO Word64
randomIOFewBits gen = do
  a <- uniform gen
  b <- uniform gen
  c <- uniform gen
  return (a .&. b .&. c)

--Given a piece and a position, find a magic.
--IO because of random-number generation
findMagic :: GenIO -> Piece -> Position -> IO Word64
findMagic gen piece pos =  testMIter 0
  where
    bits = length $ getRelevantOccupancy piece pos  
    
    ocs = getPossibleROs piece pos
    ass = map (getAttackSetFor piece pos) ocs
    vrobbs = VU.fromList $ map toBitboard ocs
    vasbbs = VU.fromList $ map toBitboard ass  
    roMask = toBitboard $ getRelevantOccupancy piece pos
    
    testMIter :: Int -> IO Word64
    testMIter tries = do
      magic <- randomIOFewBits gen
      let worked = doesMagicWork vrobbs vasbbs magic roMask bits
      --return if it worked, otherwise try again
      if worked then return magic else testMIter (tries+1)

main :: IO ()
main = do
  gen <- withSystemRandom (asGenIO return)
  --given a piece, print one magic per square
  let getMagicsFor p = parallel $ map (findMagic gen p . unflattenSq) [0..63]
  --do that for both pieces
  putStr "bishopMagics = "
  print . VU.fromList =<< getMagicsFor Bishop
  putStr "rookMagics = "
  print . VU.fromList =<< getMagicsFor Rook
  
  stopGlobalPool
