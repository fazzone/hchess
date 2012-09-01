module Magic (applyMagic, doesMagicWork) where

import Debug.Trace

import Control.Monad.ST.Strict (ST, runST)

import Data.Word (Word64)
import Data.Bits (shiftR, popCount, (.&.))

import Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as M

import Bitboard

--First, the function to do the actual hashing given the magic number.
--assume that occ (the relevant occupancy mask) has already been ANDed with the total occupancy mask
-- (otherwise we would do it here)
applyMagic :: Int -> Word64 -> Bitboard -> Int
applyMagic sh magic occ = fromIntegral ((occ * magic) `shiftR` sh)

{-If two occupancies produce the same attack set, we will call them "equivalent".
  In a perfect world, our hash function would produce the exact same result for each set of equivalent occupancies.
  That is, for any two occupancies (call them o1 and o2); their equivalence would imply the equality of
  their hashes.  More concisely:
	(equivalent o1 o2)	would imply	(magicTx o1 == magicTx o2)
  However, this is not a perfect world.  We accept that equivalent occupancies may, in fact, produce 
  unequal hashes.  However, the one thing that we absolutely cannot do is map non-equivalent occupancies
  to the same index.  Concisely:
	(not $ equivalent o1 o2)        must imply        (magicTx o1 /= magicTx o2)
  Verifying that this is the case for every possible occupancy is the job of checkIndexUse.

  This code is a pretty direct translation of Tord Romstad's C code;
  we keep a mutable array and update its values with the attack set
  associated with each hash/index, and if we find ourselves
  with an index that is both already-occupied and occupied by a 
  different attack set, we reject the magic -}
checkIndexUse ::  VU.Vector Int -> VU.Vector Bitboard -> Bool
checkIndexUse pMTx vasbbs = runST $ do
  used <- M.new (VU.length pMTx)
  M.set used 0
  let
    go 0 = return True
    go n = do
      let hash = pMTx ! n
      let atk  = vasbbs ! n
      pv <- M.unsafeRead used hash
      if pv == 0
        then M.unsafeWrite used hash atk >> go (n - 1)
        else if (pv /= atk) 
                then return False
                else go (n - 1)
  go (VU.length pMTx - 1)
          
     
--parameters:
  --vrobbs	"vector of relevant-occupancy bitboards"	Stores all the possible occupancies.
  --vasbbs	"vector of attack-set bitboards"		Stores all the attack sets.  (vasbbs ! i) is the attack set associated with (vrobbs ! i)
  --magic							The 64-bit magic number we're checking
  --roMask	"relevant-occupancy mask"			Bit set if that square can influence the attack set. (hence relevant)
  --bits							The number of bits we're hashing.  Equal to popCount roMask
doesMagicWork :: VU.Vector Bitboard -> VU.Vector Bitboard -> Word64 -> Bitboard -> Int -> Bool     
doesMagicWork vrobbs vasbbs magic roMask bits = let
  magicTx = applyMagic (64 - bits) magic
  --A clever trick: Because of the properties of multiplication, we know that A > B  ->  A*K > B*K, so this one check
  --is sufficient to basically check bounds for every hash -- every possible relevant occ. will have the same or less
  --bits set than roMask, and so will be numerically less than or equal, so we need only check roMask's hash, because
  --all other hashes will be (again) less than or equal
  earlyDiscard = magicTx roMask >= VU.length vrobbs 
  in (not earlyDiscard) && checkIndexUse (VU.map magicTx vrobbs) vasbbs
  