{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
module QwickHw (doThingsAndStuff) where

import Data.Bifunctor (second)
import Data.Foldable (toList)
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq(..))
import qualified Data.Set as Set
import qualified Data.Text as T

-- | This is a conversion of a legacy js function. The terrible name of the original function
-- is maintained, as the actual function is so idiosyncratic, a proper descriptive name could be
-- no shorter than the long comment in the implementation going over exactly how we decide to 'sort' at each
-- step.
--
-- I suspect the intended function was to sort the input list of strings, ignoring the first word (as delimited by a space),
-- possibly with dedupe. Instead it does... this. Ticket #FAKE exists to look at downstream usage of this function and fix
-- both them and this function to do something that makes sense, or failing that figure out why we do actually want this strange
-- behavior and document it extensively.
doThingsAndStuff
  :: [T.Text]
  -> [T.Text]
doThingsAndStuff
    = fmap (\(h, t) -> h <> " " <> t)
    . reverse
    . toList
    . snd
    -- \'sort\', moving right to left (original order matters); see the comment explaining @insertAtKindOfSortedPosition@
    -- for a detailed explanation of the actual semantics
    . foldr (insertAtKindOfSortedPosition mempty) (mempty, mempty)
    -- split the string on spaces and separate the first word from the remainder, discarding anything without a space
    . mapMaybe splitOnFirstSpace
  where
    -- split the string on the first space, returning Nothing if there is no space. a string that is just
    -- a space is valid
    splitOnFirstSpace str =
      let
        broken = T.breakOn " " str
      in
        -- if our second value is the empty string, we did not find a space
        if snd broken == "" then Nothing
                            -- drop the actual space; they all have it so it's pointless for comparisons,
                            -- and it better matches the original code anyway so one less chance for a subtle difference
                            -- to sneak in
                            else Just . second (T.drop 1) $ broken

    -- Our workhorse function, determining where to \'sort\' each element from our input into our output list. Keep in mind
    -- that this list is ultimately reversed before being returned.
    -- For the following explanation, LESS THAN, GREATER THAN, and EQUAL TO all refer specifically to the portion
    -- after the first space (@inputSortString@/@workingSortString@), but *UNIQUEness* is determined by looking at the whole element,
    -- including the initial portion. UNIQUEness is also only relative to the elements we've seen so far, which we keep in the set
    -- @seen@. The other component of our accumulator/state is our working list, split into @workingListInit@ and @workingListRemaining@
    -- as we iterate through it. The element of the working list we are examining at any given step, @workingHead@, is the first
    -- element of @workingListRemaining@, with everything in @workingListInit@ coming before it. Working list, not referring
    -- to a specific variable, refers to the full working list given by @workingListInit <> workingListRemaining@.
    -- Full behavior is as follows:
    --   We examine @workingListRemaining@:
    --    - If our @workingListRemaining@ is EMPTY, and @inputElement@ is UNIQUE, add it to the end of our working list
    --      - If @inputElement@ is NOT UNIQUE, we SKIP IT.
    --    - If our @workingListRemaining@ has AT LEAST A HEAD ELEMENT, compare @inputElement@ to @workingHead@:
    --     - If the @inputElement@ is EQUAL TO @workingHead@ OR @inputElement@ is PREFIXED BY @workingHead@, we recurse, advancing
    --       our position in the working list by moving @workingHead@ from the beginning of @workingListRemaining@ to the end
    --       of @workingListInit@ for the recursive call. This is the only code path where we actually examine more than
    --       the first element of our working list.
    --     - If the @inputElement@ is LESS THAN @workingHead@, we add @inputElement@ to the working list TWO POSITIONS BEFORE
    --       @workingHead@, UNLESS @workingListInit@ is EMPTY in which case it goes to the BEGINNING of our working list (which
    --       would only be one position before). WE DO NOT DEDUPE IN THIS PATH.
    --     - If the @inputElement@ is GREATER THAN @workingHead@ and @inputElement@ is UNIQUE, we add it to the END
    --       of our working list.
    --     - If the @inputElement@ is GREATER THAN @workingHead@ and @inputElement@ is NOT UNIQUE, we SKIP IT.
    insertAtKindOfSortedPosition workingListInit inputElement@(_, inputSortString) (seen, workingListRemaining) = 
      case workingListRemaining of
        Empty ->
          if Set.member inputElement seen
            then (seen, workingListInit)
            else (seen <> Set.singleton inputElement, workingListInit :|> inputElement)
        (workingHead@(_, workingSortString) :<| t) ->
          case compare inputSortString workingSortString of
            _ | workingSortString `T.isPrefixOf` inputSortString
              -> insertAtKindOfSortedPosition (workingListInit :|> workingHead) inputElement (seen, t)
            EQ
              -> insertAtKindOfSortedPosition (workingListInit :|> workingHead) inputElement (seen, t)
            LT
              -> (Set.singleton inputElement <> seen,)
                -- insert not at this position, but one before it, if possible
                case workingListInit of
                  (wliInit :|> wliLast) -> wliInit <> (inputElement :<| wliLast :<| workingListRemaining)
                  Empty -> inputElement :<| workingListRemaining
            GT
              -> if Set.member inputElement seen
                   then (seen, workingListInit <> workingListRemaining)
                   else (Set.singleton inputElement <> seen, workingListInit <> workingListRemaining :|> inputElement)
