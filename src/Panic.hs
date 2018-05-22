{-# Language Trustworthy #-}
{-# Language ImplicitParams #-}
{-# Language TemplateHaskell #-}
module Panic (HasCallStack, panic, PanicComponent(..), useGitRevision) where

import Development.GitRev
import Language.Haskell.TH
import Data.Typeable

import Control.Exception(Exception, throw)
import Data.Maybe(fromMaybe,listToMaybe)
import GHC.Stack

panic :: (PanicComponent a, HasCallStack) => a -> String -> [String] -> b
panic comp loc msg =
  throw Panic { panicComponent = comp
              , panicLoc       = loc
              , panicMsg       = msg
              , panicStack     = freezeCallStack ?callStack
              }

data Panic a = Panic { panicComponent :: a
                     , panicLoc       :: String
                     , panicMsg       :: [String]
                     , panicStack     :: CallStack
                     }

class Typeable a => PanicComponent a where
  panicComponentName     :: a -> String
  -- | ^ Name of the panicing component.

  panicComponentIssues   :: a -> String
  -- | ^ Issue tracker for the panicking component.

  panicComponentRevision :: a -> (String,String)
  -- | ^ Information about the component's revision.
  -- (commit hash, branch info)

-- | An expression of type @a -> (String,String)@.
-- Uses template Haskell to query Git for the current state of the repo.
-- Note that the state reported depends on when the module containing
-- the splice was compiled.
useGitRevision :: Q Exp
useGitRevision = [| \_ -> ($gitHash, $gitBranch ++ $dirty) |]
  where dirty = [| if $gitDirty then " (uncommited files present)" else "" |]


instance (Typeable a, PanicComponent a) => Show (Panic a) where
  show p = unlines $
    [ "You have encountered a bug in " ++
        panicComponentName comp ++ "'s implementation."
    , "*** Please create an issue at " ++
        panicComponentIssues comp
    , ""
    , "%< --------------------------------------------------- "
    ] ++ rev ++
    [ locLab ++ panicLoc p
    , msgLab ++ fromMaybe "" (listToMaybe msgLines)
    ]
    ++ map (tabs ++) (drop 1 msgLines)
    ++ [ prettyCallStack (panicStack p) ] ++
    [ "%< --------------------------------------------------- "
    ]
    where comp      = panicComponent p
          msgLab    = "  Message:   "
          locLab    = "  Location:  "
          revLab    = "  Revision:  "
          branchLab = "  Branch:    "
          msgLines  = panicMsg p
          tabs      = map (const ' ') msgLab

          (commitHash,commitBranch) = panicComponentRevision comp

          rev | null commitHash = []
              | otherwise       = [ revLab ++ commitHash
                                  , branchLab ++ commitBranch
                                  ]


instance PanicComponent a => Exception (Panic a)

