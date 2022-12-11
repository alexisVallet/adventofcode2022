{-
Contains imports of external dependencies common to all exercises.
Useful to avoid repeating a huge block of imports every time.
-}
module Imports
  ( module Data.Sequence,
    module Data.Bifunctor,
    module Control.DeepSeq,
    module Control.Exception,
    module Control.Lens,
    module Control.Monad,
    module Control.Monad.Identity,
    module Control.Monad.State.Strict,
    module Criterion.Measurement,
    module Data.Char,
    module Data.List,
    module Data.Either,
    module Data.Graph,
    module Data.List.Split,
    module Data.Map.Strict,
    module Data.IntMap.Strict,
    module Data.Set,
    module Data.Maybe,
    module Data.Text,
    module Data.Void,
    module Debug.Trace,
    module GHC.Generics,
    module Numeric,
    module System.Console.CmdArgs.Implicit,
    module System.IO,
    module Text.Megaparsec,
    module Text.Megaparsec.Char,
    module Text.Megaparsec.Char.Lexer,
    module Text.Pretty.Simple,
  )
where

import Control.DeepSeq
import Control.Exception hiding (try)
import Control.Lens hiding (noneOf, uncons, (<|), (|>))
import Control.Monad
import Control.Monad.Identity
import Data.Foldable
import Control.Monad.Reader
import Control.Monad.State.Strict
import Criterion.Measurement
import Data.Bifunctor
import Data.Char
import Data.Either
import Data.Generics.Labels ()
import Data.Graph
import Data.IntMap.Strict (IntMap)
import Data.List
import Data.List.Split hiding (chunk, endBy, oneOf, sepBy)
import Data.Map.Strict (Map)
import Data.Sequence (Seq, (<|), (|>))
import Data.Maybe
import Data.Set (Set)
import Data.Text (Text)
import Data.Void
import Debug.Trace
import GHC.Generics hiding (from, to)
import Numeric
import System.Console.CmdArgs.Implicit (Data, Typeable, cmdArgs)
import System.IO
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)
import Text.Pretty.Simple (pPrint)
