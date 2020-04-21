module Main (main) where

import Control.Monad.State
import System.Environment (getArgs)

import qualified Futhark.Compiler as Compiler
import qualified Futhark.Internalise.Defunctorise as Defunctorise
import qualified Futhark.Internalise.Defunctionalise as Defunctionalise
import qualified Futhark.Internalise.Monomorphise as Monomorphise
import Futhark.Util.Pretty (pretty)
import Language.Futhark

main :: IO ()
main = do
  [file] <- getArgs
  (_, prog, src) <- Compiler.readProgramOrDie file
  let decs :: [ValBind]
      (decs, src') = flip runState src $
                     Defunctionalise.transformProg =<<
                     Monomorphise.transformProg =<<
                     Defunctorise.transformProg prog

  putStrLn "defunctionalised:"
  mapM_ (putStrLn . pretty) decs
