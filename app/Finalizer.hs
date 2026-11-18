module Finalizer (finalize) where
import Compiler (CommandBlockChain)

finalize :: [CommandBlockChain] -> String
finalize chains = show chains