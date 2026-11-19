module Finalizer (finalize) where
import Types
import Data.List (intercalate)
import Helpers (addBackslashes)

identifierOf :: CommandBlock -> String
identifierOf (ImpulseRedstone _)        = "command_block"
identifierOf (ImpulseAlways _)        = "command_block"
identifierOf (RepeatingAlways _)        = "repeating_command_block"
identifierOf (ChainAlwaysConditional _) = "chain_command_block"

conditionalState :: CommandBlock -> String
conditionalState (ImpulseRedstone _)        = "false"
conditionalState (ImpulseAlways _)          = "false"
conditionalState (RepeatingAlways _)        = "false"
conditionalState (ChainAlwaysConditional _) = "true"

commandOf :: CommandBlock -> Command
commandOf (ImpulseRedstone c)           = c
commandOf (ImpulseAlways c)             = c
commandOf (RepeatingAlways c)           = c
commandOf (ChainAlwaysConditional c)    = c

autoTag :: CommandBlock -> String
autoTag (ImpulseRedstone _)         = "0b"
autoTag (ImpulseAlways _)           = "1b"
autoTag (RepeatingAlways _)         = "1b"
autoTag (ChainAlwaysConditional _)  = "1b"

commandBlockSetblock :: (Int, Int, Int) -> CommandBlock -> Command
commandBlockSetblock (xOffset, y, zOffset) b = "setblock ~" ++ show xOffset ++ " " ++ show y ++ " ~" ++ show zOffset ++ " " ++ identifierOf b ++
    "[conditional=" ++ conditionalState b ++ ",facing=south]{Command:\"" ++ addBackslashes (commandOf b) ++ "\",auto:" ++ autoTag b ++ "}"

-- xOffset,y,zOffset -> chain -> output generator commands
finalizeChain :: (Int, Int, Int) -> CommandBlockChain -> [Command]
finalizeChain (_, _, _) [] = []
finalizeChain (xOffset, y, zOffset) (cb:cbs) = case cb of
    (ImpulseRedstone _) -> ("setblock ~" ++ show xOffset ++ " " ++ show y ++ " ~" ++ show (zOffset - 1) ++ " polished_blackstone_button") : commandBlockSetblock (xOffset, y, zOffset) cb : finalizeChain (xOffset, y, zOffset + 1) cbs
    _                   -> commandBlockSetblock (xOffset, y, zOffset) cb : finalizeChain (xOffset, y, zOffset + 1) cbs

-- xOffset,y,zOffset -> chains -> output generator commands
finalizeInternal :: (Int, Int, Int) -> [CommandBlockChain] -> [Command]
finalizeInternal (_, _, _) [] = []
finalizeInternal (xOffset, y, zOffset) (c:cs) = finalizeChain (xOffset, y, zOffset) c ++ finalizeInternal (xOffset + 2, y, zOffset) cs

commandToMinecart :: Command -> String
commandToMinecart c = "{id:\"command_block_minecart\",Command:\"" ++ addBackslashes c ++ "\"}"

toOneCommand :: [Command] -> Command
toOneCommand commands = let minecarts = intercalate "," (map commandToMinecart commands) in
    "summon falling_block ~ ~1 ~ {BlockState:\"redstone_block\",Passengers:[{id:\"falling_block\",BlockState:\"activator_rail\"},{id:\"command_block_minecart\",Command:\"scoreboard objectives add mcmd dummy\"}," ++
    minecarts ++
    ",{id:\"command_block_minecart\",Command:\"setblock ~ ~1 ~ command_block{Command:\\\"fill ~ ~ ~ ~ ~-3 ~ air\\\",auto:1b}\"},{id:\"command_block_minecart\",Command:\"kill @e[type=command_block_minecart,distance=..1]\"}]}"

-- y -> chains -> output one command
finalize :: Int -> [CommandBlockChain] -> Command
finalize y chains = toOneCommand $ finalizeInternal (2, y, 0) chains