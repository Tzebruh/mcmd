{-# LANGUAGE OverloadedStrings #-}
module Compiler (ModuleName, Label, Command, CommandMethod, CommandBlock (ImpulseRedstone, RepeatingAlways, ChainAlwaysConditional), CommandBlockChain, compile) where
import System.Directory (doesFileExist)
import Text.Regex.TDFA ((=~))
import Data.List (isPrefixOf)
--import Data.Text (unpack, pack, replace)

type ModuleName = String
type Label = String         -- refers to a source label like main
type Command = String       -- refers to a compiled command, not a source command
type CommandMethod = (Label, [Command])
type MethodName = String    -- refers to a compiled method name like #hello-world.main

data CommandBlock = ImpulseRedstone Command
                    | RepeatingAlways Command
                    | ChainAlwaysConditional Command
                    deriving (Eq, Ord, Show, Read)

type CommandBlockChain = [CommandBlock]

-- module name -> label -> output compiled method name
labelToMethodName :: ModuleName -> Label -> MethodName
labelToMethodName name label = "#" ++ name ++ "." ++ label

labelPattern :: String
labelPattern = "^\\.([a-zA-Z0-9_\\-]+):$"

callPattern :: String
callPattern = "^call ([a-zA-Z0-9_-]+)$"

getLabelName :: String -> Label
getLabelName l = case l =~ labelPattern :: (String, String, String, [String]) of
    (_, _, _, [label])  -> label   -- before, match, after, capture groups
    _                   -> error $ "Cannot find label name in \"" ++ l ++ "\""

getCallTarget :: String -> ModuleName -> MethodName
getCallTarget l name = case l =~ callPattern :: (String, String, String, [String]) of
    (_, _, _, [label])  -> labelToMethodName name label
    _                   -> error $ "Cannot find call target in \"" ++ l ++ "\""

-- methods -> module name -> line -> output line with substitutions
--substituteMethodNames :: [CommandMethod] -> ModuleName -> String -> String
--substituteMethodNames [] _ l        = l
--substituteMethodNames (m:ms) name l = substituteMethodNames ms name (unpack $ replace labelText methodName (pack l))
--    where labelText = pack $ fst m; methodName = pack $ labelToMethodName name (fst m)

-- lines -> current method -> methods -> module name -> output methods
innerPureCompile :: [String] -> Maybe CommandMethod -> [CommandMethod] -> ModuleName -> [CommandMethod]
innerPureCompile [] m methods _ = case m of
    Just mm -> methods ++ [mm]
    Nothing -> methods
innerPureCompile (l:ls) m methods name
    | l == "" || "--" `isPrefixOf` l = innerPureCompile ls m methods name -- Skip empty lines and comments
    | l =~ labelPattern :: Bool
        = case m of
            Just mm -> innerPureCompile ls (Just (getLabelName l, [])) (methods ++ [mm]) name
            Nothing -> innerPureCompile ls (Just (getLabelName l, [])) methods name
    | "/" `isPrefixOf` l
        = case m of
            Just mm -> innerPureCompile ls (Just (fst mm, (snd mm) ++ [drop 1 l])) methods name
            Nothing -> error "Attempted to emit command outside a method"
    | l =~ callPattern :: Bool
        = let emit = "scoreboard players set " ++ (getCallTarget l name) ++ " mcmd 1" in
            case m of
                Just mm -> innerPureCompile ls (Just (fst mm, (snd mm) ++ [emit])) methods name
                Nothing -> error "Attempted to emit command outside a method"
    | otherwise = error $ "Unknown command \"" ++ l ++ "\""

methodToChain :: ModuleName -> CommandMethod -> CommandBlockChain
methodToChain _ (label, []) = error $ "No commands in method \"" ++ label ++ "\""
methodToChain name (label, (c:cs)) = let methodName = labelToMethodName name label in
    case label of
        "main"  -> ImpulseRedstone c : map ChainAlwaysConditional cs
        _       -> [RepeatingAlways $ "execute if score " ++ methodName ++ " mcmd matches 1",
                    ChainAlwaysConditional $ "scoreboard players reset " ++ methodName ++ " mcmd"]
                    ++ map ChainAlwaysConditional (c:cs)

methodsToChains :: ModuleName -> [CommandMethod] -> [CommandBlockChain]
methodsToChains name = map $ methodToChain name

pureCompile :: [String] -> [CommandBlockChain]
pureCompile [] = error "Empty source file"
pureCompile (l:ls) = case l of
    '#' : name  -> if name =~ ("^[a-zA-Z0-9_-]+$" :: String) :: Bool then
                    methodsToChains name $ innerPureCompile ls Nothing [] name
                else
                    error $ "Invalid module name \"" ++ name ++ "\". Name can only be alphanumeric including underscores or dashes."
    _           -> error "Source file does not begin with a module declaration"

compile :: String -> IO [CommandBlockChain]
compile path = do
    fileExists <- doesFileExist path
    if fileExists then do
        contents <- readFile path
        pure $ pureCompile (lines contents)
    else
        error $ "File at " ++ path ++ " does not exist"