module Compiler (compile) where
import Types
import Regex
import System.Directory (doesFileExist)
import Text.Regex.TDFA ((=~))
import Data.List (isPrefixOf)

-- lines -> current method -> methods -> module name -> output methods
innerPureCompile :: [Line] -> Maybe CommandMethod -> [CommandMethod] -> ModuleName -> [CommandMethod]
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
        = let emit = "scoreboard players set " ++ getCallTarget name l ++ " mcmd 1" in
            case m of
                Just mm -> innerPureCompile ls (Just (fst mm, (snd mm) ++ [emit])) methods name
                Nothing -> error "Attempted to emit command outside a method"
    | l =~ varOpPattern :: Bool
        = let emit = "scoreboard players operation " ++ getVarOpDest name l ++ " mcmd " ++ getVarOpOp l ++ " " ++ getVarOpSource name l ++ " mcmd" in
            case m of
                Just mm -> innerPureCompile ls (Just (fst mm, (snd mm) ++ [emit])) methods name
                Nothing -> error "Attempted to emit command outside a method"
    | l =~ varOpImmPattern :: Bool
        = let
            emit1 = "scoreboard players set #" ++ name ++ "$$imm mcmd " ++ getVarOpImmImm l
            emit2 = "scoreboard players operation " ++ getVarOpImmDest name l ++ " mcmd " ++ getVarOpImmOp l ++ " #" ++ name ++ "$$imm" ++ " mcmd"
        in
            case m of
                Just mm -> innerPureCompile ls (Just (fst mm, (snd mm) ++ [emit1, emit2])) methods name
                Nothing -> error "Attempted to emit command outside a method"
    | l =~ assertPattern :: Bool
        = let emit = "execute if score " ++ getAssertSrc1 name l ++ " mcmd " ++ getAssertOp l ++ " " ++ getAssertSrc2 name l ++ " mcmd" in
            case m of
                Just mm -> innerPureCompile ls (Just (fst mm, (snd mm) ++ [emit])) methods name
                Nothing -> error "Attempted to emit command outside a method"
    | l =~ assertImmPattern :: Bool
        = let emit = "execute if score " ++ getAssertImmSrc1 name l ++ " mcmd matches " ++ getAssertImmRange l in
            case m of
                Just mm -> innerPureCompile ls (Just (fst mm, (snd mm) ++ [emit])) methods name
                Nothing -> error "Attempted to emit command outside a method"
    | otherwise = error $ "Unknown command \"" ++ l ++ "\""

--nextBlockShouldBeUncon :: Command -> Bool
--nextBlockShouldBeUncon = isPrefixOf "execute if"

-- start with unconditional -> remaining commands -> output remaining chain
--remainingChain :: Bool -> [Command] -> CommandBlockChain
--remainingChain _ [] = []
--remainingChain startUncon (c:cs)
--    | startUncon    = ChainAlways False c : remainingChain (nextBlockShouldBeUncon c) cs
--    | otherwise     = ChainAlways True c : remainingChain (nextBlockShouldBeUncon c) cs

methodToChain :: ModuleName -> CommandMethod -> CommandBlockChain
methodToChain _ (label, []) = error $ "No commands in method \"" ++ label ++ "\""
methodToChain name (label, (c:cs)) = let methodName = labelToMethodName name label in
    case label of
        "main"  -> ImpulseRedstone c : map (ChainAlways True) cs
        "setup" -> ImpulseAlways c : map (ChainAlways True) cs
        "loop"  -> RepeatingAlways c : map (ChainAlways True) cs
        _       -> RepeatingAlways ("execute if score " ++ methodName ++ " mcmd matches 1 run scoreboard players reset " ++ methodName ++ " mcmd") : map (ChainAlways True) (c:cs)

methodsToChains :: ModuleName -> [CommandMethod] -> [CommandBlockChain]
methodsToChains name = map $ methodToChain name

pureCompile :: [Line] -> [CommandBlockChain]
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