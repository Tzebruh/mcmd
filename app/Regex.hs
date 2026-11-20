module Regex where
import Types
import Text.Regex.TDFA ((=~))

labelPattern :: String
labelPattern = "^\\.([a-zA-Z0-9_-]+):$" -- captures: [label]

callPattern :: String
callPattern = "^call ([a-zA-Z0-9_-]+)$" -- captures: [label]

varOpPattern :: String
varOpPattern = "^\\$([a-zA-Z0-9_-]+) (=|\\+=|-=|\\*=|\\/=|%=|>|<|><) \\$([a-zA-Z0-9_-]+)$" -- captures: [varDest, op, varSource]

varOpImmPattern :: String
varOpImmPattern = "^\\$([a-zA-Z0-9_-]+) (=|\\+=|-=|\\*=|\\/=|%=|>|<) ([0-9]+)$" -- captures: [varDest, op, imm]

assertPattern :: String
assertPattern = "^assert \\$([a-zA-Z0-9_-]+) (<|<=|=|>|>=) \\$([a-zA-Z0-9_-]+)$" -- captures: [varSrc1, op, varSrc2]

assertImmPattern :: String
assertImmPattern = "^assert \\$([a-zA-Z0-9_-]+) in ([0-9]+|\\.\\.[0-9]+|[0-9]+\\.\\.|[0-9]+\\.\\.[0-9]+)$" -- captures: [varSrc1, range]

-- module name -> label -> output compiled method name
labelToMethodName :: ModuleName -> Label -> MethodName
labelToMethodName name label = "#" ++ name ++ "." ++ label

getLabelName :: Line -> Label
getLabelName l = case l =~ labelPattern :: (String, String, String, [String]) of
    (_, _, _, [label])  -> label   -- before, match, after, capture groups
    _                   -> error $ "Cannot find label name in \"" ++ l ++ "\""

getCallTarget :: ModuleName -> Line -> MethodName
getCallTarget name l = case l =~ callPattern :: (String, String, String, [String]) of
    (_, _, _, [label])  -> labelToMethodName name label
    _                   -> error $ "Cannot find call target in \"" ++ l ++ "\""

getVarOpDest :: ModuleName -> Line -> VarName
getVarOpDest name l = case l =~ varOpPattern :: (String, String, String, [String]) of
    (_, _, _, [varDest, _, _])  -> "#" ++ name ++ "$" ++ varDest
    _                           -> error $ "Cannot find var dest in \"" ++ l ++ "\""

getVarOpOp :: Line -> String
getVarOpOp l = case l =~ varOpPattern :: (String, String, String, [String]) of
    (_, _, _, [_, op, _])   -> op
    _                       -> error $ "Cannot find op in \"" ++ l ++ "\""

getVarOpSource :: ModuleName -> Line -> VarName
getVarOpSource name l = case l =~ varOpPattern :: (String, String, String, [String]) of
    (_, _, _, [_, _, varSource])    -> "#" ++ name ++ "$" ++ varSource
    _                               -> error $ "Cannot find var source in \"" ++ l ++ "\""

getVarOpImmDest :: ModuleName -> Line -> VarName
getVarOpImmDest name l = case l =~ varOpImmPattern :: (String, String, String, [String]) of
    (_, _, _, [varDest, _, _])  -> "#" ++ name ++ "$" ++ varDest
    _                           -> error $ "Cannot find var dest in \"" ++ l ++ "\""

getVarOpImmOp :: Line -> VarName
getVarOpImmOp l = case l =~ varOpImmPattern :: (String, String, String, [String]) of
    (_, _, _, [_, op, _])   -> op
    _                       -> error $ "Cannot find op in \"" ++ l ++ "\""

getVarOpImmImm :: Line -> VarName
getVarOpImmImm l = case l =~ varOpImmPattern :: (String, String, String, [String]) of
    (_, _, _, [_, _, imm])  -> imm
    _                       -> error $ "Cannot find immediate in \"" ++ l ++ "\""

getAssertSrc1 :: ModuleName -> Line -> VarName
getAssertSrc1 name l = case l =~ assertPattern :: (String, String, String, [String]) of
    (_, _, _, [varSrc1, _, _])  -> "#" ++ name ++ "$" ++ varSrc1
    _                           -> error $ "Cannot find var source 1 in \"" ++ l ++ "\""

getAssertOp :: Line -> VarName
getAssertOp l = case l =~ assertPattern :: (String, String, String, [String]) of
    (_, _, _, [_, op, _])   -> op
    _                       -> error $ "Cannot find op in \"" ++ l ++ "\""

getAssertSrc2 :: ModuleName -> Line -> VarName
getAssertSrc2 name l = case l =~ assertPattern :: (String, String, String, [String]) of
    (_, _, _, [_, _, varSrc2])  -> "#" ++ name ++ "$" ++ varSrc2
    _                           -> error $ "Cannot find var source 2 in \"" ++ l ++ "\""

getAssertImmSrc1 :: ModuleName -> Line -> VarName
getAssertImmSrc1 name l = case l =~ assertImmPattern :: (String, String, String, [String]) of
    (_, _, _, [varSrc1, _]) -> "#" ++ name ++ "$" ++ varSrc1
    _                       -> error $ "Cannot find var source 1 in \"" ++ l ++ "\""

getAssertImmRange :: Line -> VarName
getAssertImmRange l = case l =~ assertImmPattern :: (String, String, String, [String]) of
    (_, _, _, [_, range])   -> range
    _                       -> error $ "Cannot find range in \"" ++ l ++ "\""