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

callifPattern :: String
callifPattern = "^callif \\$([a-zA-Z0-9_-]+) (<|<=|=|>|>=) \\$([a-zA-Z0-9_-]+) ([a-zA-Z0-9_-]+)$" -- captures: [varSrc1, op, varSrc2, label]

callifImmPattern :: String
callifImmPattern = "^callif \\$([a-zA-Z0-9_-]+) (<|<=|=|>|>=) ([0-9]+) ([a-zA-Z0-9_-]+)$" -- captures: [varSrc1, op, imm, label]

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

getCallifSrc1 :: ModuleName -> Line -> VarName
getCallifSrc1 name l = case l =~ callifPattern :: (String, String, String, [String]) of
    (_, _, _, [varSrc1, _, _, _])   -> "#" ++ name ++ "$" ++ varSrc1
    _                               -> error $ "Cannot find var source 1 in \"" ++ l ++ "\""

getCallifOp :: Line -> VarName
getCallifOp l = case l =~ callifPattern :: (String, String, String, [String]) of
    (_, _, _, [_, op, _, _])    -> op
    _                           -> error $ "Cannot find op in \"" ++ l ++ "\""

getCallifSrc2 :: ModuleName -> Line -> VarName
getCallifSrc2 name l = case l =~ callifPattern :: (String, String, String, [String]) of
    (_, _, _, [_, _, varSrc2, _])   -> "#" ++ name ++ "$" ++ varSrc2
    _                               -> error $ "Cannot find var source 2 in \"" ++ l ++ "\""

getCallifTarget :: ModuleName -> Line -> MethodName
getCallifTarget name l = case l =~ callifPattern :: (String, String, String, [String]) of
    (_, _, _, [_, _, _, label]) -> labelToMethodName name label
    _                           -> error $ "Cannot find call target in \"" ++ l ++ "\""

getCallifImmSrc1 :: ModuleName -> Line -> VarName
getCallifImmSrc1 name l = case l =~ callifImmPattern :: (String, String, String, [String]) of
    (_, _, _, [varSrc1, _, _, _])   -> "#" ++ name ++ "$" ++ varSrc1
    _                               -> error $ "Cannot find var source 1 in \"" ++ l ++ "\""

getCallifImmOp :: Line -> VarName
getCallifImmOp l = case l =~ callifImmPattern :: (String, String, String, [String]) of
    (_, _, _, [_, op, _, _])    -> op
    _                           -> error $ "Cannot find op in \"" ++ l ++ "\""

getCallifImmImm :: Line -> VarName
getCallifImmImm l = case l =~ callifImmPattern :: (String, String, String, [String]) of
    (_, _, _, [_, _, imm, _])   -> imm
    _                           -> error $ "Cannot find immediate in \"" ++ l ++ "\""

getCallifImmTarget :: ModuleName -> Line -> MethodName
getCallifImmTarget name l = case l =~ callifImmPattern :: (String, String, String, [String]) of
    (_, _, _, [_, _, _, label])  -> labelToMethodName name label
    _                   -> error $ "Cannot find call target in \"" ++ l ++ "\""