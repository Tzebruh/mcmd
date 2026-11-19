module Types where

type Line = String          -- refers to a line in the source file
type ModuleName = String
type Label = String         -- refers to a source label like main
type Command = String       -- refers to a compiled command, not a source command
type CommandMethod = (Label, [Command])
type MethodName = String    -- refers to a compiled method name like #hello-world.main
type VarName = String       -- refers to a compiled var name like #hello-world$var1

data CommandBlock = ImpulseRedstone Command
                    | ImpulseAlways Command
                    | RepeatingAlways Command
                    | ChainAlways Bool Command  -- Bool for is conditional
                    deriving (Eq, Ord, Show, Read)

type CommandBlockChain = [CommandBlock]