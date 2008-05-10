\begin{code}
module CommandLine(CommandLineOptions, keymap_name, parseCommandLine) where

import Keymaps
import DefaultKeymap

data CommandLineOptions = CommandLineOptions {
	keymap_name :: Maybe KeymapName
}

-- Extremely minimal implementation for now: switch to System.Console.GetOpt or just use a config. file?
parseCommandLine :: [String] -> CommandLineOptions
parseCommandLine [the_keymap_name] = CommandLineOptions $ Just the_keymap_name
parseCommandLine _ = CommandLineOptions Nothing
\end{code}