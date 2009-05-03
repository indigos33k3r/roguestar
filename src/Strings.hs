-- | This section merely contains a mapping from strings used in the protocol to human-readable strings.
-- For example, "str" becomes "strength."

module Strings
    (hrstring)
    where

hrstring :: String -> String
hrstring "str" =  "Strength     "
hrstring "spd" =  "Speed        "
hrstring "con" =  "Constitution "
hrstring "int" =  "Intellect    "
hrstring "per" =  "Perception   "
hrstring "cha" =  "Charisma     "
hrstring "mind" = "Mindfulness  "
hrstring "forceadept" = "force adept"
hrstring x = map (\c -> if c == '_' then ' ' else c) x
