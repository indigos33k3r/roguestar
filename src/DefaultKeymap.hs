module DefaultKeymap
    (default_keymap)
    where

import Keymaps

default_keymap :: [(String,String)]
default_keymap = fixKeymap [
 ("x","anachronid"),
 (":anachronid","anachronid"),
 ("a","androsynth"),
 (":androsynth","androsynth"),
 ("A","ascendant"),
 (":ascendant","ascendant"),
 ("c","caduceator"),
 (":caduceator","caduceator"),
 ("e","encephalon"),
 (":encephalon","encephalon"),
 ("g","goliath"),
 (":goliath","goliath"),
 ("h","hellion"),
 (":hellion","hellion"),
 ("k","kraken"),
 (":kraken","kraken"),
 ("m","myrmidon"),
 (":myrmidon","myrmidon"),
 ("p","perennial"),
 (":perennial","perennial"),
 ("r","reptilian"),
 (":reptilian","reptilian"),
 ("R","recreant"),
 (":recreant","recreant"),
 (".","reroll"),
 (":reroll","reroll"),
 ("b","barbarian"),
 (":barbarian","barbarian"),
 ("c","consular"),
 (":consular","consular"),
 ("e","engineer"),
 (":engineer","engineer"),
 ("a","forceadept"),
 (":force-adept","forceadept"),
 ("m","marine"),
 (":marine","marine"),
 ("n","ninja"),
 (":ninja","ninja"),
 ("p","pirate"),
 (":pirate","pirate"),
 ("s","scout"),
 (":scout","scout"),
 ("S","shepherd"),
 (":shepherd","shepherd"),
 ("t","thief"),
 (":thief","thief"),
 ("w","warrior"),
 (":warrior","warrior"),
 ("k","move-n"),
 (":move-n","move-n"),
 ("j","move-s"),
 (":move-s","move-s"),
 ("h","move-w"),
 (":move-w","move-w"),
 ("l","move-e"),
 (":move-e","move-e"),
 ("y","move-nw"),
 (":move-nw","move-nw"),
 ("u","move-ne"),
 (":move-ne","move-ne"),
 ("b","move-sw"),
 (":move-sw","move-sw"),
 ("n","move-se"),
 (":move-se","move-se"),
 ("K","turn-n"),
 (":turn-n","turn-n"),
 ("J","turn-s"),
 (":turn-s","turn-s"),
 ("H","turn-w"),
 (":turn-w","turn-w"),
 ("L","turn-e"),
 (":turn-e","turn-e"),
 ("Y","turn-nw"),
 (":turn-nw","turn-nw"),
 ("U","turn-ne"),
 (":turn-ne","turn-ne"),
 ("B","turn-sw"),
 (":turn-sw","turn-sw"),
 ("N","turn-se"),
 (":turn-se","turn-se"),
 ("#quit","quit"),
 (",","pickup"),
 (":pickup","pickup"),
 ("d","drop"),
 (":drop","drop"),
 ("w","wield"),
 (":wield","wield"),
 ("-","unwield"),
 (":unwield","unwield"),
 (">fk","fire-n"),
 (":fire-n","fire-n"),
 (">fj","fire-s"),
 (":fire-s","fire-s"),
 (">fh","fire-w"),
 (":fire-w","fire-w"),
 (">fl","fire-e"),
 (":fire-e","fire-e"),
 (">fy","fire-nw"),
 (":fire-new","fire-nw"),
 (">fu","fire-ne"),
 (":fire-ne","fire-ne"),
 (">fb","fire-sw"),
 (":fire-sw","fire-sw"),
 (">fn","fire-se"),
 (":fire-se","fire-sw"),
 (":continue","continue")]
