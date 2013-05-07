module ANSIColors where

data ANSIColor = ANSIBlack | ANSIRed | ANSIGreen | ANSIYellow | ANSIBlue | ANSIMagenta | ANSICyan | ANSIWhite | ANSINone
    deriving (Ord, Eq)

instance Show ANSIColor where
    show ANSINone = "\27[0m" 
    show c = "\27[" ++ show cn ++ "m"
        where cn = 30 + colorNum c 

colorNum :: ANSIColor -> Int
colorNum c = length $ takeWhile (/= c) ansibow 

ansibow :: [ANSIColor]
ansibow = [ ANSIBlack
          , ANSIRed
          , ANSIGreen
          , ANSIYellow
          , ANSIBlue
          , ANSIMagenta
          , ANSICyan
          , ANSIWhite 
          ]

colorString :: ANSIColor -> String -> String
colorString c s = show c ++ s ++ show ANSINone 
