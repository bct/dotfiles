module Color where

data Color = RGB Int Int Int
           deriving Eq

colorString :: Color -> String
colorString (RGB r g b) = "#" ++ (cconv r) ++ (cconv g) ++ (cconv b)
                          where cconv = colFill . hexStr

instance Show Color where
  show c = colorString c

instance Num Color where
  (RGB r1 g1 b1) + (RGB r2 g2 b2) = (RGB (min 255 (r1+r2)) 
                                     (min 255 (g1+g2)) 
                                     (min 255 (b1+b2)))
  (RGB r1 g1 b1) - (RGB r2 g2 b2) = (RGB (max 0 (r1-r2)) 
                                     (max 0 (g1-g2)) 
                                     (max 0 (b1-b2)))
  (RGB r1 g1 b1) * (RGB r2 g2 b2) = (RGB (min 255 (r1*r2)) 
                                     (min 255 (g1*g2)) 
                                     (min 255 (b1*b2)))
  abs c = c
  signum c = 1
  fromInteger i =(RGB (fromInteger (min 255 (max 0 i)))
                  (fromInteger (min 255 (max 0 i)))
                  (fromInteger (min 255 (max 0 i))))


white :: Color
white = fromInteger 255

black :: Color
black = fromInteger 0

greyTone :: Int -> Color
greyTone = fromInteger . toInteger

grey  :: Color
grey  = greyTone 50

red :: Color
red = RGB 255 0 0
      
green :: Color
green = RGB 0 255 0

blue :: Color
blue = RGB 0 0 255

magenta = addMix red blue
yellow  = addMix red green
cyan    = addMix green blue

addMix :: Color -> Color -> Color
addMix c1 c2 = c1 + c2

subMix :: Color -> Color -> Color
subMix c1 c2 = white - c1 - c2

darker :: Color -> Color
darker c = c - (greyTone 15) 

lighter :: Color -> Color
lighter c = c + (greyTone 15) 


-- Convert an integer to a hexadecimal string
hexStr :: Int -> String
hexStr x = case x of
  0 -> ""
  otherwise -> hexStr (x `div` 16) ++ digit (x `mod` 16)
  where
    digit x
      | x < 10 = show x 
      | (x == 10) = "a"
      | (x == 11) = "b"
      | (x == 12) = "c"
      | (x == 13) = "d"
      | (x == 14) = "e"
      | (x == 15) = "f"
                   
-- add chars to the left of the string until it matches the given length
leftFill :: Int -> Char -> String -> String
leftFill n c s
  | length s >= n  = s
  | otherwise     = (take (n - length s) $ repeat c) ++ s
                    
colFill :: String -> String
colFill = leftFill 2 '0'