-- Student: S.D. Bosman 
-- Nummer: 851887378 

import System.Random
import Data.List
import Data.Maybe
import Data.Char

type W = Int
type Worp = [Steen]
type GepakteStenen = [Steen]
type Predikaat = (Int -> Bool)
type Score = Int
type Tactiek = (Worp -> GepakteStenen -> IO Steen 
               ,GepakteStenen -> IO Bool)
type Tegel = (Int, W)
type Tegels = [(Int, W)]
type Code = [Op]
type Spel = [Speler]
type Speler = Predikaat -> IO Score
type RijTegels = [Tegel]
type StapelsTegels = [Tegels]
type SpelerId = Int

data Steen = Nummer Int | W
             deriving (Ord,Eq)

data Op = PUSH Tegel | POP
          deriving Show

instance Show Steen where
  show (Nummer n) = show n
  show W = "W"

instance Random Steen where
randomRIO (Nummer n, W) = do i <- System.Random.randomRIO (n, steenWaarde W)
                             return (steenSymbolen !! i)
                            

wormWaarde :: W
wormWaarde = 5


steenWaarde :: Steen -> Int
steenWaarde (Nummer n) = n
steenWaarde W = wormWaarde

steenSymbolen :: [Steen]
steenSymbolen = [Nummer 1, Nummer 2, Nummer 3, Nummer 4, Nummer 5, W]


rijTegels :: RijTegels
rijTegels = zip [21..24] (repeat 1) ++
         zip [25..28] (repeat 2) ++
         zip [29..32] (repeat 3) ++
         zip [33..36] (repeat 4) 



stapelTegels :: Code -> Tegels -> Tegels
stapelTegels []           ts = ts
stapelTegels (PUSH t : c) ts = stapelTegels c (t : ts)
stapelTegels (POP : c)    ts = stapelTegels c (tail ts)


waardeStapelTegels :: Tegels -> Int
waardeStapelTegels ts = sum [w | (_,w) <- ts]


pakUitRijTegels :: Score -> Tegels -> (Tegels, Tegel)
pakUitRijTegels s  ts  = (delete pakTegel ts,pakTegel)
                               where 
                                  bestaatTegel = [(t,w) | (t,w) <- ts, t == s]
                                  pakTegel = if bestaatTegel /= [] then
                                                head bestaatTegel
                                             else (0,0)


pakUitStapelsTegels :: Score -> StapelsTegels -> (StapelsTegels, Tegel) 
pakUitStapelsTegels sc sts = do let hts  = [head st | st <- sts]
                                    t    = filter (\(t,w) -> sc == t) hts
                                    in if t /= [] then
                                      do let sid = fromJust $ elemIndex (head t) hts
                                             stn = stapelTegels [POP] (sts !! sid)
                                             stsn = take sid sts ++ stn : drop (sid+1) sts
                                             in (stsn,head t)
                                    else
                                      (sts,(0,0))


char2Steen :: Char -> Steen
char2Steen c | c == 'W' = W
             | c == 'w' = W
             | c == '1' = Nummer 1
             | c == '2' = Nummer 2
             | c == '3' = Nummer 3
             | c == '4' = Nummer 4
             | c == '5' = Nummer 5
             | otherwise = Nummer 0


worp :: Int -> IO Worp
worp n = sequence [Main.randomRIO (Nummer 1, W) | _ <- [1..n]]


beurt :: Predikaat -> Tactiek -> IO Score
beurt p = beurt' p 8 [] 


beurt' :: Predikaat -> Int -> GepakteStenen -> Tactiek -> IO Score
beurt' p n g t = do w <- worp n
                    if fst (keuzeMogelijk w g) then
                       do (s,d) <- tactiek w g p t
                          let gs = filter (s==) w
                              sc = waardeWorpen $ g ++ gs
                              l = length gs
                              in if d then
                                beurt' p (n-l) (g ++ gs) t
                              else
                                return sc
                    else
                       return 0


speel :: Spel -> IO ()
speel sp = speel' sp 0 (>20) rijTegels (replicate (length sp) [(0,0)])

speel' :: Spel -> SpelerId -> Predikaat -> RijTegels -> StapelsTegels -> IO ()
speel' _ _ _ [] sts     = do let wt = [waardeStapelTegels st | st <- sts]
                                 hsc = maximum wt
                                 sp = (\(Just n)->n) (findIndex (hsc==) wt) + 1
                                 in putStrLn $ "speler: " ++ show sp ++ " heeft gewonnen, score: " ++ show hsc
speel' sp sid p rts sts = do sc <- speelBeurt
                             if sc == 0 then
                               speel' sp (volgendeSpeler sid) p rts sts
                             else
                               do let (rtsn, t) = pakUitRijTegels sc rts
                                      in if fst t == 0 then 
                                        do let (stsn,tn) = pakUitStapelsTegels sc sts 
                                               in if fst tn == 0 then
                                                 speel' sp (volgendeSpeler sid) p rtsn stsn
                                               else 
                                                 do let st = stapelTegels [PUSH tn] (stsn !! sid)
                                                        stsnn = stapelsTegels st stsn
                                                        pn = (> (min (minRijTegels rtsn) (minStapelsTegels stsnn)))
                                                        in speel' sp (volgendeSpeler sid) pn rtsn stsnn
                                
                                      else 
                                        do let st = stapelTegels [PUSH t] (sts !! sid)
                                               stsn = stapelsTegels st sts
                                               pn = (> (min (minRijTegels rtsn) (minStapelsTegels stsn)))
                                               in speel' sp (volgendeSpeler sid) pn rtsn stsn
                          where speelBeurt = (sp !! sid) p
                                stapelsTegels st sts = take sid sts ++ st : drop (sid + 1) sts
                                volgendeSpeler sid =  (sid + 1) `mod` length sp
                                minRijTegels rts = (fst . minimum) rts
                                minStapelsTegels sts = let p = (fst . minimum) [head st | st <- sts]
                                                           in if p > 20 then 
                                                             p 
                                                           else 
                                                             20


keuzeMogelijk :: Worp -> GepakteStenen -> (Bool, String)
keuzeMogelijk  [] _                                             
  = (False, "worp: [] \ngeen stenen meer om te gooien.\nbeurt is voorbij.")

keuzeMogelijk w g | length [s | s <- w, s `elem` g] == length w 
  = (False, "worp: " ++ show w ++ "\ngeen stenen meer om te kiezen.\nbeurt is voorbij.")  

keuzeMogelijk w g                                            
  = (True, "worp: " ++ show w ++ "keuze is mogelijk.")


alGekozen :: Char -> GepakteStenen -> Bool
alGekozen c = elem (char2Steen c) 

kanStoppen :: (Int -> Bool) -> GepakteStenen -> Bool
kanStoppen f g =  f (waardeWorpen g) && elem W g


waardeWorpen :: GepakteStenen -> Score
waardeWorpen = foldr ((+) . steenWaarde) 0 


doorgaan :: Char -> Bool
doorgaan c | c == 'n' || c == 'N' = False
           | c == 'j' || c == 'J' = True
doorgaan c                        = False


tactiek :: Worp -> GepakteStenen -> Predikaat -> Tactiek -> IO(Steen,Bool)
tactiek w g p (pa, d) = do s <- pa w g
                           let gs = g ++ filter (s==) w
                           if kanStoppen p gs then
                             do dg <- d gs
                                return (s,dg)
                           else
                             return (s,True)


spelerTactiek :: Tactiek
spelerTactiek = (spelerTactiekPakken,spelerTactiekDoorgaan)


spelerTactiekPakken :: Worp -> GepakteStenen -> IO Steen
spelerTactiekPakken w g = do putStrLn ("worp: " ++ show w)
                             putStr "pakken? "
                             c <- getChar
                             putChar '\n'
                             if alGekozen c g then
                               do putStrLn "steen is al gekozen."
                                  spelerTactiekPakken w g
                             else 
                               if char2Steen c `elem`w then 
                                 return $ char2Steen c
                               else 
                                 do putStrLn "geen geldige waarde gekozen."
                                    spelerTactiekPakken w g 
  

spelerTactiekDoorgaan :: GepakteStenen -> IO Bool
spelerTactiekDoorgaan g = do putStrLn $ "doorgaan (score is " ++ show(waardeWorpen g) ++ ")?"  
                             c <- getChar
                             putChar '\n'
                             return $ doorgaan c 


computerTactiek :: Tactiek
computerTactiek = (computerTactiekPakken,computerTactiekDoorgaan)


computerTactiekPakken :: Worp -> GepakteStenen -> IO Steen
computerTactiekPakken w g = return $ maximum [s | s <- w, s `notElem` g]


computerTactiekDoorgaan :: GepakteStenen -> IO Bool
computerTactiekDoorgaan g = return False


spelerMens :: Speler
spelerMens p = beurt p spelerTactiek


spelerComputer :: Speler
spelerComputer p = beurt p computerTactiek


spel :: Int -> Spel
spel a = spelerMens : replicate a spelerComputer 

main:: IO ()
main = do putStrLn "Tegen hoeveel computertegenstanders wilt u spelen? Maximaaal 9."
          a <- getChar
          if isDigit a then 
            (speel . spel . digitToInt) a
          else main

