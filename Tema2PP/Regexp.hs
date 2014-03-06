{-
	Nume: Dimcica Tudor
	Grupa: 321CC
-}


module Regexp where

import Parser
import Data.Char (isLetter, isDigit)


data Reg = Nil 
	| Sym Char 
	| Alt Reg Reg 
	| Seq Reg Reg 
	| Rep Reg 
	| Opt Reg 
	deriving Show	

{- 
	Parser pentru extragerea unei litere sau a unei expresii dintr-o paranteza
-}

atom = (((spot isLetter) `transform` (\x -> Sym x)) `alt` parenSeq)
	where
		parenSeq = (token '(' >*> fParser >*> token ')') `transform` f
			where
				f (_, (e, _)) = e
		
{- 
	Parser pentru 	litera/paranteza 
					litera/paranteza + star 
					litera/paranteza + plus
					litera/paranteza + semnul intrebarii
					litera/paranteza + acolade de cuantificare
-}	
	
qnty0 = atom >*> token '{' >*> number >*> token '}'
qnty1 = atom >*> token '{' >*> number >*> token ',' >*> token '}'
qnty2 = atom >*> token '{' >*> number >*> token ',' >*> number >*> token '}'

	
operator = (makeStar `alt` makePlus `alt` makeOpt `alt` makeQ `alt` atom)
	where
		makeStar = (atom >*> token '*') 	`transform` 	(\(e, _) -> Rep e)			
		makePlus = (atom >*> token '+') 	`transform` 	(\(e, _) -> Seq e (Rep e)) 
		makeOpt = (atom >*> token '?') 	`transform` 	(\(e, _) -> Opt e) 
		makeQ = (qnty0 `transform` f0) `alt` (qnty1 `transform` f1) `alt` (qnty2 `transform` f2)
			where
				f0 (c, (_, (n, _))) = toQty0 c n
				f1 (c, (_, (n, (_, _)))) = toQty1 c n
				f2 (c, (_, (n1, (_, (n2, _))))) = toQty2 c n1 n2

{-
	Functiile de creere a formei intermediare pentru acolada	
-}

number = (spotWhile1 isDigit) `transform` (\x -> read x :: Integer)


{-
	Cazul {n} 	
-}
toQty0 :: Reg -> Integer -> Reg
toQty0 e 1 = e
toQty0 e n = Seq e (toQty0 e (n - 1))


{-
	Cazul {n,} 	
-}
toQty1 :: Reg -> Integer -> Reg
toQty1 e 0 = Rep e
toQty1 e 1 = Seq e (Rep e)
toQty1 e n = Seq e (toQty1 e (n - 1))


{-
	Cazul {n1,n2} 	
-}
toQty2 :: Reg -> Integer -> Integer -> Reg
toQty2 e 0 n2  = 	if(n2 == 0)
							then (toQty0 (Opt e) 1)
							else Seq (toQty2 e 0 (n2 - 1)) (Opt e)
toQty2 e n1 n2 = 	if (n1 == n2) 
							then (toQty0 e n1) 
							else Seq (toQty2 e n1 (n2 - 1)) (Opt e)


{-
	Parser pentru expresii ce pot fi multiple
-}

group = (operator `alt` multiple)
	where
		multiple = (operator >*> group) `transform` toSeq
			where
				toSeq (a, b) = Seq a b
		
{- 
	Parser pentru expresii ce pot contine si pipe
-}

fParser = (group `alt` makePipe)
	where
		makePipe = (group >*> token '|' >*> fParser) `transform` (\(a , (_, b)) -> Alt a b)


{-
	Functia pentru transformarea unei expresii regulate din string in tipul de data dorit (forma intermediara)
-}
				
makeRegEx :: String -> Reg
makeRegEx str = result fParser str


{-
	Functie ce creeaza un parser asociat tipului de data Reg
-}


parseSym :: Char -> Parser Char String
parseSym x = (token x) `transform` (\x -> [x])


makeParser :: Reg -> Parser Char String
makeParser (Sym c)		= parseSym c
makeParser (Opt o)      = parseOpt (makeParser o)
makeParser (Rep r)    	= parseRep (makeParser r)
makeParser (Seq a b)    = parseSeq (makeParser a) (makeParser b)
makeParser (Alt a1 a2) 	= parseAlt (makeParser a1) (makeParser a2)

{-
	Creere parser dintr-o secventa de parsere
-}
parseSeq :: Parser Char String -> Parser Char String -> Parser Char String
parseSeq s1 s2 = (s1 >*> s2) `transform` (\(s1,s2) -> s1 ++ s2)

{-
	Creere parser asociat unei repetari de elemente 
-}
parseRep :: Parser Char String -> Parser Char String
parseRep x = (maxList x) `transform` concat

{-
	Creere parser pentru gasirea unui element optional
-}
parseOpt :: Parser Char String -> Parser Char String
parseOpt x = (maxOptional x) `transform` (\x -> if (null x) then "" else head x)

{-
	Creere parser pentru o alternare de parsere
-}
parseAlt :: Parser Char String -> Parser Char String -> Parser Char String
parseAlt x y = x `alt` y


{-
	Functiile matches si getMatches ce returneaza potrivirile pentru expresia data
-}

matches :: String -> String -> [String]
matches expression input = getMatches (makeParser (makeRegEx expression)) input


getMatches :: Parser Char String -> String -> [String]
getMatches regExParser ""
	| (regExParser "" == []) = []
	| otherwise = [""]


{-
	Pentru orice fel de potrivire (nula, vida sau string), potrivirea se salveaza 
	corespunzator si se trece la verificarea potrivirilor pentru ce a ramas de
	parsat.
-}	

		
getMatches regExParser input = f (regExParser input)
	where
		f [] = getMatches regExParser (tail input)
		f [("", (x:xs))] = "" : getMatches regExParser xs
		f [(x, y)] = x : (getMatches regExParser y)

