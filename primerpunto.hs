data Persona = CPersona String Float Int [(String,Int)] deriving(Show)

frodo = (CPersona "Frodo" 100.0 30 [("amuleto",3),("manos magicas", 100)])
sam = (CPersona "Sam" 100.0 42 [("inteligencia",55),("paciencia",50)])

suerteSinAmuleto (CPersona _ _ s _) = s

valorDeElemento elemento (CPersona _ _ _ lista) =  snd (head (filter (amuletoSi elemento) lista))
amuletoSi elemento elementos = fst (elementos) == elemento

tieneElemento elemento (CPersona _ _ _ lista) = any (==elemento) (concatMap (\ x -> [fst x]) lista)
 
suerteTotal persona 
	|tieneElemento "amuleto" persona == True = (suerteSinAmuleto persona) * (valorDeElemento "amuleto" persona) 
	| otherwise = suerteSinAmuleto persona

