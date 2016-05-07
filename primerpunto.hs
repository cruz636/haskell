data Persona = CPersona String Float Int [(String,Int)] deriving(Show)

frodo = (CPersona "Frodo" 100.0 30 [("amuleto",3),("manos magicas", 100)])
sam = (CPersona "Sam" 100.0 42 [("inteligencia",55),("paciencia",50)])

suerteSinAmuleto (CPersona _ _ s _) = s

valorDeAmuleto (CPersona _ _ _ lista) =  snd (head (filter amuletoSi lista))
amuletoSi elemento = fst (elemento) == "amuleto"

tieneAmuleto (CPersona _ _ _ lista) = any (=="amuleto") (concatMap (\ x -> [fst x]) lista)
 
suerteTotal persona 
	|tieneAmuleto persona == True = (suerteSinAmuleto persona) * (valorDeAmuleto persona) 
	| otherwise = suerteSinAmuleto persona
