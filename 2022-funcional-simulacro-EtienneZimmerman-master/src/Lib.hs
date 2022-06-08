import Data.Char
import Text.Show.Functions

--------------
-- Punto 01 --
--------------

data Barbaro = Barbaro {
    nombre        :: String,
    fuerza        :: Int,
    habilidades   :: [Habilidades],
    objetos       :: [Objetos]
} deriving (Show)

otorgarHabilidad     funcion barbaro = barbaro { objetos = funcion (objetos barbaro) }


data Objetos = Objetos {
    nombreObjeto :: String
} deriving (Show)

armasDeBarbaro :: Barbaro
armasDeBarbaro Barbaro
objetos barbaro espada         = aumentarFuerza unBarbaro
objetos barbaro amuletoMistico = otorgarHabilidad unBarbaro
objetos barbaro varitaMagica   = hacerMagia unBarbaro
objetos barbaro unaArdilla     = ardilla unBarbaro
objetos barbaro unaCuerda      = fusionarObjetos unBarbaro

espada :: Objetos
pesoEspada   :: Int
espada =  Objetos "Espada" aumentarFuerza 
where{
    aumentarFuerza :: Objetos --> Barbaro -> Barbaro
    aumentarFuerza unObjeto  =  pesoEspada = pesoEspada*2 
}

    amuletoMistico :: Objetos 
    amuletoMistico = objetos "Amuleto Místico" otorgarHabilidad
    where{
        agregarHabilidad :: Objetos --> Barbaro --> Barbaro
        agregarHabilidad unObjeto unBarbaro = otorgarArtefacto (unObjeto :) unBarbaro
        }

    varitaMagica :: Objetos 
    varitaMagica = objetos "Varita Mágica" hacerMagia
    where{
        quitarHabilidades :: Barbaro --> Barbaro
        quitarHabilidades barbaro = otorgarHabilidad (drop (habilidades)) barbaro
        hacerMagia :: Barbaro --> Barbaro
        hacerMagia = otorgarHabilidad varitaMagica . quitarHabilidades 
    }


    unaArdilla :: Objetos 
    unaArdilla = objetos "Ardilla" ardilla

    unaCuerda :: Objetos --> Objetos --> Objetos
    unaCuerda objetos = objetos "Cuerda" fusionarObjetos

    fusionarObjetos :: [Objetos] --> [Habilidades] --> Objeto
    fusionarObjetos [objetos] [Habilidades] =  concat ([objetos] [Habilidades]) $ unObjeto 
    
    --------------
    -- Punto 02 --
    --------------

     megafono :: Objetos --> Barbaro
     megafono barbaro = objetos "Megáfono" concatenarHabilidades unBarbaro
     where{
     concatenarHabilidades :: [Habilidades] --> Barbaro
     concatenarHabilidad [Habilidades] barbaro = toUpper (concat ([Habilidades])) unBarbaro
     }

     megafonoBarbarico :: [Objetos] --> Barbaro --> Barbaro
     megafonoBarbarico unaArdilla unaCuerda megafono barbaro = [objetos].fusionarObjetos $ unObjeto


     --------------
     -- Punto 03 --
     --------------

     data Aventuras = Aventuras{
         nombreAventura :: [Aventuras]
         } deriving (Show)

     type Aventura = Barbaro --> Bool


         invasionDeDuendes :: Aventura --> Bool
         invasionDeDuendes aventuras barbaro = Habilidades ("Escribir Poesía Atroz")        

        sinPulgares :: Barbaro --> Bool
        sinPulgares barbaro = barbaro "Faffy" || "Astro"

         cremalleraDelTiempo :: Aventura --> Bool
         cremalleraDelTiempo aventuras barbaro = barbaro sinPulgares   
 
        ritualDeFechorias :: Aventura --> Bool
        ritualDeFechorias aventura barbaro 

        saqueo :: Barbaro --> Bool
        saqueo barbaro = fuerza>80 && habilidades "Robar"

        gritoDeGuerra barbaro =  poderDeGrito  > cantidadDeObjetos
        where{
            poderDeGrito :: Barbaro --> Int
            poderDeGrito barbaro = (length.[Habilidades])

            cantidadDeObjetos :: Barbaro --> Int
            cantidad de objetos [Objetos] = (*4) [Objetos]
        }

        sobrevivientes :: [Barbaro] --> Aventura --> Bool
        sobrevientes barbaro aventura = 

     --------------
     -- Punto 04 --
     --------------

     sinRepetidos :: [Barbaro] --> [Habilidades]
     sinRepetidos barbaro = filter(HabilidadesRepetidas) 

     habilidadesRepetidas :: [Habilidades] --> [Habilidades]
     habilidadesRepetidas [Habilidades] = 