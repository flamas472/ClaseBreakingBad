--- Les damos los siguientes datas y funciones auxiliares para usar en la resolución

data Meta = UnaMeta {
 peso :: Float, -- en gramos
 pureza :: Float -- 0 a 1, donde 1 es la mejor calidad
} deriving (Show, Eq)

data Personaje = UnPersonaje {
 nombre :: String,
 nivelIntoxicacion :: Float,
 aguante :: Float, -- para ver qué tanto le impacta consumir meta
 dosisDeMeta :: [Meta], --(porque cualquiera puede tener algo de meta encima y porque es conveniente para la trama ;p)
 dinero :: Dinero -- el dinero se intercambia por bienes y servicios :P
} deriving (Show, Eq)

type Dinero = Float -- para no complicar tanto

walter = UnPersonaje {
 nombre = "Walter White",
 nivelIntoxicacion = 0,
 aguante = 5,
 dosisDeMeta = replicate 10 (UnaMeta 500 1),
 dinero = 5000
}

jesse = UnPersonaje {
 nombre = "Jesse Pinkman",
 nivelIntoxicacion = 10,
 aguante = 500,
 dosisDeMeta = [UnaMeta 10 0.8, UnaMeta 25 0.98],
 dinero = 1500
}

-- funciones auxiliares que les damos de entrada
conDinero :: Dinero -> Personaje -> Personaje
conDinero nuevoDinero personaje = personaje {dinero = nuevoDinero}
conNivelIntoxicacion :: Float -> Personaje -> Personaje
conNivelIntoxicacion nuevoNivelIntoxicacion personaje = personaje {nivelIntoxicacion = nuevoNivelIntoxicacion}
conAguante :: Float -> Personaje -> Personaje
conAguante nuevoAguante personaje = personaje {aguante = nuevoAguante}
conDosisDeMeta :: [Meta] -> Personaje -> Personaje
conDosisDeMeta dosisNuevas personaje = personaje {dosisDeMeta = dosisNuevas}

mayorSegun ponderacion x y
 | ponderacion x >= ponderacion y = x
 | otherwise = y

aplicarHasta transformacion criterio valor
 | criterio valor = valor
 | otherwise = aplicarHasta transformacion criterio (transformacion valor)

--1a
estaDrogado :: Personaje -> Bool
estaDrogado personaje = nivelIntoxicacion personaje > aguante personaje

--1b
aporteDeAguante :: Meta -> Personaje -> Float
aporteDeAguante meta personaje
 | estaDrogado personaje = pureza meta
 | otherwise = aporteDeIntoxicacion meta
aporteDeIntoxicacion :: Meta -> Float
aporteDeIntoxicacion meta = peso meta * pureza meta

--2a
consumirMeta :: Meta -> Personaje -> Personaje
consumirMeta meta personaje = (aumentarIntoxicacion meta) . (aumentarAguante meta) $ personaje

aumentarIntoxicacion :: Meta -> Personaje -> Personaje
aumentarIntoxicacion meta personaje = conNivelIntoxicacion (nivelIntoxicacion personaje + aporteDeIntoxicacion meta) personaje
aumentarAguante :: Meta -> Personaje -> Personaje
aumentarAguante meta personaje = conAguante (aguante personaje + aporteDeAguante meta personaje) personaje

--2b
mandarARehabilitacion :: Personaje -> Personaje
mandarARehabilitacion personaje = (aplicarHasta diaRehab ((<3) . nivelIntoxicacion)) . incautarMeta $ personaje

--aplicarNVeces
--aplicarNVeces n funcion valor = foldr1 (.) (replicate n funcion) $ valor
diaRehab :: Personaje -> Personaje
diaRehab = (disminuirIntoxicacion 1) . (disminuirAguante 25)
incautarMeta :: Personaje -> Personaje
incautarMeta personaje = conDosisDeMeta [] personaje
disminuirIntoxicacion :: Float -> Personaje -> Personaje
disminuirIntoxicacion cantidad personaje = conNivelIntoxicacion (nuevaintoxicacion) personaje
 where nuevaintoxicacion = max 0 (nivelIntoxicacionActual - cantidad)
       nivelIntoxicacionActual = nivelIntoxicacion personaje
disminuirAguante :: Float -> Personaje -> Personaje
disminuirAguante porcentaje personaje = conAguante (nuevoAguante) personaje
 where nuevoAguante = aguanteActual - porcentaje * (aguanteActual)
       aguanteActual = aguante personaje