-- =====================================================================
--  CI-3661 — Tarea 1 (Haskell) — Septiembre–Diciembre 2026
--
--  Nombre: Samuel Sanabria
--  Carnet: 20-10591
--
--  Declaración de uso de herramientas de IA (obligatoria, ver enunciado):
--  Se utilizó Antigravity (asistente de IA) para consultas de diseño,
--  revisión de restricciones del enunciado y apoyo en la elaboración de pruebas.
-- =====================================================================
--  No cambies el nombre del módulo ni la lista de exportación, ni las
--  firmas de las funciones. Puedes agregar todas las funciones auxiliares
--  que quieras. El único import permitido es Data.Char.
-- =====================================================================
module Tarea1
  ( esPalindromo
  , mayorYMenor
  , leerHora
  , duracion
  , promedioSobreUmbral
  , movimientosCaballo
  , enDosSaltos
  , miMap
  , miFilter
  , miFoldl
  ) where

import Data.Char (isAlphaNum, isDigit, toLower, digitToInt)

-- =====================================================================
-- Parte A: recursión explícita.
-- Prohibido: map, filter, foldr, foldl, listas por comprensión, reverse,
-- maximum, minimum, all, any y cualquier otra función de orden superior.
-- =====================================================================

-- A1
esPalindromo :: String -> Bool
esPalindromo xs = limpio == invertir limpio
  where
    limpiar :: String -> String
    limpiar [] = []
    limpiar (c:cs)
      | isAlphaNum c = toLower c : limpiar cs
      | otherwise    = limpiar cs

    invertir :: String -> String
    invertir ys = revAux ys []

    revAux :: String -> String -> String
    revAux [] acc     = acc
    revAux (c:cs) acc = revAux cs (c : acc)

    limpio :: String
    limpio = limpiar xs

-- A2
-- Explica aquí, en dos o tres líneas, por qué tu solución recorre la
-- lista una sola vez:
-- La función auxiliar recursiva 'recorrer' procesa la lista elemento a elemento
-- actualizando simultáneamente en cada paso tanto el mayor como el menor vistos.
-- De esta forma se realiza una única pasada lineal sobre la estructura sin recomputar.
mayorYMenor :: Ord a => [a] -> Maybe (a, a)
mayorYMenor []     = Nothing
mayorYMenor (x:xs) = Just (recorrer xs x x)
  where
    recorrer :: Ord a => [a] -> a -> a -> (a, a)
    recorrer [] may men = (may, men)
    recorrer (y:ys) may men =
      let nuevoMay = if y > may then y else may
          nuevoMen = if y < men then y else men
      in recorrer ys nuevoMay nuevoMen

-- A3
leerHora :: String -> Either String (Int, Int)
leerHora [h1, h2, ':', m1, m2]
  | not (isDigit h1 && isDigit h2 && isDigit m1 && isDigit m2) = Left "formato inválido"
  | h > 23    = Left "hora fuera de rango"
  | m > 59    = Left "minutos fuera de rango"
  | otherwise = Right (h, m)
  where
    h = digitToInt h1 * 10 + digitToInt h2
    m = digitToInt m1 * 10 + digitToInt m2
leerHora _ = Left "formato inválido"

duracion :: String -> String -> Either String Int
duracion s1 s2 =
  case leerHora s1 of
    Left err1 -> Left err1
    Right (h1, m1) ->
      case leerHora s2 of
        Left err2 -> Left err2
        Right (h2, m2) ->
          let min1 = h1 * 60 + m1
              min2 = h2 * 60 + m2
              diff = min2 - min1
          in Right (if diff < 0 then diff + 24 * 60 else diff)

-- =====================================================================
-- Parte B: orden superior.
-- Prohibido: recursión explícita (ninguna función de esta parte, ni sus
-- auxiliares, puede llamarse a sí misma).
-- =====================================================================

-- B1
promedioSobreUmbral :: Double -> [Double] -> Maybe Double
promedioSobreUmbral = undefined

-- B2
movimientosCaballo :: (Int, Int) -> [(Int, Int)]
movimientosCaballo = undefined

enDosSaltos :: (Int, Int) -> Int
enDosSaltos = undefined

-- B3
miMap :: (a -> b) -> [a] -> [b]
miMap = undefined

miFilter :: (a -> Bool) -> [a] -> [a]
miFilter = undefined

miFoldl :: (b -> a -> b) -> b -> [a] -> b
miFoldl = undefined
