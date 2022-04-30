type Producto = (String, Float)

nombreDelProducto :: Producto -> String
nombreDelProducto (nombre, _) = nombre

precioDelProducto :: Producto -> Float
precioDelProducto (_,precio) = precio

-- En la funcion precioTotal me da error, yo supongo que es por los tipos! pero no logro ver el error 

--precioTotal :: Producto -> Float -> Float -> Float -> Float
--precioTotal producto cantidad descuento envio = aplicarCostoDeEnvio ((*cantidad). aplicarDescuento producto $ descuento)envio 
--precioTotal producto cantidad descuento envio = (+) (aplicarDescuento producto cantidad descuento) . (aplicarCostoEnvio envio) 

productoDeElite :: Producto -> Bool
productoDeElite unProducto = productoDeLujo unProducto && productoCodiciado unProducto

aplicarDescuento :: Producto -> Float -> Float
aplicarDescuento unProducto descuento = precioDelProducto unProducto - descuento

entregaSencilla :: String -> Bool
entregaSencilla unDia = even . length $ unDia

descodiciarProducto :: Producto -> String
descodiciarProducto unProducto = take 10 (nombreDelProducto  unProducto)

productoDeLujo :: Producto -> Bool
productoDeLujo unProducto = 'x' `elem` nombreDelProducto unProducto || 'z' `elem` nombreDelProducto unProducto

aplicarCostoDeEnvio :: Producto -> Float -> Float
aplicarCostoDeEnvio unProducto precioDeEnvio = precioDelProducto unProducto + precioDeEnvio

productoCodiciado' :: Producto -> Int
productoCodiciado' unNombre = length . nombreDelProducto $  unNombre

productoCodiciado :: Producto -> Bool
productoCodiciado  unNombre = productoCodiciado' unNombre > 10

productoCorriente :: Producto -> Bool
productoCorriente unProducto =  (primeraLetraVocal . nombreDelProducto) $ unProducto

primeraLetraVocal :: String -> Bool
primeraLetraVocal nombre = (elem . head $ nombre) "aeiouAEIOU"

productoXL :: Producto -> String
productoXL unProducto = nombreDelProducto unProducto ++ "XL"

versionBarata :: Producto -> String
versionBarata unProducto = reverse. descodiciarProducto $ unProducto 