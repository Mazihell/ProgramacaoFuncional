module Main where

--1. Escreva uma função chamada soma1 que recebe um inteiro como --argumento e retorna um
--inteiro uma unidade maior que a entrada.--
soma1::Int -> Int -> Int
soma1 x y = x + y

--2. Escreva uma função chamada sempre que, não importando o valor de ---entrada, devolva
--sempre zero. Observe que neste caso a entrada pode ser de qualquer ----tipo.
sempre::Num a => a -> a
sempre x = 0

--3. Escreva uma função chamada treco que receba três valores em ponto flutuantes com
--precisão dupla e retorne o resultado da soma dos dois primeiros --multiplicado pelo terceiro.
treco:: Float -> Float -> Float -> Float
treco x y z = (x + y) * z

--4. Escreva uma função chamada resto que devolva o resto de uma divisão entre dois números
--inteiros.
resto:: Int -> Int -> Int
resto x y = mod x y

--5. Escreva uma função chamada precoMaior que devolva o maior valor --entre quatro valores monetários.
maior1:: Double -> Double -> Double
maior2:: Double -> Double -> Double
precoMaior:: Double -> Double -> Double -> Double -> Double
maior1 a b = if a > b then a else b
maior2 c d = if c > d then c else d
precoMaior a b c d = if (maior1 a b) > (maior2 c d) then (maior1 a b) else (maior2 c d)

--6. Escreva uma função chamada impar que devolva True, sempre que o resultado do produto
--de dois números inteiros for ímpar.
impar1:: Int -> Int -> Int
impar:: Int -> Int -> Bool
impar1 x y = x * y
impar x y = if mod (impar1 x y) 2 == 0 then False else True

--7. Em Haskell existe o tipo par cuja assinatura tem a seguinte forma: 𝑝𝑎𝑟 ∷ (𝐼𝑛𝑡, 𝐼𝑛𝑡). Escreva
--uma função em Haskell que devolva a soma dos componentes de um par --de inteiros.
par::(Int,Int) -> Int
par (x,y) = x + y

--8. Escreva uma função em Haskell que receba números reais (double) e devolva o resultado
--da equação 𝑥**2 + y/2 + z
equacao::Double -> Double -> Double -> Double
equacao x y z = x**2 +  y/2 + z

--9. Escreva uma função em Haskell chamada diagnostico que receba o peso --do aluno e imprima
--um diagnóstico de obesidade, segundo a tabela que pode ser encontrada no link:
--Sobrepeso, obesidade e obesidade mórbida: entenda a diferença entre --os três termos
--(cuidadospelavida.com.br). Observe que este diagnóstico é meramente --estatístico e não
--tem nenhum valor real, está sendo usado nesta questão apenas para a --definição das faixas.
--Todo e qualquer diagnóstico deve ser feito por um profissional médico
diagnostico:: Float -> Float -> String
diagnostico p a | (p /(a**2)) < 17.0 = "Muito abaixo do peso" | (p/(a**2)) >= 17.0 && (p/(a**2)) < 18.5 = "Abaixo do peso" | (p/(a**2)) >= 18.5 && (p/(a**2)) < 25.0 = "Peso Normal" | (p/(a**2)) >= 25.0 && (p/(a**2)) < 30.0 = "Sobrepeso" | (p/(a**2)) >= 30 && (p/(a**2)) < 35.0 = "Obesidade leve" | (p/(a**2)) >= 35.0 && (p/(a**2)) < 40.0 = "Obesidade severa" | (p/(a**2)) >= 40 = "Obesidade morbida"

--10. Escreva uma função em Haskell chamada bissexto que receba um ano e devolva True se o
--ano for bisexto sabendo que anos bissextos obedecem a seguinte regra:
--𝑇𝑜𝑑𝑜𝑠 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠𝑒𝑗𝑎𝑚 𝑑𝑖𝑣𝑖𝑠í𝑣𝑒𝑖𝑠 𝑝𝑜𝑟 4
--𝐸𝑥𝑐𝑒𝑡𝑜 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠ã𝑜 𝑚ú𝑙𝑡𝑖𝑝𝑙𝑜𝑠 𝑑𝑒 100
--𝐸𝑥𝑐𝑒𝑡𝑜 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠ã𝑜 𝑚ú𝑙𝑡𝑖𝑝𝑙𝑜𝑠 𝑑𝑒 400
--1997 não é bissexto, 1900 não é bissexto e 2000 é bissexto.
bissexto::Int -> Bool
bissexto ano = if mod ano 4 == 0 && mod ano 100 == 0 && mod ano 400 == 0 then True else False

main :: IO()
main = do

putStrLn $ ("Func.1: entrada 50; 55 resultado " ++ show(soma1 50 55))
putStrLn $("Func.2: entrada 5.0; resultado " ++ show(sempre 5.0))
putStrLn $("Func.3: entrada 3.0; 7.0; 2.0 resultado " ++ show(treco 3.0 7.0 2.0))
putStrLn $("Func.4: entrada 5; 3 resultado " ++ show(resto 5 3))
putStrLn $("Func.5: entrada 10.50; 9.12; 23.15; 41.05 resultado " ++ show(precoMaior 10.50 9.12 23.15 41.05))
putStrLn $("Func.6: entrada 3; 2 resultado " ++ show(impar 3 2))
putStrLn $("Func.7: entrada 2; 7 resultado " ++ show(par(2,7)))
putStrLn $("Func.8: entrada 2; 5; 7 resultado " ++ show(equacao 2 5 7))
putStrLn $("Func.9: entrada 70.0; 1.71 resultado " ++ show(diagnostico 70.0 1.71))
putStrLn $("Func.10: entrada 1900 resultado " ++ show(bissexto 1900))
