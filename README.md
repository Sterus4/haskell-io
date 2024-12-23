# Лабораторная работа номер 3

Автор: Нигаматуллин Степан P3334

[Требования и задачния лабораторной](https://gitlab.se.ifmo.ru/functional-programming/main/-/blob/master/laboratory-course.md)

В данной лабораторной работе реализовано два метода интерполяции:
- Линейная (с окном данных в 2 точки)
- Лагранжа (с окном данных в 3 точки)

Использование программы:
``` bash
stack build
stack exec haskell-io-exe <step> <method>
```
- `step` - Шаг интерполяции
- `method` - Метод интерполяции:
    - Четный:   Линейная
    - Нечетный: Лагранжа


## Реализация

Библиотека реализована в файлах:
- [Lib.hs](./src/Lib.hs) - Ответственность за управление вводом-выводом и выбором метода
- [Interpolation.hs](./src/Interpolation.hs) - Ответственность за интерполяцию

### Ввод-вывод:

```haskell
printer :: Show a => [a] -> IO ()
printer [] = putStr ""
printer (x:xs) = do
    print x
    printer xs


computeForLinear :: [b] -> [(b, b)]
computeForLinear a =
    zip a (drop 1 a)

computeForLagrange :: [b] -> [(b, b, b)]
computeForLagrange a =
    zip3 a (drop 1 a) (drop 2 a)

someFunc = do
    args <- getArgs
    if length args /= 2
        then putStrLn "Usage: stack exec haskell-io-exe <step> <method>"
        else do
            let step = read (head args) :: Double
            let method = mod (read (head $ tail args) :: Integer) 2
            let content = fmap lines getContents
            string_content <- content
            let double_content = fmap formatter string_content where
                formatter l = (w1, w2) where
                    w1 = read $ head $ words l          :: Double
                    w2 = read $ head $ tail $ words l   :: Double
            let dataLinear =    computeForLinear    double_content
            let dataLagrange =  computeForLagrange  double_content
            let result = if method == 0 
                then fmap (lineInterpolation step) dataLinear 
                else fmap (lagrInterpolation step) dataLagrange
            mapM_ printer result
```

### Интерполяция

Методы интерполяции просто реализуют всю математичку над заданным интервалом. Здесь используется упрощение: Интерполяция по лагранжу реализуется всегда параболой. При желании можно было бы ввести параметр степени многочлена, но это сильно усложнило бы разработку. Основа лабораторной работы же - ввод-вывод.

```haskell
type Point a = (a, a)

lineInterpolation :: Double -> (Point Double, Point Double) -> [Point Double]
lineInterpolation step ((x0, y0), (x1, y1)) =
    let xCoordinates = takeWhile (< x1 + step) [x0, x0 + step .. ] in
        [(i, take_y_from_x i) | i <- xCoordinates] where
            take_y_from_x x = 
                (y0 * (x1 - x) + y1 * (x - x0)) / (x1 - x0)

lagrInterpolation :: Double -> (Point Double, Point Double, Point Double) -> [Point Double]
lagrInterpolation step ((x0, y0), (x1, y1), (x2, y2)) =
    let xCoordinates = takeWhile (< x2 + step) [x0, x0 + step .. ] in
        [(i, take_y_from_x i) | i <- xCoordinates] where
            take_y_from_x x = 
                (x - x1) * (x - x2) * y0 / ((x0 - x1) * (x0 - x2)) + 
                (x - x0) * (x - x2) * y1 / ((x1 - x0) * (x1 - x2)) + 
                (x - x0) * (x - x1) * y2 / ((x2 - x0) * (x2 - x1))
```

## Примеры использования

### Линейная:
``` bash
> stack exec haskell-io-exe 1 0
0 0
4 2
(0.0,0.0)
(1.0,0.5)
(2.0,1.0)
(3.0,1.5)
(4.0,2.0)
5.5 3
(4.0,2.0)
(5.0,2.6666666666666665)
(6.0,3.3333333333333335)
7 7
(5.5,3.0)
(6.5,5.666666666666667)
(7.5,8.333333333333334)
```

### Лагранжа:
``` bash
> stack exec haskell-io-exe 0.5 1
0 4
2 1
4 4
(0.0,4.0)
(0.5,2.6875)
(1.0,1.75)
(1.5,1.1875)
(2.0,1.0)
(2.5,1.1875)
(3.0,1.75)
(3.5,2.6875)
(4.0,4.0)
5 5
(2.0,1.0)
(2.5,1.875)
(3.0,2.666666666666666)
(3.5,3.375)
(4.0,4.0)
(4.5,4.541666666666667)
(5.0,5.0)
```
## Выводы

Лабораторная работа была довольно непростой, работать с вводом-вывода в новой парадигме было очень непривычно. Во время работы я полностью прочувствовал на себе утерждение: "В Haskell вы не можете для отладки в любом месте поставить print".

Была продемонстрирована работа с ленью - вместо того, чтобы реализовывать бесконечный цикл с ожиданием ввода-вывода была использована ленивая функция GetContents, благодаря которой удалось реализовать потоковую обработку данных линия за линией.
