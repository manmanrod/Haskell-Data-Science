module Graficos (
  graficoCircular
, graficoBarras
, graficoEjes
) where

import Graphics.GChart
import Data.Dates

-- ------------------------------------------------------------------------
-- Modulo que continene las funciones necesarias para crear los gráficos circular y de
-- barras a partir de los parámetros: 
--          - Título: String
--          - Valores: [Int]
--          - Etiquetas: [String]
--
--  Salida: URL de tipo String
-- ------------------------------------------------------------------------



listaColores :: [String]
listaColores = ["065535", "ffa500","bada55","cc0000","ff1493","2acaea","ffff66","00ff7f","CCFFCC" ]

graficoCircular :: String -> [Int] -> [String] -> String
graficoCircular titulo valores etiquetas = getChartUrl $ do setChartSize 600 300
                                                            setDataEncoding simple
                                                            setChartType Pie
                                                            addChartData valores
                                                            setChartTitle titulo
                                                            setLabels etiquetas
                                                            setColors (take (length etiquetas) listaColores)

graficoBarras :: String -> [Int] -> [String] -> String
graficoBarras titulo valores etiquetas = getChartUrl $ do setChartSize 600 300
                                                          setDataEncoding simple
                                                          setChartType BarHorizontalGrouped
                                                          addChartData valores
                                                          setChartTitle titulo
                                                          addAxis $ makeAxis {  axisType = AxisBottom }
                                                          addAxis $ makeAxis {  axisType = AxisLeft,
                                                                                axisLabels = Just etiquetas }
                                                          addColor "065535"


graficoEjes :: String -> [(Float,Float)] -> String -> String -> String
graficoEjes titulo valores d1 d2 = getChartUrl $ do setChartSize 600 300
                                                    setChartType LineXY
                                                    setDataEncoding text
                                                    setChartTitle titulo
                                                    setGrid $ makeGrid { xAxisStep = 100,
                                                                         yAxisStep = 100,
                                                                         lineSegmentLength = Just 1,
                                                                         blankSegmentLength = Just 3 }
                                                    addAxis $ makeAxis { axisType = AxisLeft,
                                                                         axisRange = Just $ Range (0,50) (Just 25) }
                                                    addAxis $ makeAxis { axisType = AxisBottom,
                                                                         axisLabels = Just $ [d1] ++ [d2] }
                                                    addChartDataXY valores
                                                    addColor "458B00"
