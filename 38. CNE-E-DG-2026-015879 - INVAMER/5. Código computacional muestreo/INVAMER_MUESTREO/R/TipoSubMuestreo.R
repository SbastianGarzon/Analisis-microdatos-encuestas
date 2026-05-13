
# Se inicia el análisis con Follow Brand:
# SE REALIZA EN LAS ZONAS DE, Y
# ingreso las cuotas segun el F34 en este orden:
# 11001 --> Bogota
# 05001 --> Medellin
# 76001 --> Cali
# 08001 --> Barranquilla
# 13001 --> Cartagena
# 54001 --> Cúcuta
# 68001 --> Bucaramanga
# 17001 --> Manizales
# 66001 --> Pereira

# cuotas1 = data.frame(
#      COD_MUNICIPIO = c('11001', '05001', '76001', '08001', '13001', '54001', '68001', '17001', '66001')
#    , cuotas = c(
#          40 #BOGOTA
#         ,20 #MEDELLIN
#         ,7 #CALI
#         ,7 #BARRANQUILLA
#         ,7 #CARTAGENA
#         ,7 #CUCUTA
#         ,7 #BUCARAMANGA
#         ,7 #MANIZALES
#         ,7 #PEREIRA
#     )
# )
#
#
# cuotas= data.frame(
#     COD_MUNICIPIO = c(rep('11001',5), rep('05001',5), rep('76001',5), rep('08001',5), rep('13001',4)
#                , rep('54001',3), rep('68001',4), rep('17001',3), rep('66001',4)
#     )
#     ,ESTRATO = c(  rep(2:6, 4)
#                  , 2:5
#                  , 2:4
#                  , 2:5
#                  , 2:4
#                  , 2:5)
#     ,cuotas = c(
#          8, 6, 4, 3, 3 #BOGOTA
#        , 8, 6, 4, 3, 2 #MEDELLIN
#        , 8, 6, 4, 3, 2 #CALI
#        , 8, 6, 4, 2, 1 #BARRANQUILLA
#        , 4, 3, 1, 1 #CARTAGENA
#        , 4, 3, 2 #CUCUTA
#        , 8, 6, 2, 1 #BUCARAMANGA
#        , 4, 3, 2 #MANIZALES
#        , 4, 3, 1, 1 #PEREIRA
#        )
# )
#
#
#
# library(odbc)
# con <- DBI::dbConnect(odbc::odbc(),
#                       Driver   = "SQL Server",
#                       Server   = "CLEANING\\CLEANINGDB",
#                       Database = "datos_analisis_estadistico",
#                       UID      = "Rodriguezo",
#                       PWD      = "Invamer2022*",
#                       Port     = 1433)
#
#
# t1 <- Sys.time()
# data <- tbl(con, "Marco_Manzanas_2019") %>% data.frame()
# t2 <- Sys.time()
# t2-t1
