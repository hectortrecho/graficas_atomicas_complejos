#!\\usr\\bin\\env Rscript

# LIBRERIAS ---------------------------------------------------------------
library("igraph",quietly = T)             #Libreria con las funciones para la aplicacion de teoria de grafica.
library("matlib",quietly = T)             #Libreria con funciones para la aplicacion de algebra lineal.
library("rlist",quietly = T)              #Libreria que contiene la funcion list.append que necesito para agregar elementos a listas.
library("pracma",quietly = T)             #Libreria con la funcion para calcular el polynomio caracteristico, usando el metodo Faddeew-Leverrier.
library("readxl",quietly = T)             #Libreria para importar el .xls con la funcion read.xls
library("dplyr",quietly = T)              #Libreria para usar pipes.
library("ggplot2",quietly = T)            #Libreria para graficar. 
library("ggstatsplot",quietly = T)        #Libreria con funciones estadisticas para remover outliners
library("reshape2",quietly = T)           #mapa de calor
library("tidyverse",quietly = T)          #formato de pivot longer
library("Matrix",quietly = T)
library("reticulate",quietly = T)         #para usar modulos,libreria y codigo de python
library("ggpie",quietly=T)
library("xtable",quietly=T)
#MACHINE LEARNING
library("factoextra",quietly = T) 
library("torch",quietly = T)
library("luz",quietly = T) 
library("rsample",quietly = T)            # for data splitting
library("caTools",quietly = T)
# Modeling packages
library("caret",quietly = T)              # for logistic regression modeling
library("class",quietly = T)
# Model interpretability packages
library("vip",quietly = T)                # variable importance
#ESPECIFICANDO VARIABLES Y RUTAS
#use_python("~Documents/codigo_python/investigacion_graficas_atomicas/TESIS")                 #especificando el ambiente de trabajo de python.
#  MATRIZ OCTAEDRO --------------------------------------------------------
#matrices de conectividad graficas atomicas de complejos de acuosos.
matriz_Co2 <- matrix(c(0,1,0,0,1,0,0,1,
                       1,0,0,0,0,1,1,0,
                       0,0,0,1,1,0,0,1,
                       0,0,1,0,0,1,1,0,
                       1,0,1,0,0,0,1,0,
                       0,1,0,1,0,0,0,1,
                       0,1,0,1,1,0,0,0,
                       1,0,1,0,0,1,0,0),nrow = 8,ncol = 8,byrow = TRUE)
matriz_Cr2 <- matrix(c(0,2,2,0,
                       2,0,0,2,
                       2,0,0,2,
                       0,2,2,0),nrow = 4, ncol = 4, byrow = TRUE)
matriz_Cr3 <- matrix(c(0,0,0,1,0,1,1,0,
                       0,0,1,0,1,0,0,1,
                       0,1,0,0,0,1,1,0,
                       1,0,0,0,1,0,0,1,
                       0,1,0,1,0,0,1,0,
                       1,0,1,0,0,0,0,1,
                       1,0,1,0,1,0,0,0,
                       0,1,0,1,0,1,0,0),nrow = 8, ncol = 8, byrow = TRUE)
matriz_Cu2 <- matrix(c(0,0,2,2,
                       0,0,2,2,
                       2,2,0,0,
                       2,2,0,0),nrow= 4, ncol = 4, byrow = TRUE)
matriz_Fe2 <- matrix(c(0,0,1,1,1,1,
                       0,0,1,1,1,1,
                       1,1,0,0,0,0,
                       1,1,0,0,0,0,
                       1,1,0,0,0,0,
                       1,1,0,0,0,0),nrow = 6, ncol = 6, byrow = TRUE)
matriz_Fe3 <- matrix(c(0,0,1,1,1,1,
                       0,0,1,1,1,1,
                       1,1,0,0,1,1,
                       1,1,0,0,1,1,
                       1,1,1,1,0,0,
                       1,1,1,1,0,0),nrow = 6, ncol = 6, byrow = TRUE)
matriz_Mn2 <- matrix(c(0,0,1,1,1,1,
                       0,0,1,1,1,1,
                       1,1,0,0,1,1,
                       1,1,0,0,1,1,
                       1,1,1,1,0,0,
                       1,1,1,1,0,0),nrow = 6, ncol = 6, byrow = TRUE)
matriz_Mn3 <- matrix(c(0,0,0,0,1,1,
                       0,0,0,0,1,1,
                       0,0,0,0,1,1,
                       0,0,0,0,1,1,
                       1,1,1,1,0,0,
                       1,1,1,1,0,0),nrow = 6, ncol = 6, byrow = TRUE)
matriz_Mn4 <- matrix(c(0,0,1,0,0,1,0,1,
                       0,0,0,1,1,0,1,0,
                       1,0,0,0,1,0,1,0,
                       0,1,0,0,0,1,0,1,
                       0,1,1,0,0,0,0,1,
                       1,0,0,1,0,0,1,0,
                       0,1,1,0,0,1,0,0,
                       1,0,0,1,1,0,0,0),nrow = 8, ncol = 8, byrow = TRUE)
matriz_Ni2 <- matrix(c(0,0,1,0,1,0,1,0,
                       0,0,0,1,1,0,1,0,
                       1,0,0,0,0,1,0,1,
                       0,1,0,0,0,1,0,1,
                       1,1,0,0,0,0,0,1,
                       0,0,1,1,0,0,1,0,
                       1,1,0,0,0,1,0,0,
                       0,0,1,1,1,0,0,0),nrow = 8, ncol = 8, byrow = TRUE)
matriz_Ni3 <- matrix(c(0,0,1,1,1,1,
                       0,0,1,1,1,1,
                       1,1,0,0,1,1,
                       1,1,0,0,1,1,
                       1,1,1,1,0,0,
                       1,1,1,1,0,0), nrow = 6, ncol = 6, byrow = TRUE)
matriz_Sc2 <- matrix(c(0,0,1,0,1,0,1,0,
                       0,0,0,1,0,1,0,1,
                       1,0,0,0,0,1,0,1,
                       0,1,0,0,1,0,1,0,
                       1,0,0,1,0,1,0,0,
                       0,1,1,0,1,0,0,0,
                       1,0,0,1,0,0,0,1,
                       0,1,1,0,0,0,1,0), nrow = 8, ncol = 8, byrow = TRUE)
matriz_Sc3 <- matrix(c(0,0,1,1,1,1,
                       0,0,1,1,1,1,
                       1,1,0,0,1,1,
                       1,1,0,0,1,1,
                       1,1,1,1,0,0,
                       1,1,1,1,0,0), nrow = 6, ncol = 6, byrow = TRUE)
matriz_Ti2 <- matrix(c(0,0,1,1,1,1,
                       0,0,1,1,1,1,
                       1,1,0,1,1,0,
                       1,1,1,0,0,1,
                       1,1,1,0,0,1,
                       1,1,0,1,1,0), nrow = 6, ncol = 6, byrow = TRUE)
matriz_Ti3 <- matrix(c(0,0,1,0,1,0,0,1,
                       0,0,0,1,0,1,1,0,
                       1,0,0,0,0,1,1,0,
                       0,1,0,0,1,0,0,1,
                       1,0,0,1,0,0,1,0,
                       0,1,1,0,0,0,0,1,
                       0,1,1,0,1,0,0,0,
                       1,0,0,1,0,1,0,0), nrow = 8, ncol = 8, byrow = TRUE)
matriz_V2 <- matrix(c(0,0,0,1,1,0,0,1,
                      0,0,1,0,0,1,1,0,
                      0,1,0,0,1,0,0,1,
                      1,0,0,0,0,1,1,0,
                      1,0,1,0,0,0,1,0,
                      0,1,0,1,0,0,0,1,
                      0,1,0,1,1,0,0,0,
                      1,0,1,0,0,1,0,0), nrow = 8, ncol = 8, byrow = TRUE)
matriz_V3 <- matrix(c(0,0,1,1,1,1,
                      0,0,1,1,1,1,
                      1,1,0,0,1,1,
                      1,1,0,0,1,1,
                      1,1,1,1,0,0,
                      1,1,1,1,0,0), nrow = 6, ncol = 6, byrow = TRUE)
matriz_V4 <- matrix(c(0,0,1,0,0,1,1,0,
                      0,0,0,1,1,0,0,1,
                      1,0,0,0,1,0,0,1,
                      0,1,0,0,0,1,1,0,
                      0,1,1,0,0,0,1,0,
                      1,0,0,1,0,0,0,1,
                      1,0,0,1,1,0,0,0,
                      0,1,1,0,0,1,0,0), nrow = 8, ncol = 8, byrow = TRUE)
matriz_Zn2 <- matrix(c(0,0,1,1,1,1,
                       0,0,1,1,1,1,
                       1,1,0,1,1,0,
                       1,1,1,0,0,1,
                       1,1,1,0,0,1,
                       1,1,0,1,1,0), nrow = 6, ncol = 6, byrow = TRUE)

# MATRIZ CU-PHEN ----------------------------------------------------------
#Matriz de conectividad Cu-phen
matriz_Cu_phen_s0 <- matrix(c(0,1,1,1,
                             1,0,1,1,
                             1,1,0,1,
                             1,1,1,0),nrow = 4, ncol = 4, byrow = TRUE)
matriz_Cu_phen_s1 = matrix(c(0,0,1,1,1,1,
                              0,0,1,1,1,1,
                              1,1,0,0,1,0,
                              1,1,0,0,0,1,
                              1,1,1,0,0,0,
                              1,1,0,1,0,0),nrow = 6, ncol = 6, byrow = TRUE)
matriz_Cu_phen_s2 = matrix(c(0,0,1,1,1,1,
                             0,0,1,1,1,1,
                             1,1,0,0,1,0,
                             1,1,0,0,0,1,
                             1,1,1,0,0,0,
                             1,1,0,1,0,0),nrow = 6, ncol = 6, byrow = TRUE)
matriz_Cu_phen_s3 = matrix(c(0,0,2,2,
                              0,0,2,2,
                              2,2,0,0,
                              2,2,0,0), nrow = 4, ncol = 4, byrow = TRUE)
matriz_Cu_phen_s4 = matrix(c(0,0,1,1,
                              0,0,1,1,
                              1,1,0,2,
                              1,1,2,0), nrow = 4, ncol = 4, byrow = TRUE)
matriz_Cu_phen_s5 = matrix(c(0,0,1,1,
                              0,0,1,1,
                              1,1,0,2,
                              1,1,2,0), nrow = 4, ncol = 4, byrow = TRUE)
matriz_Cu_phen_s6 = matrix(c(0,0,1,1,
                              0,0,1,1,
                              1,1,0,2,
                              1,1,2,0), nrow = 4, ncol = 4, byrow = TRUE)
matriz_Cu_phen_s7 = matrix(c(0,0,1,1,0,1,
                              0,0,0,1,0,1,
                              1,0,0,0,1,0,
                              1,1,0,0,1,0,
                              0,0,1,1,0,1,
                              1,1,0,0,1,0), nrow = 6, ncol = 6, byrow = TRUE)
matriz_Cu_phen_s8 = matrix(c(0,1,0,0,1,1,
			     1,0,0,1,0,0,
			     0,0,0,0,1,1,
			     0,1,0,0,1,1,
			     1,0,1,1,0,0,
			     1,0,1,1,0,0), nrow = 6, ncol = 6, byrow = TRUE)
matriz_Cu_phen_s9 = matrix(c(0,0,1,1,
			     0,0,1,1,
			     1,1,0,2,
			     1,1,2,0), nrow = 4, ncol = 4, byrow = TRUE)
matriz_Cu_phen_s10 = matrix(c(0,2,2,0,0,
			      2,0,0,1,1,
			      2,0,0,1,1,
			      0,1,1,0,1,
			      0,1,1,1,0), nrow = 5, ncol = 5, byrow = TRUE)
matriz_Cu_phen_s11 = matrix(c(0,2,2,0,0,
			      2,0,0,1,1,
			      2,0,0,1,1,
			      0,1,1,0,1,
			      0,1,1,1,0), nrow = 5, ncol = 5, byrow = TRUE)
matriz_Cu_phen_s12 = matrix(c(0,0,1,1,
			      0,0,1,1,
			      1,1,0,2,
			      1,1,2,0), nrow = 4, ncol = 4, byrow = TRUE)
matriz_Cu_phen_s13 = matrix(c(0,0,1,1,
			      0,0,1,1,
			      1,1,0,2,
			      1,1,2,0), nrow = 4, ncol = 4, byrow = TRUE)
matriz_Cu_phen_s14 = matrix(c(0,1,0,1,
			      1,0,1,2,
			      0,1,0,1,
			      1,2,1,0), nrow = 4, ncol = 4, byrow = TRUE)
matriz_Cu_phen_s15 = matrix(c(0,0,1,1,
			      0,0,1,1,
			      1,1,0,2,
			      1,1,2,0), nrow = 4, ncol = 4, byrow = TRUE)
matriz_Cu_phen_s16 = matrix(c(0,1,1,0,1,0,
			                        1,0,0,1,0,1,
			                        1,0,0,0,0,1,
			                        0,1,0,0,1,0,
			                        1,0,0,1,0,1,
			                        0,1,1,0,1,0), nrow = 6, ncol = 6, byrow = TRUE)
matriz_Cu_phen_s17 = matrix(c(0,2,2,2,
			      2,0,1,0,
			      2,1,0,0,
			      2,0,0,0), nrow = 4, ncol = 4, byrow = TRUE)
matriz_Cu_phen_s18 = matrix(c(0,2,2,2,
			      2,0,0,0,
			      2,0,0,0,
			      2,0,0,0), nrow = 4, ncol = 4, byrow = TRUE)
matriz_Cu_phen_s19 = matrix(c(0,0,1,1,1,1,
			      0,0,1,1,1,1,
			      1,1,0,1,0,0,
			      1,1,1,0,0,0,
			      1,1,0,0,0,1,
			      1,1,0,0,1,0), nrow = 6, ncol = 6, byrow = TRUE)
matriz_Cu_phen_s20 = matrix(c(0,2,2,0,0,
			      2,0,0,1,1,
			      2,0,0,1,1,
			      0,1,1,0,1,
			      0,1,1,1,0), nrow = 5, ncol = 5, byrow = TRUE)
#Matriz de conectividad de complejos con geometria cuadrada plana (CP) - sin optimizar
matriz_Co2_CP <- matrix(c(0,0,1,0,0,1,0,1,
                          0,0,0,1,1,0,1,0,
                          1,0,0,0,1,0,1,0,
                          0,1,0,0,0,1,0,1,
                          0,1,1,0,0,0,0,1,
                          1,0,0,1,0,0,1,0,
                          0,1,1,0,0,1,0,0,
                          1,0,0,1,1,0,0,0),nrow = 8, ncol= 8,byrow = TRUE)
matriz_Cr3_CP <- matrix(c(0,1,1,1,0,0,
                          1,0,1,0,1,1,
                          0,1,0,1,1,0,
                          1,0,1,0,1,1,
                          0,1,1,1,0,0,
                          1,1,0,1,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_Fe3_CP <- matrix(c(0,1,1,1,0,0,
                          1,0,0,1,1,1,
                          1,0,0,1,1,1,
                          1,1,1,0,0,0,
                          0,1,1,0,0,1,
                          0,1,1,0,1,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_Ni2_CP <- matrix(c(0,2,0,2,
                          2,0,2,0,
                          0,2,0,2,
                          2,0,2,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_Cu2_CP <- matrix(c(0,2,0,2,
                          2,0,2,0,
                          0,2,0,2,
                          2,0,2,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_Mn2_CP <- matrix(c(0,2,1,2,
                          2,0,1,2,
                          2,1,0,0,
                          1,2,0,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_Mn4_CP <- matrix(c(0,1,1,0,0,0,
                          1,0,0,1,1,1,
                          1,0,0,1,1,1,
                          0,1,1,0,0,0,
                          0,1,1,0,0,0,
                          0,1,1,0,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_Ni3_CP <- matrix(c(0,0,1,1,1,1,
                          0,0,1,1,1,1,
                          1,1,0,0,1,1,
                          1,1,0,0,0,1,
                          1,1,1,0,0,0,
                          1,1,0,1,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_Sc2_CP <- matrix(c(0,1,1,0,1,1,
                          1,0,0,1,1,1,
                          1,0,0,1,1,1,
                          0,1,1,0,1,1,
                          1,1,1,1,0,0,
                          1,1,1,1,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_Sc3_CP <- matrix(c(0,0,1,1,1,1,
                          0,0,1,1,1,1,
                          1,1,0,0,1,0,
                          1,1,0,0,1,0,
                          1,1,1,1,0,0,
                          1,1,0,0,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_Ti3_CP <- matrix(c(0,1,1,0,1,0,
                          1,0,0,1,1,1,
                          1,0,0,1,1,1,
                          0,1,1,0,1,1,
                          1,1,1,1,0,0,
                          0,1,1,1,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_V2_CP <- matrix(c(0,0,0,0,1,1,
                         0,0,1,1,0,0,
                         0,1,0,1,1,0,
                         0,1,1,0,0,1,
                         1,0,1,0,0,1,
                         1,0,0,1,1,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_V4_CP <- matrix(c(0,1,1,1,2,
                          1,0,1,1,1,
                          1,1,0,1,0,
                          1,1,1,0,0,
                          2,1,0,0,0),nrow = 5,ncol = 5,byrow = TRUE)
matriz_Zn2_CP  <- matrix(c(0,2,0,2,
                           2,0,2,0,
                           0,2,0,1,
                           2,0,1,0),nrow = 4,ncol = 4,byrow = TRUE)
#Matriz de conectividad de complejos con geometria tetraedrica (T) - sin optimizar
matriz_Co2_T <- matrix(c(0,0,1,1,1,1,
                         0,0,1,1,1,1,
                         1,1,0,0,1,1,
                         1,1,0,0,1,1,
                         1,1,1,1,0,0,
                         1,1,1,1,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_Cr2_T  <- matrix(c(0,1,1,0,1,1,
                          1,0,0,1,1,1,
                          1,0,0,1,1,1,
                          0,1,1,0,1,1,
                          1,1,1,1,0,0,
                          1,1,1,1,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_Cr3_T <- matrix(c(0,0,1,1,1,1,
                         0,0,1,1,1,1,
                         1,1,0,0,1,1,
                         1,1,0,0,1,1,
                         1,1,1,1,0,0,
                         1,1,1,1,0,0),nrow = 6, ncol = 6,byrow = TRUE)
matriz_Cu2_T <- matrix(c(0,1,2,0,
                         1,0,0,2,
                         2,0,0,2,
                         0,2,2,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_Fe3_T <- matrix(c(0,1,1,0,1,1,
                         1,0,0,1,1,0,
                         1,0,0,1,0,1,
                         0,1,1,0,1,1,
                         1,1,0,1,0,0,
                         1,0,1,1,0,0),nrow = 6,ncol = 6,byrow = TRUE) 
matriz_Mn2_T  <- matrix(c(0,2,1,0,
                          2,0,0,1,
                          1,0,0,1,
                          1,0,1,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_Mn3_T <- matrix(c(0,0,1,1,1,1,
                         1,0,0,1,1,0,
                         1,0,0,1,0,1,
                         0,1,1,0,1,1,
                         1,1,0,1,0,0,
                         1,0,1,1,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_Mn4_T  <- matrix(c(0,0,1,1,1,1,
                          0,0,1,1,1,1,
                          1,1,0,0,1,1,
                          1,1,0,0,1,1,
                          1,1,1,1,0,0,
                          1,1,1,1,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_Ni3_T <- matrix(c(0,0,1,1,1,1,
                         0,0,1,1,1,1,
                         1,1,0,1,1,0,
                         1,1,1,0,0,1,
                         1,0,1,0,0,1,
                         1,1,0,1,1,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_Sc2_T <- matrix(c(0,2,1,1,
                         2,0,1,1,
                         1,1,0,0,
                         1,1,0,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_Sc3_T <- matrix(c(0,1,1,1,
                         1,0,1,1,
                         1,1,0,1,
                         1,1,1,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_Ti3_T <- matrix(c(0,0,0,1,0,1,
                         0,0,1,0,1,0,
                         0,1,0,1,0,1,
                         1,0,1,0,1,0,
                         0,1,0,1,0,1,
                         1,0,1,0,1,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_V4_T  <- matrix(c(0,0,1,0,1,1,
                         0,0,1,0,1,1,
                         1,1,0,1,0,0,
                         0,0,1,0,0,1,
                         1,1,0,0,0,0,
                         1,1,0,1,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_Zn2_T <- matrix(c(0,1,1,1,
                         1,0,1,1,
                         1,1,0,1,
                         1,1,1,0),nrow = 4,ncol = 4,byrow = TRUE)

# MATRIZ CUADRADA PLANA ---------------------------------------------------
#Matriz de complejos con geometria cuadrado plana optimizados la geometria.
matriz_Co2_CP_opt <- matrix(c(0,1,1,0,1,0,0,0,
                              1,0,0,1,0,1,0,0,
                              1,0,0,1,0,0,1,0,
                              0,1,1,0,0,0,0,1,
                              1,0,0,0,0,1,1,0,
                              0,1,0,0,1,0,0,1,
                              0,0,1,0,1,0,0,1,
                              0,0,0,1,0,1,1,0),nrow = 8,ncol = 8,byrow = TRUE)
matriz_Cr2_CP_opt <- matrix(c(0,0,1,1,1,1,
                              0,0,1,1,1,1,
                              1,1,0,1,1,0,
                              1,1,1,0,0,1,
                              1,1,1,0,0,1,
                              1,1,0,1,1,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_Cr3_CP_opt  <- matrix(c(0,0,0,1,0,1,1,0,
                               0,0,1,0,1,0,0,1,
                               0,1,0,1,0,0,1,0,
                               1,0,1,0,0,0,0,1,
                               0,1,0,0,0,1,1,0,
                               1,0,0,0,1,0,0,1,
                               1,0,1,0,1,0,0,0,
                               0,1,0,1,0,1,0,0),nrow = 8,ncol = 8,byrow = TRUE)
matriz_Cu2_CP_opt <- matrix(c(0,1,0,1,
                              1,0,0,1,
                              1,0,0,1,
                              0,1,1,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_Fe2_CP_opt <- matrix(c(0,1,0,0,1,0,1,0,
                              1,0,0,0,0,1,0,1,
                              0,0,0,1,1,0,1,0,
                              0,0,1,0,0,1,0,1,
                              1,0,1,0,0,1,0,0,
                              0,1,0,1,1,0,0,0,
                              1,0,1,0,0,0,0,1,
                              0,1,0,1,0,0,1,0),nrow = 8,ncol = 8,byrow = TRUE)
matriz_Fe3_CP_opt  <- matrix(c(0,2,2,0,
                               2,0,0,2,
                               2,0,0,2,
                               0,2,2,0),nrow = 4,ncol = ,byrow = TRUE)
matriz_Mn2_CP_opt <- matrix(c(0,1,2,0,
                              1,0,0,2,
                              2,0,0,1,
                              0,2,1,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_Mn3_CP_opt <- matrix(c(0,0,1,1,1,1,
                              0,0,1,1,1,1,
                              1,1,0,1,1,0,
                              1,1,1,0,0,1,
                              1,1,1,0,0,1,
                              1,1,0,1,1,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_Mn4_CP_opt <- matrix(c(0,1,0,0,1,0,1,0,
                              1,0,0,0,0,1,0,1,
                              0,0,0,1,1,0,1,0,
                              0,0,1,0,0,1,0,1,
                              1,0,1,0,0,1,0,0,
                              0,1,0,1,1,0,0,0,
                              1,0,1,0,0,0,0,1,
                              0,1,0,1,0,0,1,0),nrow = 8,ncol = 8,byrow = TRUE)
matriz_Ni2_CP_opt <- matrix(c(0,0,1,0,1,0,1,0,
                              0,0,0,1,0,1,0,1,
                              1,0,0,1,0,0,0,1,
                              0,1,1,0,0,0,1,0,
                              1,0,0,0,0,1,0,1,
                              0,1,0,0,1,0,1,0,
                              1,0,0,1,0,1,0,0,
                              0,1,1,0,1,0,0,0),nrow = 8,ncol = 8,byrow = TRUE)
matriz_Ni3_CP_opt <- matrix(c(0,0,1,0,1,0,1,0,
                              0,0,0,1,0,1,0,1,
                              1,0,0,1,0,0,0,1,
                              0,1,1,0,0,0,1,0,
                              1,0,0,0,0,1,0,1,
                              0,1,0,0,1,0,1,0,
                              1,0,0,1,0,1,0,0,
                              0,1,1,0,1,0,0,0),nrow = 8,ncol = 8,byrow = TRUE)
matriz_Sc2_CP_opt <- matrix(c(0,0,1,1,1,1,
                              0,0,1,1,1,1,
                              1,1,0,1,0,0,
                              1,1,1,0,0,0,
                              1,1,0,0,0,1,
                              1,1,0,0,1,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_Sc3_CP_opt <- matrix(c(0,0,1,1,1,1,
                              0,0,1,1,1,1,
                              1,1,0,1,0,0,
                              1,1,1,0,0,0,
                              1,1,0,0,0,1,
                              1,1,0,0,1,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_Ti2_CP_opt <- matrix(c(0,0,1,1,1,1,
                              0,0,1,1,1,1,
                              1,1,0,0,1,1,
                              1,1,0,0,1,1,
                              1,1,1,1,0,0,
                              1,1,1,1,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_Ti3_CP_opt <- matrix(c(0,0,1,1,1,1,
                              0,0,1,1,1,1,
                              1,1,0,0,1,1,
                              1,1,0,0,1,1,
                              1,1,1,1,0,0,
                              1,1,1,1,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_V2_CP_opt <- matrix(c(0,1,0,0,1,0,1,0,
                             1,0,0,0,0,1,0,1,
                             0,0,0,1,1,0,1,0,
                             0,0,1,0,0,1,0,1,
                             1,0,1,0,0,1,0,0,
                             0,1,0,1,1,0,0,0,
                             1,0,1,0,0,0,0,1,
                             0,1,0,1,0,0,1,0),nrow = 8,ncol = 8,byrow = TRUE)
matriz_V3_CP_opt <- matrix(c(0,0,1,1,1,1,
                             0,0,1,1,1,1,
                             1,1,0,0,1,1,
                             1,1,0,0,1,1,
                             1,1,1,1,0,0,
                             1,1,1,1,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_V4_CP_opt  <- matrix(c(0,0,1,1,1,1,
                              0,0,1,1,1,1,
                              1,1,0,0,1,1,
                              1,1,0,0,1,1,
                              1,1,1,1,0,0,
                              1,1,1,1,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_Zn2_CP_opt  <- matrix(c(0,0,2,2,
                               0,0,2,2,
                               2,2,0,0,
                               2,2,0,0),nrow = 4,ncol = 4,byrow = TRUE)

# MATRIZ TETRAEDRO --------------------------------------------------------
#Matriz de adyacencia de complejos tetraedricos con geometria optimizado
matriz_Co2_T_opt <- matrix(c(0,1,1,0,1,1,
                             1,0,0,1,1,1,
                             1,0,0,1,1,1,
                             0,1,1,0,1,1,
                             1,1,1,1,0,0,
                             1,1,1,1,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_Cr2_T_opt <- matrix(c(0,0,1,1,1,1,
                             0,0,1,1,1,1,
                             1,1,0,1,1,0,
                             1,1,1,0,0,1,
                             1,1,1,0,0,1,
                             1,1,0,1,1,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_Cr3_T_opt <- matrix(c(0,0,1,1,1,1,
                             0,0,1,1,1,1,
                             1,1,0,0,1,1,
                             1,1,0,0,1,1,
                             1,1,1,1,0,0,
                             1,1,1,1,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_Cu2_T_opt <- matrix(c(0,1,1,0,1,
                             1,0,1,2,0,
                             1,1,0,0,1,
                             0,2,0,0,1,
                             1,0,1,1,0),nrow = 5,ncol = 5,byrow = TRUE)
matriz_Fe2_T_opt <- matrix(c(0,1,0,0,0,1,1,0,
                             1,0,0,0,1,0,0,1,
                             0,0,0,1,0,1,1,0,
                             0,0,1,0,1,0,0,1,
                             0,1,0,1,0,0,1,0,
                             1,0,1,0,0,0,0,1,
                             1,0,1,0,1,0,0,0,
                             0,1,0,1,0,1,0,0),nrow = 8,ncol = 8,byrow = TRUE)
matriz_Fe3_T_opt <- matrix(c(0,0,1,1,1,1,
                             0,0,1,1,1,1,
                             1,1,0,0,1,0,
                             1,1,0,0,0,1,
                             1,1,1,0,0,0,
                             1,1,0,1,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_Mn2_T_opt <- matrix(c(0,0,1,1,1,0,
                             0,0,1,1,0,1,
                             1,1,0,0,1,1,
                             1,0,1,1,0,0,
                             1,0,1,1,0,0,
                             0,1,1,1,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_Mn3_T_opt <- matrix(c(0,0,1,1,1,1,
                             0,0,1,1,1,1,
                             1,1,0,0,1,1,
                             1,1,0,0,1,1,
                             1,1,1,1,0,0,
                             1,1,1,1,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_Mn4_T_opt <- matrix(c(0,0,1,1,1,1,
                             0,0,1,1,1,1,
                             1,1,0,0,1,1,
                             1,1,0,0,1,1,
                             1,1,1,1,0,0,
                             1,1,1,1,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_Ni2_T_opt <- matrix(c(0,0,1,0,0,1,1,0,
                             0,0,0,1,1,0,0,1,
                             1,0,0,1,0,0,0,1,
                             0,1,1,0,0,0,1,0,
                             0,1,0,0,0,1,1,0,
                             1,0,0,0,1,0,0,1,
                             1,0,0,1,1,0,0,0,
                             0,1,1,0,0,1,0,0),nrow = 8,ncol = 8,byrow = TRUE) 
matriz_Ni3_T_opt <- matrix(c(0,1,0,1,0,0,0,1,
                             1,0,0,0,1,0,1,0,
                             0,0,0,1,0,1,0,1,
                             1,0,1,0,0,0,1,0,
                             0,1,0,0,0,1,0,1,
                             0,0,1,0,1,0,1,0,
                             0,1,0,1,0,1,0,0,
                             1,0,1,0,1,0,0,0),nrow = 8,ncol = 8,byrow = TRUE)
matriz_Sc2_T_opt <- matrix(c(0,0,0,0,1,1,
                             0,0,0,0,1,1,
                             0,0,0,0,1,1,
                             0,0,0,0,1,1,
                             1,1,1,1,0,0,
                             1,1,1,1,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_Sc3_T_opt  <- matrix(c(0,1,1,1,
                              1,0,1,1,
                              1,1,0,1,
                              1,1,1,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_Ti2_T_opt <- matrix(c(0,0,1,1,1,1,
                             0,0,1,1,1,1,
                             1,1,0,0,1,1,
                             1,1,0,0,1,1,
                             1,1,1,1,0,0,
                             1,1,1,1,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_Ti3_T_opt <- matrix(c(0,1,0,0,0,1,1,0,
                             1,0,0,0,1,0,0,1,
                             0,0,0,1,1,0,1,0,
                             0,0,1,0,0,1,0,1,
                             0,1,1,0,0,0,0,0,
                             1,0,0,1,0,0,0,0,
                             1,0,1,0,0,0,0,0,
                             0,1,0,1,0,0,0,0),nrow = 8,ncol = 8,byrow = TRUE)
matriz_V2_T_opt <- matrix(c(0,0,1,1,1,1,
                            0,0,1,1,1,1,
                            1,1,0,0,1,1,
                            1,1,0,0,1,1,
                            1,1,1,1,0,0,
                            1,1,1,1,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_V3_T_opt <- matrix(c(0,0,1,1,1,1,
                            0,0,1,1,1,1,
                            1,1,0,0,1,1,
                            1,1,0,0,1,1,
                            1,1,1,1,0,0,
                            1,1,1,1,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_V4_T_opt <- matrix(c(0,0,1,1,0,0,
                            0,0,1,1,0,0,
                            1,1,0,0,0,0,
                            1,1,0,0,0,0,
                            1,1,0,0,0,0,
                            1,1,0,0,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_Zn2_T_opt <- matrix(c(0,1,1,1,
                             1,0,1,1,
                             1,1,0,1,
                             1,1,1,0),nrow = 4,ncol = 4,byrow = TRUE)
#Matriz de incidencia - Complejo con geometria octaedrico
matriz_Co2_O_inc <- matrix(c(0,0,1,0,0,1,0,0,1,0,0,0,
                             0,0,1,0,1,0,0,0,0,1,0,0,
                             0,0,0,1,0,0,0,1,0,0,1,0,
                             0,0,0,1,0,0,1,0,0,0,0,1,
                             1,0,0,0,0,0,0,0,1,0,1,0,
                             0,1,0,0,0,0,0,0,0,1,0,1,
                             1,0,0,0,1,0,1,0,0,0,0,0,
                             0,1,0,0,0,1,0,1,0,0,0,0),nrow = 8,ncol = 12,byrow = TRUE)
matriz_Cr2_O_inc <- matrix(c(1,1,0,0,1,0,1,0,
                             0,0,1,1,1,0,1,0,
                             1,1,0,0,0,1,0,1,
                             0,0,1,1,0,1,0,1),nrow = 4,ncol = 8,byrow = TRUE)
matriz_Cr3_O_inc <- matrix(c(0,0,0,1,1,0,1,0,0,0,0,0,
                             0,0,1,0,0,1,0,1,0,0,0,0,
                             1,0,0,0,0,0,0,1,0,1,0,0,
                             0,1,0,0,0,0,1,0,1,0,0,0,
                             0,0,1,0,0,0,0,0,1,0,1,0,
                             0,0,0,1,0,0,0,0,0,1,0,1,
                             1,0,0,0,1,0,0,0,0,0,1,0,
                             0,1,0,0,0,1,0,0,0,0,0,1),nrow = 8,ncol = 12,byrow = TRUE)
matriz_Cu2_O_inc  <- matrix(c(0,0,1,1,1,1,0,0,
                              1,1,0,0,0,0,1,1,
                              1,1,0,0,0,0,1,1,
                              0,0,1,1,0,0,1,1),nrow = 4,ncol = 8,byrow = TRUE)
matriz_Fe2_O_inc <- matrix(c(0,0,1,1,1,1,0,0,
                             1,1,0,0,0,0,1,1,
                             1,0,0,0,1,0,0,0,
                             0,1,0,0,0,1,0,0,
                             0,0,1,0,0,0,1,0,
                             0,0,0,1,0,0,0,1),nrow = 6,ncol = 8,byrow = TRUE)
matriz_Fe3_O_inc  <- matrix(c(1,0,1,0,1,0,0,0,0,0,1,0,
                              0,1,0,1,0,1,0,0,0,0,0,1,
                              0,0,0,0,0,1,0,1,0,1,1,0,
                              0,0,0,0,1,0,1,0,1,0,0,1,
                              1,0,0,1,0,0,1,0,0,1,0,0,
                              0,1,1,0,0,0,0,1,1,0,0,0),nrow = 6,ncol = 12,byrow = TRUE)
matriz_Mn2_O_inc <- matrix(c(1,1,0,0,1,1,0,0,0,0,0,0,
                             0,0,1,1,0,0,1,1,0,0,0,0,
                             1,0,1,0,0,0,0,0,1,0,1,0,
                             0,0,0,0,0,1,0,1,0,1,0,1,
                             0,1,0,1,0,0,0,0,0,1,1,0,
                             0,0,0,0,1,0,1,0,1,0,0,1),nrow = 6,ncol = 12,byrow = TRUE)
matriz_Mn3_O_inc <- matrix(c(0,0,0,0,1,1,0,0,
                             1,1,0,0,0,0,0,0,
                             0,0,1,1,0,0,0,0,
                             0,0,0,0,0,0,1,1,
                             1,0,1,0,1,0,1,0,
                             0,1,0,1,0,1,0,1),nrow = 6,ncol = 8,byrow = TRUE)
matriz_Mn4_O_inc <- matrix(c(0,1,0,0,0,0,1,0,0,0,0,1,
                             1,0,0,0,0,0,0,1,0,0,1,0,
                             0,0,1,0,1,0,1,0,0,0,0,0,
                             0,0,0,1,0,1,0,1,0,0,0,0,
                             1,0,1,0,0,0,0,0,0,1,0,0,
                             1,0,0,1,0,0,0,0,1,0,0,0,
                             0,0,0,0,1,0,0,0,1,0,1,0,
                             0,0,0,0,0,1,0,0,0,1,0,1),nrow = 8,ncol = 12,byrow = TRUE)
matriz_Ni2_O_inc  <- matrix(c(0,0,0,0,0,0,0,1,0,1,0,1,
                              1,0,0,0,1,0,1,0,0,0,0,0,
                              0,1,0,0,0,1,0,1,0,0,0,0,
                              0,0,0,0,0,0,1,0,1,0,1,0,
                              0,0,1,0,1,0,0,0,0,0,0,1,
                              0,0,0,1,0,1,0,0,0,0,1,0,
                              1,0,0,1,0,0,0,0,0,1,0,0,
                              0,1,0,0,0,0,0,0,1,1,0,0),nrow = 8,ncol = 12,byrow = TRUE)
matriz_Ni3_O_inc <- matrix(c(1,0,0,0,0,1,0,0,0,1,0,1,
                             0,1,0,0,1,0,1,0,0,0,1,0,
                             0,0,1,0,0,0,1,0,1,1,0,0,
                             0,0,0,1,0,1,0,1,0,0,1,0,
                             1,0,1,0,1,0,0,1,0,0,0,0,
                             0,1,0,1,0,0,0,0,1,0,0,1),nrow = 6,ncol = 12,byrow = TRUE)
matriz_Sc2_O_inc <- matrix(c(0,0,0,0,0,0,1,0,1,0,1,0,
                             0,0,0,0,0,0,0,1,0,1,0,1,
                             0,0,1,1,0,0,1,0,0,0,0,0,
                             1,0,1,0,0,0,0,0,0,1,0,0,
                             1,0,0,0,1,0,0,0,1,0,0,0,
                             1,0,1,0,0,0,0,0,0,1,0,0,
                             0,1,0,0,0,1,0,0,0,0,1,0,
                             0,1,0,1,0,0,0,0,0,0,0,1),nrow = 8,ncol = 12,byrow = TRUE)
matriz_Sc3_O_inc <- matrix(c(1,0,0,0,0,1,1,0,0,0,0,1,
                             0,1,0,0,1,0,0,1,0,0,1,0,
                             1,0,1,0,0,0,0,0,1,0,1,0,
                             0,1,0,1,0,0,0,0,0,1,0,1,
                             0,0,1,0,1,0,1,0,0,1,0,0,
                             0,0,0,1,0,1,0,1,1,0,0,0),nrow = 6,ncol = 12,byrow = TRUE)
matriz_Ti2_O_inc <- matrix(c(0,0,1,0,1,0,1,0,1,0,0,0,
                             0,0,0,1,0,1,0,1,0,1,0,0,
                             1,0,0,0,0,0,1,0,0,1,1,0,
                             0,1,1,0,0,1,0,0,0,0,1,0,
                             1,0,0,1,1,0,0,0,0,0,0,1,
                             0,1,0,0,0,0,0,1,1,0,0,1),nrow = 6,ncol = 12,byrow = TRUE)
matriz_Ti3_O_inc <- matrix(c(0,0,1,0,0,1,0,0,0,0,1,0,
                             0,0,0,1,1,0,0,0,0,0,0,1,
                             0,0,1,0,0,0,1,1,0,0,0,0,
                             0,0,0,1,0,0,0,0,1,1,0,0,
                             1,0,0,0,0,0,0,0,1,0,1,0,
                             0,1,0,0,0,0,0,1,0,0,0,1,
                             1,0,0,0,1,0,1,0,0,0,0,0,
                             0,1,0,0,0,1,0,0,0,1,0,0),nrow = 8,ncol = 12,byrow = TRUE)
matriz_V2_O_inc <- matrix(c(1,0,0,0,0,0,1,0,0,0,0,1,
                            0,1,0,0,0,0,0,1,0,0,1,0,
                            0,0,1,0,1,0,0,0,0,0,1,0,
                            0,0,0,1,0,1,0,0,0,0,0,1,
                            0,0,1,0,0,0,1,0,1,0,0,0,
                            0,0,0,1,0,0,0,1,0,1,0,0,
                            0,1,0,0,0,1,0,0,1,0,0,0,
                            1,0,0,0,1,0,0,0,0,1,0,0),nrow = 8,ncol = 12,byrow = TRUE)
matriz_V3_O_inc <- matrix(c(0,0,1,0,0,0,1,0,0,1,1,0,
                            0,0,0,1,0,0,0,1,1,0,0,1,
                            1,0,1,0,0,1,0,1,0,0,0,0,
                            0,1,0,1,1,0,1,0,0,0,0,0,
                            1,0,0,0,1,0,0,0,1,0,1,0,
                            0,1,0,0,0,1,0,0,0,1,0,1),nrow = 6,ncol = 12,byrow = TRUE)
matriz_V4_O_inc <- matrix(c(0,1,0,0,0,1,0,0,0,0,0,1,
                            0,0,0,0,1,0,1,0,0,0,1,0,
                            0,1,1,0,0,0,0,1,0,0,0,0,
                            0,0,0,1,1,0,0,0,1,0,0,0,
                            1,0,0,0,0,0,0,1,0,0,1,0,
                            0,0,0,0,0,0,0,0,1,1,0,1,
                            1,0,0,1,0,1,0,0,0,0,0,0,
                            0,0,1,0,0,0,1,0,0,1,0,0),nrow = 8,ncol = 12,byrow = TRUE)
matriz_Zn2_O_inc  <- matrix(c(0,0,0,1,1,0,1,0,0,0,0,1,
                              0,0,1,0,0,1,0,1,0,0,1,0,
                              1,0,1,0,1,0,0,0,0,1,0,0,
                              1,0,0,0,0,0,1,0,1,0,1,0,
                              0,1,0,0,0,0,0,1,0,1,0,1,
                              0,1,0,1,0,1,0,0,1,0,0,0),nrow = 6,ncol = 12,byrow = TRUE)
#Matriz de incidencia de complejo con geometria cuadrada plana sin optimizar
matriz_Co2_CP_inc <- matrix(c(0,0,1,0,0,1,0,0,1,0,0,0,
                              0,0,0,1,0,0,1,0,0,1,0,0,
                              0,0,1,0,1,0,0,0,0,0,1,0,
                              0,0,0,1,0,0,0,1,0,0,0,1,
                              0,1,0,0,1,0,1,0,0,0,0,0,
                              1,0,0,0,0,1,0,1,0,0,0,0,
                              1,0,0,0,0,0,0,0,0,1,1,0,
                              0,1,0,0,0,0,0,0,1,0,0,1),nrow = 8,ncol = 12,byrow = TRUE)
matriz_Cr3_CP_inc <- matrix(c(0,0,1,0,0,0,1,0,0,1,
                              0,1,0,0,1,1,0,0,0,1,
                              0,0,0,0,0,1,0,1,1,0,
                              1,0,1,1,0,0,0,0,1,0,
                              1,0,0,0,1,0,0,1,0,0,
                              0,1,0,1,0,0,1,0,0,0),nrow = 6,ncol = 10,byrow = TRUE)
matriz_Cu2_CP_inc <- matrix(c(1,0,0,0,1,0,1,1,
                              1,0,0,1,0,1,0,1,
                              0,1,1,1,0,1,0,0,
                              0,1,1,0,1,0,1,0),nrow = 4,ncol = 8,byrow = TRUE)
matriz_Fe3_CP_inc <- matrix(c(0,0,1,0,0,0,1,0,0,1,
                              1,0,1,0,1,0,0,1,0,0,
                              0,1,0,1,0,1,0,0,0,1,
                              0,1,0,0,0,0,1,1,0,0,
                              1,0,0,1,0,0,0,0,1,0,
                              0,0,0,0,1,1,0,0,1,0),nrow = 6,ncol = 10,byrow = TRUE)
matriz_Mn2_CP_inc <- matrix(c(1,0,1,0,1,0,1,1,
                              0,1,0,1,0,1,1,1,
                              1,0,0,0,1,1,0,0,
                              0,1,1,1,0,0,0,0),nrow = 4,ncol = 8,byrow = TRUE)
matriz_Mn4_CP_inc <- matrix(c(0,0,0,0,0,0,1,1,
                              0,1,0,1,1,0,1,0,
                              1,0,1,0,0,1,0,1,
                              0,0,1,0,1,0,0,0,
                              0,0,0,1,0,1,0,0,
                              1,1,0,0,0,0,0,0),nrow = 6,ncol = 8,byrow = TRUE)
matriz_Ni2_CP_inc <- matrix(c(0,1,1,1,1,0,0,0,
                              1,0,0,1,1,0,1,0,
                              1,0,0,0,0,1,1,1,
                              0,1,1,0,0,1,0,1),nrow = 4,ncol = 8,byrow = TRUE)
matriz_Ni3_CP_inc <- matrix(c(0,0,1,0,0,1,0,1,0,1,
                              1,1,0,0,0,0,1,0,1,0,
                              1,0,0,0,1,0,0,0,0,1,
                              0,0,0,1,0,1,1,0,0,0,
                              0,1,0,0,1,0,0,1,0,0,
                              0,0,1,1,0,0,0,0,1,0),nrow = 6,ncol = 10,byrow = TRUE)
matriz_Sc2_CP_inc <- matrix(c(0,0,1,0,0,1,0,0,1,0,1,0,
                              0,1,1,0,1,0,0,0,0,0,0,1,
                              1,0,0,1,0,0,1,0,0,0,1,0,
                              0,0,0,1,0,0,0,1,0,1,0,1,
                              0,1,0,0,0,1,1,1,0,0,0,0,
                              1,0,0,0,1,0,0,0,1,1,0,0),nrow = 6,ncol = 12,byrow = TRUE)
matriz_Sc3_CP_inc  <- matrix(c(0,0,1,0,0,0,1,1,0,1,
                               0,0,0,1,1,1,0,0,1,0,
                               0,1,1,0,0,0,0,0,1,0,
                               1,0,0,0,1,0,0,1,0,0,
                               1,1,0,1,0,0,1,0,0,0,
                               0,0,0,0,0,1,0,0,0,1),nrow = 6,ncol = 10,byrow = TRUE)
matriz_Ti3_CP_inc <- matrix(c(0,1,0,0,0,0,0,1,0,0,1,
                              1,0,0,0,0,1,0,1,0,0,1,
                              0,1,0,1,0,0,1,0,0,1,0,
                              1,0,1,0,1,0,0,0,0,1,0,
                              0,0,1,1,0,0,0,1,1,0,0,
                              0,0,0,0,1,1,1,0,0,0,0),nrow = 6,ncol = 11,byrow = TRUE)
matriz_V2_CP_inc  <- matrix(c(0,0,0,0,0,1,1,0,
                              0,0,1,0,0,0,0,1,
                              1,0,0,1,0,0,0,1,
                              0,1,1,1,0,0,0,0,
                              1,0,0,0,1,1,0,0,
                              0,1,0,0,1,0,1,0),nrow = 6,ncol = 8,byrow = TRUE)
matriz_V4_CP_inc <- matrix(c(1,1,1,0,0,1,0,1,0,
                             0,1,0,1,1,0,0,0,1,
                             1,0,0,0,0,0,1,0,1,
                             0,0,1,0,1,0,1,0,0,
                             0,0,0,1,0,1,0,1,0),nrow = 5,ncol = 9,byrow = TRUE)
matriz_Zn2_CP_inc <- matrix(c(1,1,1,0,0,0,1,
                              1,1,0,1,1,0,0,
                              0,0,0,1,1,1,0,
                              0,0,1,0,0,1,1),nrow = 4,ncol = 7,byrow = TRUE)
#Matriz de incidencia de complejos con geometria tetraedrica sin optimizar.
matriz_Cr2_T_inc  <- matrix(c(1,0,1,0,0,0,0,0,0,1,0,1,
                              0,0,1,1,0,0,1,0,1,0,0,0,
                              0,0,0,0,1,1,0,0,0,1,1,0,
                              0,1,0,0,0,1,1,1,0,0,0,0,
                              1,0,0,1,0,0,0,1,0,0,1,0,
                              0,1,0,0,1,0,0,0,1,0,0,1),nrow = 6,ncol = 12,byrow = TRUE)
matriz_Co2_T_inc <- matrix(c(0,0,0,1,0,0,1,0,1,1,0,0,
                             0,0,1,0,1,0,0,1,0,0,0,1,
                             0,1,0,0,1,0,1,0,0,0,1,0,
                             1,0,0,1,0,1,0,0,0,0,0,1,
                             1,0,0,0,0,0,0,1,0,1,1,0,
                             0,1,1,0,0,1,0,0,1,0,0,0),nrow = 6,ncol = 12,byrow = TRUE)
matriz_Cr3_T_inc  <- matrix(c(0,0,0,0,0,1,1,0,1,0,0,1,
                              0,0,1,0,1,0,0,0,0,1,1,0,
                              0,1,0,0,1,0,1,0,0,0,1,0,
                              1,0,0,1,0,1,0,0,0,0,0,1,
                              1,0,0,0,0,0,0,1,0,1,1,0,
                              0,1,1,0,0,1,0,0,1,0,0,0),nrow = 6,ncol = 12,byrow = TRUE)
matriz_Cu2_T_inc <- matrix(c(1,0,1,1,1,0,0,
                             1,0,0,1,0,1,1,
                             0,1,1,0,1,0,0,
                             0,1,0,0,0,1,1),nrow = 4,ncol = 7,byrow = TRUE)
matriz_Fe3_T_inc <- matrix(c(0,1,0,1,0,0,0,1,0,1,
                             0,0,0,0,1,1,0,0,0,1,
                             1,1,0,0,0,0,0,0,1,0,
                             0,0,1,0,0,1,1,0,1,0,
                             0,0,1,0,1,0,0,1,0,0,
                             1,0,0,1,0,0,1,0,0,0),nrow = 6,ncol = 10,byrow = TRUE)
matriz_Mn2_T_inc <- matrix(c(1,1,0,1,0,
                             1,1,0,0,1,
                             0,0,1,1,0,
                             0,0,1,0,1),nrow = 4,ncol = 5,byrow = TRUE)
matriz_Mn3_T_inc  <- matrix(c(0,0,0,0,1,1,1,1,0,0,0,0,
                              0,0,0,0,0,0,0,0,1,1,1,1,
                              1,1,0,0,1,0,0,0,1,0,0,0,
                              0,0,1,1,0,1,0,0,0,0,0,1,
                              0,1,0,1,0,0,1,0,0,1,0,0,
                              1,0,1,0,0,0,0,1,0,0,1,0),nrow = 6,ncol = 12,byrow = TRUE)
matriz_Mn4_T_inc <- matrix(c(0,0,0,0,0,0,1,1,1,1,0,0,
                             0,0,0,0,1,1,0,0,0,0,1,1,
                             1,0,1,0,1,0,1,0,0,0,0,0,
                             0,1,0,1,0,1,0,1,0,0,0,0,
                             1,0,0,1,0,0,0,0,1,0,1,0,
                             0,1,1,0,0,0,0,0,0,1,0,1),nrow = 6,ncol = 12,byrow = TRUE)
matriz_Ni3_T_inc  <- matrix(c(0,0,0,0,1,0,1,1,0,1,0,
                              0,0,0,0,0,1,0,0,1,0,1,
                              1,0,1,0,0,0,1,0,0,0,1,
                              0,1,1,0,1,0,0,0,1,0,0,
                              1,0,0,1,0,0,0,0,0,1,0,
                              0,1,0,1,0,1,0,1,0,0,0),nrow = 6,ncol = 11,byrow = TRUE)
matriz_Sc2_T_inc <- matrix(c(0,1,1,0,1,1,
                             1,0,0,1,1,1,
                             1,1,0,0,0,0,
                             0,0,1,1,0,0),nrow = 4,ncol = 6,byrow = TRUE)
matriz_Sc3_T_inc <- matrix(c(1,0,1,0,0,1,0,
                             0,1,0,0,1,1,1,
                             0,1,1,1,0,0,1,
                             1,0,0,1,1,0,0),nrow = 4,ncol = 7,byrow = TRUE)
matriz_Ti3_T_inc  <- matrix(c(0,0,0,1,0,1,0,0,
                              1,0,0,0,1,0,0,0,
                              1,1,0,0,0,0,1,0,
                              0,1,0,0,0,1,0,1,
                              0,0,1,0,1,0,0,1,
                              0,0,1,1,0,0,1,0),nrow = 6,ncol = 8,byrow = TRUE)
matriz_V4_T_inc <- matrix(c(0,0,1,0,0,1,1,0,
                            1,1,0,0,0,0,0,1,
                            0,1,0,1,0,0,1,0,
                            0,0,0,1,1,0,0,0,
                            1,0,1,0,0,0,0,0,
                            0,0,0,0,1,1,0,1),nrow = 6,ncol = 8,byrow = TRUE)
matriz_Zn2_T_inc <- matrix(c(1,0,0,1,1,0,
                             1,0,1,0,0,1,
                             0,1,0,1,0,1,
                             0,1,1,0,1,0),nrow = 4,ncol = 6,byrow = TRUE)
#Matriz incidencia de complejos con geometria cuadrada plana con geometria optimizada.
matriz_Co2_CP_inc_opt <- matrix(c(1,0,0,0,1,0,0,0,1,0,0,0,
                                  1,0,0,0,0,1,0,0,0,1,0,0,
                                  0,1,0,0,1,0,0,0,0,0,1,0,
                                  0,1,0,0,0,1,0,0,0,0,0,1,
                                  0,0,1,0,0,0,1,0,1,0,0,0,
                                  0,0,1,0,0,0,0,1,0,1,0,0,
                                  0,0,0,1,0,0,1,0,0,0,1,0,
                                  0,0,0,1,0,0,0,1,0,0,0,1),nrow = 8,ncol = 12,byrow = TRUE)
matriz_Cr2_CP_inc_opt <- matrix(c(0,0,0,0,1,1,0,0,1,1,0,0,
                                  0,0,0,0,0,0,1,1,0,0,1,1,
                                  1,0,1,0,1,0,0,0,0,0,1,0,
                                  0,1,1,0,0,0,1,0,1,0,0,0,
                                  1,0,0,1,0,1,0,0,0,0,0,1,
                                  0,1,0,1,0,0,0,1,0,1,0,0),nrow = 6,ncol = 12,byrow = TRUE)
matriz_Cr3_CP_inc_opt <- matrix(c(1,0,0,0,1,0,1,0,0,0,0,0,
                                  0,1,0,0,0,1,0,1,0,0,0,0,
                                  0,0,1,0,0,1,0,0,1,0,0,0,
                                  0,0,1,0,1,0,0,0,0,1,0,0,
                                  0,0,0,1,0,0,0,1,0,0,1,0,
                                  0,0,0,1,0,0,1,0,0,0,0,1,
                                  1,0,0,0,0,0,0,0,1,0,1,0,
                                  0,1,0,0,0,0,0,0,0,1,0,1),nrow = 8,ncol = 12,byrow = TRUE)
matriz_Cu2_CP_inc_opt <- matrix(c(1,1,0,1,0,0,0,1,
                                  1,0,1,0,1,0,0,1,
                                  0,1,0,1,0,1,1,0,
                                  0,0,1,0,1,1,1,0),nrow = 4,ncol = 8,byrow = TRUE)
matriz_Fe2_CP_inc_opt <- matrix(c(1,0,0,0,1,0,0,0,1,0,0,0,
                                  1,0,0,0,0,1,0,0,0,1,0,0,
                                  0,0,0,1,0,0,1,0,0,0,1,0,
                                  0,0,0,1,0,0,0,1,0,0,0,1,
                                  0,1,0,0,1,0,1,0,0,0,0,0,
                                  0,1,0,0,0,1,0,1,0,0,0,0,
                                  0,0,1,0,0,0,0,0,1,0,1,0,
                                  0,0,1,0,0,0,0,0,0,1,0,1),nrow = 8,ncol = 12,byrow = TRUE)
matriz_Fe3_CP_inc_opt <- matrix(c(0,1,1,0,1,0,1,0,
                                  0,1,1,0,0,1,0,1,
                                  1,0,0,1,1,0,1,0,
                                  1,0,0,1,0,1,0,1),nrow = 4,ncol = 8,byrow = TRUE)
matriz_Mn2_CP_inc_opt <- matrix(c(1,0,1,0,1,0,
                                  1,0,0,1,0,1,
                                  0,1,1,0,1,0,
                                  0,1,0,1,0,1),nrow = 4,ncol = 6,byrow = TRUE)
matriz_Mn3_CP_inc_opt <- matrix(c(0,0,0,0,1,1,0,0,1,1,0,0,
                                  0,0,1,1,0,0,0,0,0,0,1,1,
                                  1,0,1,0,0,0,1,0,1,0,0,0,
                                  0,1,0,1,0,0,1,0,0,1,0,0,
                                  1,0,0,0,1,0,0,1,0,0,1,0,
                                  0,1,0,0,0,1,0,1,0,0,0,1),nrow = 6,ncol = 12,byrow = TRUE)
matriz_Mn4_CP_inc_opt <- matrix(c(1,0,0,0,1,0,0,0,1,0,0,0,
                                  1,0,0,0,0,1,0,0,0,1,0,0,
                                  0,1,0,0,0,0,1,0,0,0,1,0,
                                  0,1,0,0,0,0,0,1,0,0,0,1,
                                  0,0,1,0,1,0,1,0,0,0,0,0,
                                  0,0,1,0,0,1,0,1,0,0,0,0,
                                  0,0,0,1,0,0,0,0,1,0,1,0,
                                  0,0,0,1,0,0,0,0,0,1,0,1),nrow = 8,ncol = 12,byrow = TRUE)
matriz_Ni2_CP_inc_opt <- matrix(c(1,0,0,0,1,0,0,0,0,0,0,1,
                                  0,0,0,1,0,0,0,1,0,0,1,0,
                                  0,1,0,0,1,0,0,0,0,1,0,0,
                                  0,1,0,0,0,1,0,0,0,0,1,0,
                                  0,0,1,0,0,0,1,0,0,0,0,1,
                                  0,0,1,0,0,0,0,1,1,0,0,0,
                                  1,0,0,0,0,1,0,0,1,0,0,0,
                                  0,0,0,1,0,0,1,0,0,1,0,0),nrow = 8,ncol = 12,byrow = TRUE)
matriz_Ni3_CP_inc_opt <- matrix(c(1,0,0,0,0,0,0,0,1,0,1,0,
                                  0,1,0,0,0,0,0,0,0,1,0,1,
                                  0,0,1,0,0,1,0,0,1,0,0,0,
                                  0,0,1,0,1,0,0,0,0,1,0,0,
                                  0,0,0,1,0,0,0,1,0,0,1,0,
                                  0,0,0,1,0,0,1,0,0,0,0,1,
                                  1,0,0,0,1,0,1,0,0,0,0,0,
                                  0,1,0,0,0,1,0,1,0,1,0,0),nrow = 8,ncol = 12,byrow = TRUE)
matriz_Sc2_CP_inc_opt <- matrix(c(0,0,1,1,0,0,1,1,0,0,
                                  0,0,0,0,1,1,0,0,1,1,
                                  1,0,1,0,1,0,0,0,0,0,
                                  1,0,0,1,0,1,0,0,0,0,
                                  0,1,0,0,0,0,1,0,1,0,
                                  0,1,0,0,0,0,0,1,0,1),nrow = 6,ncol = 10,byrow = TRUE)
matriz_Sc3_CP_inc_opt <- matrix(c(0,0,0,1,0,1,0,1,0,1,
                                  0,0,1,0,1,0,1,0,1,0,
                                  1,0,1,1,0,0,0,0,0,0,
                                  1,0,0,0,1,1,0,0,0,0,
                                  0,1,0,0,0,0,1,1,0,0,
                                  0,1,0,0,0,0,0,0,1,1),nrow = 6,ncol = 10,byrow = TRUE)
matriz_Ti2_CP_inc_opt <- matrix(c(1,0,0,0,1,0,0,0,0,1,0,1,
                                  0,1,0,0,0,0,0,1,1,0,1,0,
                                  0,0,0,0,1,1,1,1,0,0,0,0,
                                  1,1,1,1,0,0,0,0,0,0,0,0,
                                  0,0,0,1,0,1,0,0,0,1,1,0,
                                  0,0,1,0,0,0,1,0,1,0,0,1),nrow = 6,ncol = 12,byrow = TRUE)
matriz_Ti3_CP_inc_opt <- matrix(c(0,0,0,0,1,0,1,0,1,0,1,0,
                                  0,0,0,0,0,1,0,1,0,1,0,1,
                                  1,0,1,0,0,0,0,0,1,1,0,0,
                                  0,1,0,1,0,0,0,0,0,0,1,1,
                                  1,1,0,0,1,1,0,0,0,0,0,0,
                                  0,0,1,1,0,0,1,1,0,0,0,0),nrow = 6,ncol = 12,byrow = TRUE)
matriz_V2_CP_inc_opt <- matrix(c(1,0,0,0,1,0,0,0,1,0,0,0,
                                 1,0,0,0,0,1,0,0,0,1,0,0,
                                 0,1,0,0,0,0,1,0,0,0,1,0,
                                 0,1,0,0,0,0,0,1,0,0,0,1,
                                 0,0,1,0,1,0,1,0,0,0,0,0,
                                 0,0,1,0,0,1,0,1,0,0,0,0,
                                 0,0,0,1,0,0,0,0,1,0,1,0,
                                 0,0,0,1,0,0,0,0,0,1,0,1),nrow = 8,ncol = 12,byrow = TRUE)
matriz_V3_CP_inc_opt <- matrix(c(0,0,0,0,1,0,1,0,1,0,1,0,
                                 0,0,0,0,0,1,0,1,0,1,0,1,
                                 1,0,1,0,0,0,0,0,1,1,0,0,
                                 0,1,0,1,0,0,0,0,0,0,1,1,
                                 1,1,0,0,1,1,0,0,0,0,0,0,
                                 0,0,1,1,0,0,1,1,0,0,0,0),nrow = 6,ncol = 12,byrow = TRUE)
matriz_V4_CP_inc_opt <- matrix(c(0,1,0,0,0,0,1,0,1,0,0,1,
                                 1,0,0,0,0,1,0,0,0,1,1,0,
                                 0,0,1,0,1,0,0,0,1,0,1,0,
                                 0,0,0,1,0,0,0,1,0,1,0,1,
                                 1,1,1,1,0,0,0,0,0,0,0,0,
                                 0,0,0,0,1,1,1,1,0,0,0,0),nrow = 6,ncol = 12,byrow = TRUE)
matriz_Zn2_CP_inc_opt <- matrix(c(1,0,1,0,1,0,1,0,
                                  0,1,0,1,0,1,0,1,
                                  1,1,0,0,1,1,0,0,
                                  0,0,1,1,0,0,1,1),nrow = 4,ncol = 8,byrow = TRUE)
#Matriz de incidencia tetraedrica optimizados
matriz_Co2_T_inc_opt <- matrix(c(1,0,0,1,0,1,0,0,0,0,1,0,
                                 1,0,1,0,0,0,0,1,0,0,0,1,
                                 0,1,0,0,0,0,0,0,1,1,1,0,
                                 0,1,0,0,1,0,1,0,0,0,0,1,
                                 0,0,0,0,1,1,0,1,0,1,0,0,
                                 0,0,1,1,0,0,1,0,1,0,0,0),nrow = 6,ncol = 12,byrow = TRUE)
matriz_Cr2_T_inc_opt <- matrix(c(0,0,0,0,1,1,1,0,0,0,1,0,
                                 0,0,0,0,0,0,0,1,1,1,0,1,
                                 1,0,1,0,1,0,0,1,0,0,0,0,
                                 0,1,1,0,0,1,0,0,1,0,0,0,
                                 1,0,0,1,0,0,0,0,0,1,1,0,
                                 0,1,0,1,0,0,1,0,0,0,0,1),nrow = 6,ncol = 12,byrow = TRUE)
matriz_Cr3_T_inc_opt <- matrix(c(1,1,0,0,0,0,0,0,1,1,0,0,
                                 0,0,1,1,0,0,0,0,0,0,1,1,
                                 0,0,1,0,1,0,1,0,1,0,0,0,
                                 0,0,0,1,0,1,0,1,0,1,0,0,
                                 1,0,0,0,1,0,0,1,0,0,1,0,
                                 0,1,0,0,0,1,1,0,0,0,0,1),nrow = 6,ncol = 12,byrow = TRUE)
matriz_Cu2_T_inc_opt <- matrix(c(0,1,0,0,1,1,0,0,
                                 1,1,0,1,0,0,0,1,
                                 1,0,0,0,1,0,1,0,
                                 0,0,1,1,0,0,0,1,
                                 0,0,1,0,0,1,1,0),nrow = 5,ncol = 8,byrow = TRUE)
matriz_Fe2_T_inc_opt <- matrix(c(1,0,0,0,0,0,1,0,0,1,0,0,
                                 1,0,0,0,0,0,0,1,0,0,1,0,
                                 0,1,0,0,1,0,0,0,0,0,0,1,
                                 0,1,0,0,0,1,0,0,1,0,0,0,
                                 0,0,1,0,0,0,0,1,1,0,0,0,
                                 0,0,0,1,0,0,0,0,0,1,0,1,
                                 0,0,0,1,0,1,0,0,0,0,1,0,
                                 0,0,1,0,1,0,1,0,0,0,0,0),nrow = 8,ncol = 12,byrow = TRUE)
matriz_Fe3_T_inc_opt <- matrix(c(1,0,0,0,1,0,1,0,1,0,
                                 0,1,0,0,0,1,0,1,0,1,
                                 0,1,1,0,0,0,1,0,0,0,
                                 0,0,0,1,1,0,0,1,0,0,
                                 1,0,1,0,0,0,0,0,0,1,
                                 0,0,0,1,0,1,0,0,1,0),nrow = 6,ncol = 10,byrow = TRUE)
matriz_Mn2_T_inc_opt <- matrix(c(0,0,1,0,0,1,0,1,0,0,
                                 0,0,0,1,0,0,1,0,1,0,
                                 0,1,0,0,0,0,1,1,0,1,
                                 1,0,0,0,1,1,0,0,1,0,
                                 1,0,1,0,0,0,0,0,0,1,
                                 0,1,0,1,1,0,0,0,0,0),nrow = 6,ncol = 10,byrow = TRUE)
matriz_Mn3_T_inc_opt <- matrix(c(0,0,0,0,0,0,1,1,1,1,0,0,
                                 0,0,0,0,1,1,0,0,0,0,1,1,
                                 0,1,1,0,1,0,0,0,1,0,0,0,
                                 1,0,0,1,0,1,0,0,0,1,0,0,
                                 1,0,1,0,0,0,1,0,0,0,1,0,
                                 0,1,0,1,0,0,0,1,0,0,0,1),nrow = 6,ncol = 12,byrow = TRUE)
matriz_Mn4_T_inc_opt <- matrix(c(1,0,0,1,0,0,0,0,0,1,1,0,
                                 0,1,1,0,0,0,0,0,1,0,0,1,
                                 0,1,0,0,0,1,1,0,0,1,0,0,
                                 0,0,1,0,1,0,0,1,0,0,1,0,
                                 1,0,0,0,1,0,1,0,1,0,0,0,
                                 0,0,0,1,0,1,0,1,0,0,0,1),nrow = 6,ncol = 12,byrow = TRUE)
matriz_Ni2_T_inc_opt  <- matrix(c(0,0,1,0,0,1,0,1,0,0,0,0,
                                  0,0,0,1,0,0,0,0,1,0,1,0,
                                  0,1,0,0,0,0,0,1,0,0,0,1,
                                  0,1,0,0,0,0,1,0,0,0,1,0,
                                  1,0,0,0,1,0,0,0,1,0,0,0,
                                  1,0,0,0,0,1,0,0,0,1,0,0,
                                  0,0,1,0,1,0,1,0,0,0,0,0,
                                  0,0,0,1,0,0,0,0,0,1,0,1),nrow = 8,ncol = 12,byrow = TRUE)
matriz_Ni3_T_inc_opt <- matrix(c(0,0,0,0,0,0,1,0,0,1,1,0,
                                 1,0,0,0,0,0,0,1,0,0,1,0,
                                 0,1,0,0,0,0,0,0,1,0,0,1,
                                 0,0,0,1,0,0,0,0,1,1,0,0,
                                 0,0,1,0,1,0,0,1,0,0,0,0,
                                 0,0,1,0,0,1,0,0,0,0,0,1,
                                 1,0,0,1,0,1,0,0,0,0,0,0,
                                 0,1,0,0,1,0,1,0,0,0,0,0),nrow = 8,ncol = 12,byrow = TRUE)
matriz_Sc2_T_inc_opt <- matrix(c(0,0,0,0,1,0,1,0,
                                 0,0,0,0,0,1,0,1,
                                 1,0,1,0,0,0,0,0,
                                 0,1,0,1,0,0,0,0,
                                 1,1,0,0,1,1,0,0,
                                 0,0,1,1,0,0,1,1),nrow = 6,ncol = 8,byrow = TRUE)
matriz_Sc3_T_inc_opt <- matrix(c(0,1,0,1,1,0,
                                 1,0,0,1,0,1,
                                 1,0,1,0,1,0,
                                 0,1,1,0,0,1),nrow = 4,ncol = 6,byrow = TRUE)
matriz_Ti2_T_inc_opt <- matrix(c(0,0,0,0,1,1,0,1,1,0,0,0,
                               0,0,0,0,0,0,1,0,0,1,1,1,
                               1,0,0,1,0,0,0,0,1,0,1,0,
                               0,1,1,0,0,0,0,1,0,1,0,0,
                               1,0,1,0,1,0,0,0,0,0,0,1,
                               0,1,0,1,0,1,1,0,0,0,0,0),nrow = 6,ncol = 12,byrow = TRUE)
matriz_Ti3_T_inc_opt <- matrix(c(1,0,1,0,0,0,0,0,1,0,
                                 1,0,0,1,0,0,0,0,0,1,
                                 0,1,0,0,1,0,1,0,0,0,
                                 0,1,0,0,0,1,0,1,0,0,
                                 0,0,0,1,0,0,1,0,0,0,
                                 0,0,1,0,0,0,0,1,0,0,
                                 0,0,0,0,1,0,0,0,1,0,
                                 0,0,0,0,0,1,0,0,0,1),nrow = 8,ncol = 10,byrow = TRUE)
matriz_V2_T_inc_opt <- matrix(c(0,0,0,1,0,1,0,1,0,1,0,0,
                                0,0,0,0,1,0,1,0,1,0,1,0,
                                1,0,0,0,0,0,0,0,0,1,1,1,
                                0,1,1,0,0,0,0,1,1,0,0,0,
                                1,1,0,1,1,0,0,0,0,0,0,0,
                                0,0,1,0,0,1,1,0,0,0,0,1),nrow = 6,ncol = 12,byrow = TRUE)
matriz_V3_T_inc_opt <- matrix(c(0,0,0,0,0,0,1,1,0,1,1,0,
                                0,0,0,0,1,1,0,0,1,0,0,1,
                                0,1,1,0,1,0,0,0,0,1,0,0,
                                1,0,0,1,0,1,0,0,0,0,1,0,
                                1,0,1,0,0,0,1,0,1,0,0,0,
                                0,1,0,1,0,0,0,1,0,0,0,1),nrow = 6,ncol = 12,byrow = TRUE)
matriz_V4_T_inc_opt <- matrix(c(1,1,0,0,1,1,0,0,
                                0,0,1,1,0,0,1,1,
                                1,0,0,0,0,0,1,0,
                                0,1,0,0,0,0,0,1,
                                0,0,0,1,1,0,0,0,
                                0,0,1,0,0,1,0,0),nrow = 6,ncol = 8,byrow = TRUE)
matriz_Zn2_T_inc_opt <- matrix(c(1,0,1,1,0,0,
                                 1,0,0,0,1,1,
                                 0,1,0,1,0,1,
                                 0,1,1,0,1,0),nrow = 4,ncol = 6,byrow = TRUE)

#  LISTAS MATRICES --------------------------------------------------------
#Lista de matrices de graficas
lista_matriz_adyacencia_octaedrico <- list("Co2" = matriz_Co2,"Cr2" = matriz_Cr2,"Cr3" = matriz_Cr3,"Cu2" = matriz_Cu2, "Fe2" = matriz_Fe2, "Fe3" = matriz_Fe3, "Mn2" = matriz_Mn2, "Mn3" = matriz_Mn3, "Mn4" = matriz_Mn4, "Ni2" = matriz_Ni2, "Ni3" = matriz_Ni3, "Sc2" = matriz_Sc2, "Sc3" = matriz_Sc3, "Ti2" = matriz_Ti2, "Ti3" = matriz_Ti3, "V2" = matriz_V2, "V3" = matriz_V3, "V4" = matriz_V4, "Zn2" = matriz_Zn2)
lista_matriz_adyacencia_cuadrado_plano <- list("Co2" = matriz_Co2_CP_opt,"Cr2" = matriz_Cr2_CP_opt,"Cr3" = matriz_Cr3_CP_opt,"Cu2" = matriz_Cu2_CP_opt,"Fe2" = matriz_Fe2_CP_opt,"Fe3" = matriz_Fe3_CP_opt,"Mn2" = matriz_Mn2_CP_opt,"Mn3" = matriz_Mn3_CP_opt,"Mn4" = matriz_Mn4_CP_opt,"Ni2" = matriz_Ni2_CP_opt,"Ni3" = matriz_Ni3_CP_opt,"Sc2" = matriz_Sc2_CP_opt,"Sc3" = matriz_Sc3_CP_opt,"Ti2" = matriz_Ti2_CP_opt,"Ti3" = matriz_Ti3_CP_opt,"V2" = matriz_V2_CP_opt,"V3" = matriz_V3_CP_opt,"V4" = matriz_V4_CP_opt,"Zn2" = matriz_Zn2_CP_opt) 
lista_matriz_adyacencia_tetraedrico <- list("Co2" = matriz_Co2_T_opt,"Cr2" = matriz_Cr2_T_opt,"Cr3" = matriz_Cr3_T_opt,"Cu2" = matriz_Cu2_T_opt,"Fe2" = matriz_Fe2_T_opt,"Fe3" = matriz_Fe3_T_opt,"Mn2" = matriz_Mn2_T_opt,"Mn3" = matriz_Mn3_T_opt,"Mn4" = matriz_Mn4_T_opt,"Ni2" = matriz_Ni2_T_opt,"Ni3" = matriz_Ni3_T_opt,"Sc2" = matriz_Sc2_T_opt,"Sc3" = matriz_Sc3_T_opt,"Ti2" = matriz_Ti2_T_opt,"Ti3" = matriz_Ti3_T_opt,"V2" = matriz_V2_T_opt,"V3" = matriz_V3_T_opt,"V4" = matriz_V4_T_opt,"Zn2" = matriz_Zn2_T_opt)
lista_matriz_incidencia_octaedrico <- list("Co2" = matriz_Co2_O_inc,"Cr2" = matriz_Cr2_O_inc,"Cr3" = matriz_Cr3_O_inc,"Cu2" = matriz_Cu2_O_inc,"Fe2" = matriz_Fe2_O_inc,"Fe3" = matriz_Fe3_O_inc,"Mn2" = matriz_Mn2_O_inc,"Mn3" = matriz_Mn3_O_inc,"Mn4" = matriz_Mn4_O_inc,"Ni2" = matriz_Ni2_O_inc,"Ni3" = matriz_Ni3_O_inc,"Sc2" = matriz_Sc2_O_inc,"Sc3" = matriz_Sc3_O_inc,"Ti2" = matriz_Ti2_O_inc,"Ti3" = matriz_Ti3_O_inc,"V2" = matriz_V2_O_inc,"V3" = matriz_V3_O_inc,"V4" = matriz_V4_O_inc,"Zn2" = matriz_Zn2_O_inc)
lista_matriz_incidencia_cuadrado_plano <- list("Co2" = matriz_Co2_CP_inc_opt,"Cr2" = matriz_Cr2_CP_inc_opt,"Cr3" = matriz_Cr3_CP_inc_opt,"Cu2" = matriz_Cu2_CP_inc_opt,"Fe2" = matriz_Fe2_CP_inc_opt,"Fe3" = matriz_Fe3_CP_inc_opt,"Mn2" = matriz_Mn2_CP_inc_opt,"Mn3" = matriz_Mn3_CP_inc_opt,"Mn4" = matriz_Mn4_CP_inc_opt,"Ni2" = matriz_Ni2_CP_inc_opt,"Ni3" = matriz_Ni3_CP_inc_opt,"Sc2" = matriz_Sc2_CP_inc_opt,"Sc3" = matriz_Sc3_CP_inc_opt,"Ti2" = matriz_Ti2_CP_inc_opt,"Ti3" = matriz_Ti3_CP_inc_opt,"V2" = matriz_V2_CP_inc_opt,"V3" = matriz_V3_CP_inc_opt,"V4" = matriz_V4_CP_inc_opt,"Zn2" = matriz_Zn2_CP_inc_opt)
lista_matriz_incidencia_tetraedrico <- list("Co2" = matriz_Co2_T_inc_opt,"Cr2" = matriz_Cr2_T_inc_opt,"Cr3" = matriz_Cr3_T_inc_opt,"Cu2" = matriz_Cu2_T_inc_opt,"Fe2" = matriz_Fe2_T_inc_opt,"Fe3" = matriz_Fe3_T_inc_opt,"Mn2" = matriz_Mn2_T_inc_opt,"Mn3" = matriz_Mn3_T_inc_opt,"Mn4" = matriz_Mn4_T_inc_opt,"Ni2" = matriz_Ni2_T_inc_opt,"Ni3" = matriz_Ni3_T_inc_opt,"Sc2" = matriz_Sc2_T_inc_opt,"Sc3" = matriz_Sc3_T_inc_opt,"Ti2" = matriz_Ti2_T_inc_opt,"Ti3" = matriz_Ti3_T_inc_opt,"V2" = matriz_V2_T_inc_opt,"V3" = matriz_V3_T_inc_opt,"V4" = matriz_V4_T_inc_opt,"Zn2" = matriz_Zn2_T_inc_opt) 
lista_matrices_Cu_phen  <- list("s0" = matriz_Cu_phen_s0,"s1" = matriz_Cu_phen_s1,"s2" =  matriz_Cu_phen_s2,"s3" = matriz_Cu_phen_s3,"s4" = matriz_Cu_phen_s4,"s5" = matriz_Cu_phen_s5,"s6" = matriz_Cu_phen_s6,"s7" = matriz_Cu_phen_s7,"s8" = matriz_Cu_phen_s8,"s9" = matriz_Cu_phen_s9,"s10" = matriz_Cu_phen_s10,"s11" = matriz_Cu_phen_s11,"s12" = matriz_Cu_phen_s12,"s13" = matriz_Cu_phen_s13,"s14" = matriz_Cu_phen_s14,"s15" = matriz_Cu_phen_s15,"s16" = matriz_Cu_phen_s16,"s17" = matriz_Cu_phen_s17,"s18" = matriz_Cu_phen_s18,"s19" = matriz_Cu_phen_s19,"s20" = matriz_Cu_phen_s20)
lista_matriz_adyacencia_geometrias <- list("Co2_CP" = matriz_Co2_CP_opt,"Co2_T" = matriz_Co2_T_opt,"Co2_O" = matriz_Co2,"Cr2_CP" = matriz_Cr2_CP_opt,"Cr2_T" = matriz_Cr2_T_opt,"Cr2_O" = matriz_Cr2,"Cr3_CP" = matriz_Cr3_CP_opt,"Cr3_T" = matriz_Cr3_T_opt,"Cr3_O" = matriz_Cr3,"Cu2_CP" = matriz_Cu2_CP_opt,"Cu2_T" = matriz_Cu2_T_opt,"Cu2_O" = matriz_Cu2,"Fe2_CP" = matriz_Fe2,"Fe2_T" = matriz_Fe2_T_opt,"Fe2_O" = matriz_Fe2,"Fe3_CP" = matriz_Fe3_CP_opt,"Fe3_T" = matriz_Fe3_T_opt,"Fe3_O" = matriz_Fe3,"Mn2_CP" = matriz_Mn2_CP_opt,"Mn2_T" = matriz_Mn2_T_opt,"Mn2_O" = matriz_Mn2,"Mn3_CP" = matriz_Mn3_CP_opt,"Mn3_T" = matriz_Mn3_T_opt,"Mn3_O" = matriz_Mn3,"Mn4_CP" = matriz_Mn4_CP_opt,"Mn4_T" = matriz_Mn4_T_opt,"Mn4_O" = matriz_Mn4,"Ni2_CP" = matriz_Ni2_CP_opt,"Ni2_T" = matriz_Ni2_T_opt,"Ni2_O" = matriz_Ni2,"Ni3_CP" = matriz_Ni3_CP_opt,"Ni3_T" = matriz_Ni3_T_opt,"Ni3_O" = matriz_Ni3,"Sc2_CP" = matriz_Sc2_CP_opt,"Sc2_T" = matriz_Sc2_T_opt,"Sc2_O" = matriz_Sc2,"Sc3_CP" = matriz_Sc3_CP_opt,"Sc3_T" = matriz_Sc3_T_opt,"Sc3_O" = matriz_Sc3,"Ti2_CP" = matriz_Ti2_CP_opt,"Ti2_T" = matriz_Ti2_T_opt,"Ti2_O" = matriz_Ti2,"Ti3_CP" = matriz_Ti3_CP_opt,"Ti3_T" = matriz_Ti3_T_opt,"Ti3_O" = matriz_Ti3,"Ni2_CP" = matriz_Ni2_CP_opt,"Ni2_T" = matriz_Ni2_T_opt,"Ni2_O" = matriz_Ni2,"Ni3_CP" = matriz_Ni3_CP_opt,"Ni3_T" = matriz_Ni3_T_opt,"Ni3_O" = matriz_Ni3,"V2_CP" = matriz_V2_CP_opt,"V2_T" = matriz_V2_T_opt,"V2_O" = matriz_V2,"V3_CP" = matriz_V3_CP_opt,"V3_T" = matriz_V3_T_opt,"V3_O" = matriz_V3,"V4_CP" = matriz_V4_CP_opt,"V4_T" = matriz_V4_T_opt,"V4_O" = matriz_V4,"Zn2_CP" = matriz_Zn2_CP_opt,"Zn2_T" = matriz_Zn2_T_opt,"Zn2_O" = matriz_Zn2)
lista_matriz_incidencia_geometrias <- list("Co2_CP" = matriz_Co2_CP_inc_opt,"Co2_T" = matriz_Co2_T_inc_opt,"Co2_O" = matriz_Co2_O_inc,"Cr2_CP" = matriz_Cr2_CP_inc_opt,"Cr2_T" = matriz_Cr2_T_inc_opt,"Cr2_O" = matriz_Cr2_O_inc,"Cr3_CP" = matriz_Cr3_CP_inc_opt,"Cr3_T" = matriz_Cr3_T_inc_opt,"Cr3_O" = matriz_Cr3_O_inc,"Cu2_CP" = matriz_Cu2_CP_inc_opt,"Cu2_T" = matriz_Cu2_T_inc_opt,"Cu2_O" = matriz_Cu2_O_inc,"Fe2_CP" = matriz_Fe2_CP_inc_opt,"Fe2_T" = matriz_Fe2_T_inc_opt,"Fe2_O" = matriz_Fe2_O_inc,"Fe3_CP" = matriz_Fe3_CP_inc_opt,"Fe3_T" = matriz_Fe3_T_inc_opt,"Fe3_O" = matriz_Fe3_O_inc,"Mn2_CP" = matriz_Mn2_CP_inc_opt,"Mn2_T" = matriz_Mn2_T_inc_opt,"Mn2_O" = matriz_Mn2_O_inc,"Mn3_CP" = matriz_Mn3_CP_inc_opt,"Mn3_T" = matriz_Mn3_T_inc_opt,"Mn3_O" = matriz_Mn3_O_inc,"Mn4_CP" = matriz_Mn4_CP_inc_opt,"Mn4_T" = matriz_Mn4_T_inc_opt,"Mn4_O" = matriz_Mn4_O_inc,"Sc2_CP" = matriz_Sc2_CP_inc_opt,"Sc2_T" = matriz_Sc2_T_inc_opt,"Sc2_O" = matriz_Sc2_O_inc,"Sc3_CP" = matriz_Sc3_CP_inc_opt,"Sc3_T" = matriz_Sc3_T_inc_opt,"Sc3_O" = matriz_Sc3_O_inc,"Ti2_CP" = matriz_Ti2_CP_inc_opt,"Ti2_T" = matriz_Ti2_T_inc_opt,"Ti2_O" = matriz_Ti2_O_inc,"Ti3_CP" = matriz_Ti3_CP_inc_opt,"Ti3_T" = matriz_Ti3_T_inc_opt,"Ti3_O" = matriz_Ti3_O_inc,"Ni2_CP" = matriz_Ni2_CP_inc_opt,"Ni2_T" = matriz_Ni2_T_inc_opt,"Ni2_O" = matriz_Ni2_O_inc,"Ni3_CP" = matriz_Ni3_CP_inc_opt,"Ni3_T" = matriz_Ni3_T_inc_opt,"Ni3_O" = matriz_Ni3_O_inc,"V2_CP" = matriz_V2_CP_inc_opt,"V2_T" = matriz_V2_T_inc_opt,"V2_O" = matriz_V2_O_inc,"V3_CP" = matriz_V3_CP_inc_opt,"V3_T" = matriz_V3_T_inc_opt,"V3_O" = matriz_V3_O_inc,"V4_CP" = matriz_V4_CP_inc_opt,"V4_T" = matriz_V4_T_inc_opt,"V4_O" = matriz_V4_O_inc,"Zn2_CP" = matriz_Zn2_CP_inc_opt,"Zn2_T" = matriz_Zn2_T_inc_opt,"Zn2_O" = matriz_Zn2_O_inc)
#Lista  de conjunto caracteristico
lista_conjunto_caracteristico_complejos_acuosos <- list("Co2" = "[8(3),12,6]","Cr3" = "[8(3),12,6]","Fe2" = "[6,8,4]","Mn2" = "[6(4),12,8]","Cr2" = "[4(4),8,6]","Cu2" =  "[4(4),8,6]","Fe3" = "[8(3),12,6]","Mn3" = "[6,8,4]","Mn4" = "[8(3),12,6]","Sc2" =  "[6(4),12,8]","Ti3" = "[8(3),12,6]","V4" = "[8(3),12,6]","Ni2" = "[6(4),12,8]","Sc3" = "[8(3),12,6]","V2" = "[8(3),12,6]","Zn2" = "[6(4),12,8]","Ni3" = "[8(3),12,6]","Ti2" = "[6(4),12,8]","V3" = "[6(4),12,8]")
lista_conjunto_caracteristico_Cu_phen <- list("{s0}" = "[4(3),6,4]","{s1,s2,s19}" = "[6,9,5]","{s3}" = "[4(4),8,6]","{s4,s5,s6,s6,s12,s13,s14,s15}" = "[4,6,4]","{s7,s8}" = "[6,8,4]","{s10}" = "[6,8,4]","{s11,s20}" = "[5,9,6]","{s16}" = "[6,7,3]","{s17}" = "[4,8,6]","{s18}" = "[4,6,4]")
lista_conjunto_caracteristico_complejos_CP <- list("Co2" = "[8(3),12,6]","Cr2" = "[6(4),12,8]","Cr3" = "[4,6,4]","Cu2" = "[4(4),8,6]","Fe2" = "[8(3),12,6]","Fe3" = "[4(4),8,6]","Mn2" = "[4,6,4]","Mn3" = "[6(4),12,8]","Mn4" = "[6(4),12,8]","Ni2" = "[4(4),8,6]","Ni3" = "[8(3),12,6]","Sc2" = "[6,10,6]","Sc3" = "[6,10,6]","Ti2" = "[6(4),12,8]","Ti3" = "[6(4),12,8]","V2" = "[6(4),12,8]","V3" = "[6,8,4]","V4" = "[6(4),12,8]","Zn2" = "[4(4),8,6]")
lista_conjunto_caracteristico_complejos_T <- list("Co2" = "[6(4),12,8]","Cr2" = "[6(4),12,8]","Cr3" = "[6(4),12,8]","Cu2" = "[5,8,5]","Fe2" = "[8(3),12,6]","Fe3" = "[4(4),8,6]","Mn2" = "[4,6,4]","Mn3" = "[6(4),12,8]","Mn4" = "[6(4),12,8]","Ni2" = "[4(4),8,6]","Ni3" = "[8(3),12,6]","Sc2" = "[6,10,6]","Sc3" = "[6,10,6]","Ti2" = "[6(4),12,8]","Ti3" = "[6(4),12,8]","V2" = "[6(4),12,8]","V3" = "[6,8,4]","V4" = "[6(4),12,8]","Zn2" = "[4(4),8,6]")

# GRAFICAS IGRAPH ---------------------------------------------------------
#Graficas Complejos acuosos
grafica_Co2 <- graph_from_adjacency_matrix(matriz_Co2, mode = "undirected")
grafica_Cr2 <- graph_from_adjacency_matrix(matriz_Cr2, mode = "undirected")
grafica_Cr3 <- graph_from_adjacency_matrix(matriz_Cr3, mode = "undirected")
grafica_Cu2 <- graph_from_adjacency_matrix(matriz_Cu2, mode = "undirected")
grafica_Fe2 <- graph_from_adjacency_matrix(matriz_Fe2, mode = "undirected")
grafica_Fe3 <- graph_from_adjacency_matrix(matriz_Fe3,mode = "undirected")
grafica_Mn2 <- graph_from_adjacency_matrix(matriz_Mn2,mode = "undirected")
grafica_Mn3 <- graph_from_adjacency_matrix(matriz_Mn3, mode = "undirected")
grafica_Mn4 <- graph_from_adjacency_matrix(matriz_Mn4, mode = "undirected")
grafica_Ni2 <- graph_from_adjacency_matrix(matriz_Ni2, mode = "undirected")
grafica_Ni3 <- graph_from_adjacency_matrix(matriz_Ni3, mode = "undirected")
grafica_Sc2 <- graph_from_adjacency_matrix(matriz_Sc2, mode = "undirected")
grafica_Sc3 <- graph_from_adjacency_matrix(matriz_Sc3, mode = "undirected")
grafica_Ti2 <- graph_from_adjacency_matrix(matriz_Ti2, mode = "undirected")
grafica_Ti3 <- graph_from_adjacency_matrix(matriz_Ti3, mode = "undirected")
grafica_V2 <- graph_from_adjacency_matrix(matriz_V2, mode = "undirected")
grafica_V3 <- graph_from_adjacency_matrix(matriz_V3, mode = "undirected")
grafica_V4 <- graph_from_adjacency_matrix(matriz_V4, mode = "undirected")
grafica_Zn2 <- graph_from_adjacency_matrix(matriz_Zn2, mode = "undirected")
#Graficas Cu-phen
grafica_Cu_phen_s0 <- graph_from_adjacency_matrix(matriz_Cu_phen_s0, mode = "undirected")
grafica_Cu_phen_s1 <- graph_from_adjacency_matrix(matriz_Cu_phen_s1, mode = "undirected")
grafica_Cu_phen_s2 <- graph_from_adjacency_matrix(matriz_Cu_phen_s2, mode = "undirected")
grafica_Cu_phen_s3 <- graph_from_adjacency_matrix(matriz_Cu_phen_s3, mode = "undirected")
grafica_Cu_phen_s4 <- graph_from_adjacency_matrix(matriz_Cu_phen_s4, mode = "undirected")
grafica_Cu_phen_s5 <- graph_from_adjacency_matrix(matriz_Cu_phen_s5, mode = "undirected")
grafica_Cu_phen_s6 <- graph_from_adjacency_matrix(matriz_Cu_phen_s6, mode = "undirected")
grafica_Cu_phen_s7 <- graph_from_adjacency_matrix(matriz_Cu_phen_s7, mode = "undirected")
grafica_Cu_phen_s8 <- graph_from_adjacency_matrix(matriz_Cu_phen_s8, mode = "undirected")
grafica_Cu_phen_s9 <- graph_from_adjacency_matrix(matriz_Cu_phen_s9, mode = "undirected")
grafica_Cu_phen_s10  <- graph_from_adjacency_matrix(matriz_Cu_phen_s10, mode = "undirected")
grafica_Cu_phen_s11  <- graph_from_adjacency_matrix(matriz_Cu_phen_s11, mode = "undirected")
grafica_Cu_phen_s12  <- graph_from_adjacency_matrix(matriz_Cu_phen_s12, mode = "undirected")
grafica_Cu_phen_s13  <- graph_from_adjacency_matrix(matriz_Cu_phen_s13, mode = "undirected")
grafica_Cu_phen_s14  <- graph_from_adjacency_matrix(matriz_Cu_phen_s14, mode = "undirected")
grafica_Cu_phen_s15  <- graph_from_adjacency_matrix(matriz_Cu_phen_s15, mode = "undirected")
grafica_Cu_phen_s16  <- graph_from_adjacency_matrix(matriz_Cu_phen_s16, mode = "undirected")
grafica_Cu_phen_s17  <- graph_from_adjacency_matrix(matriz_Cu_phen_s17, mode = "undirected")
grafica_Cu_phen_s18  <- graph_from_adjacency_matrix(matriz_Cu_phen_s18, mode = "undirected")
grafica_Cu_phen_s19  <- graph_from_adjacency_matrix(matriz_Cu_phen_s19, mode = "undirected")
grafica_Cu_phen_s20  <- graph_from_adjacency_matrix(matriz_Cu_phen_s20, mode = "undirected")
#### LAYOUT CLASE Cu-phen ####
#modificando la configuracion de las graficas
layout_clase1 <-matrix(c(2,3,
                     6,3,
                     4,1,
                     4,2),ncol = 2,nrow = 4,byrow = TRUE)
layout_clase2 <- matrix(c(6,8,
                          6,3,
                          10,1,
                          5,4,
                          2,1,
                          7,4),ncol = 2,nrow = 6,byrow = TRUE)
layout_clase3 <- matrix(c(3,6,
                          6,3,
                          6,6,
                          3,3),ncol = 2,nrow = 4,byrow = TRUE)
layout_clase4 <- matrix(c(3,3,
                          6,6,
                          6,3,
                          3,6),ncol = 2,nrow = 4,byrow = TRUE)
layout_clase5 <- matrix(c(4,6,
                          1,1,
                          5,3,
                          3,3,
                          4,2,
                          7,1),ncol = 2,nrow = 6,byrow = TRUE)
layout_clase6 <- matrix(c(7,1,
                          4,2,
                          4,6,
                          3,3,
                          5,3),ncol = 2,nrow = 5,byrow = TRUE)
layout_clase7 <- matrix(c(5,2,
                          7,4,
                          3,4,
                          5,1),ncol = 2,nrow = 4,byrow = TRUE)
layout_clase8 <- matrix(c(5,2,
                          7,4,
                          3,4,
                          5,1),ncol = 2,nrow = 4,byrow = TRUE)
#Grafica de complejo cuadrado plano sin optimizar
grafica_Co2_CP <- graph_from_adjacency_matrix(matriz_Co2_CP,mode = "undirected")
grafica_Cr3_CP <- graph_from_adjacency_matrix(matriz_Cr3_CP,mode = "undirected")
grafica_Fe3_CP <- graph_from_adjacency_matrix(matriz_Fe3_CP,mode = "undirected")
grafica_Ni2_CP <- graph_from_adjacency_matrix(matriz_Ni2_CP,mode = "undirected")
grafica_Cu2_CP <- graph_from_adjacency_matrix(matriz_Cu2_CP,mode = "undirected")
grafica_Mn2_CP <- graph_from_adjacency_matrix(matriz_Mn2_CP,mode = "undirected")
grafica_Mn4_CP <- graph_from_adjacency_matrix(matriz_Mn4_CP,mode = "undirected")
grafica_Ni3_CP <- graph_from_adjacency_matrix(matriz_Ni3_CP,mode = "undirected")
grafica_Sc2_CP <- graph_from_adjacency_matrix(matriz_Sc2_CP,mode = "undirected")
grafica_Sc3_CP <- graph_from_adjacency_matrix(matriz_Sc3_CP,mode = "undirected")
grafica_Ti3_CP <- graph_from_adjacency_matrix(matriz_Ti3_CP,mode = "undirected")
grafica_V2_CP <- graph_from_adjacency_matrix(matriz_V2_CP,mode = "undirected")
grafica_V4_CP <- graph_from_adjacency_matrix(matriz_V4_CP,mode = "undirected")
grafica_Zn2_CP <- graph_from_adjacency_matrix(matriz_Zn2_CP,mode = "undirected")
#Grafica de complejo tetraedrico sin optimizar
grafica_Co2_T  <- graph_from_adjacency_matrix(matriz_Co2_T,mode = "undirected")
grafica_Cr2_T <- graph_from_adjacency_matrix(matriz_Cr2_T,mode = "undirected")
grafica_Cr3_T <- graph_from_adjacency_matrix(matriz_Cr3_T,mode = "undirected")
grafica_Cu2_T <- graph_from_adjacency_matrix(matriz_Cu2_T,mode = "undirected")
grafica_Fe3_T <- graph_from_adjacency_matrix(matriz_Fe3_T,mode = "undirected")
grafica_Mn2_T <- graph_from_adjacency_matrix(matriz_Mn2_T,mode = "undirected")
grafica_Mn3_T <- graph_from_adjacency_matrix(matriz_Mn3_T,mode = "undirected")
grafica_Mn4_T <- graph_from_adjacency_matrix(matriz_Mn4_T,mode = "undirected")
grafica_Ni3_T <- graph_from_adjacency_matrix(matriz_Ni3_T,mode = "undirected")
grafica_Sc2_T <- graph_from_adjacency_matrix(matriz_Sc2_T,mode = "undirected")
grafica_Sc3_T <- graph_from_adjacency_matrix(matriz_Sc3_T,mode = "undirected")
grafica_Ti3_T <- graph_from_adjacency_matrix(matriz_Ti3_T,mode = "undirected")
grafica_Zn2_T <- graph_from_adjacency_matrix(matriz_Zn2_T,mode = "undirected")
#Grafica de complejo tetraedrico optimizada (matriz adyacencia)
grafica_Co2_T_A <- graph_from_adjacency_matrix(matriz_Co2_T_opt,mode = "undirected")
grafica_Cr2_T_A  <- graph_from_adjacency_matrix(matriz_Cr2_T_opt,mode = "undirected")
grafica_Cr3_T_A <- graph_from_adjacency_matrix(matriz_Cr3_T_opt,mode = "undirected")
grafica_Cu2_T_A <- graph_from_adjacency_matrix(matriz_Cu2_T_opt,mode = "undirected")
grafica_Fe2_T_A <- graph_from_adjacency_matrix(matriz_Fe2_T_opt,mode = "undirected")
grafica_Fe3_T_A <- graph_from_adjacency_matrix(matriz_Fe3_T_opt,mode = "undirected")
grafica_Mn2_T_A <- graph_from_adjacency_matrix(matriz_Mn2_T_opt,mode = "undirected")
grafica_Mn3_T_A <- graph_from_adjacency_matrix(matriz_Mn3_T_opt,mode = "undirected")
grafica_Mn4_T_A <- graph_from_adjacency_matrix(matriz_Mn4_T_opt,mode = "undirected")
grafica_Ni2_T_A <- graph_from_adjacency_matrix(matriz_Ni2_T_opt,mode = "undirected")
grafica_Ni3_T_A <- graph_from_adjacency_matrix(matriz_Ni3_T_opt,mode = "undirected")
grafica_Sc2_T_A <- graph_from_adjacency_matrix(matriz_Sc2_T_opt,mode = "undirected")
grafica_Sc3_T_A <- graph_from_adjacency_matrix(matriz_Sc3_T_opt,mode = "undirected")
grafica_Ti2_T_A <- graph_from_adjacency_matrix(matriz_Ti2_T_opt,mode = "undirected")
grafica_Ti3_T_A <- graph_from_adjacency_matrix(matriz_Ti3_T_opt,mode = "undirected")
grafica_V2_T_A <- graph_from_adjacency_matrix(matriz_V2_T_opt,mode = "undirected")
grafica_V3_T_A <- graph_from_adjacency_matrix(matriz_V3_T_opt,mode = "undirected")
grafica_V4_T_A <- graph_from_adjacency_matrix(matriz_V4_T_opt,mode = "undirected")
grafica_Zn2_T_A <- graph_from_adjacency_matrix(matriz_Zn2_T_opt,mode = "undirected")
#Grafica de complejo cuadrado plano optimizada (matriz adyacencia)
grafica_Co2_CP_A <- graph_from_adjacency_matrix(matriz_Co2_CP_opt,mode = "undirected")
grafica_Cr2_CP_A <- graph_from_adjacency_matrix(matriz_Cr2_CP_opt,mode = "undirected")
grafica_Cr3_CP_A <- graph_from_adjacency_matrix(matriz_Cr3_CP_opt,mode = "undirected")
grafica_Cu2_CP_A <- graph_from_adjacency_matrix(matriz_Cu2_CP_opt,mode = "undirected")
grafica_Fe2_CP_A <- graph_from_adjacency_matrix(matriz_Fe2_CP_opt,mode = "undirected")
grafica_Fe3_CP_A <- graph_from_adjacency_matrix(matriz_Fe3_CP_opt,mode = "undirected")
grafica_Mn2_CP_A <- graph_from_adjacency_matrix(matriz_Mn2_CP_opt,mode = "undirected")
grafica_Mn3_CP_A <- graph_from_adjacency_matrix(matriz_Mn3_CP_opt,mode = "undirected")
grafica_Mn4_CP_A <- graph_from_adjacency_matrix(matriz_Mn4_CP_opt,mode = "undirected")
grafica_Ni2_CP_A <- graph_from_adjacency_matrix(matriz_Ni2_CP_opt,mode = "undirected")
grafica_Ni3_CP_A <- graph_from_adjacency_matrix(matriz_Ni3_CP_opt,mode = "undirected")
grafica_Sc2_CP_A <- graph_from_adjacency_matrix(matriz_Sc2_CP_opt,mode = "undirected")
grafica_Sc3_CP_A <- graph_from_adjacency_matrix(matriz_Sc3_CP_opt,mode = "undirected")
grafica_Ti2_CP_A <- graph_from_adjacency_matrix(matriz_Ti2_CP_opt,mode = "undirected") 
grafica_Ti3_CP_A <- graph_from_adjacency_matrix(matriz_Ti3_CP_opt,mode = "undirected")
grafica_V2_CP_A <- graph_from_adjacency_matrix(matriz_V2_CP_opt,mode = "undirected")
grafica_V3_CP_A <- graph_from_adjacency_matrix(matriz_V3_CP_opt,mode = "undirected")
grafica_V4_CP_A <- graph_from_adjacency_matrix(matriz_V4_CP_opt,mode = "undirected")
grafica_Zn2_CP_A <- graph_from_adjacency_matrix(matriz_Zn2_CP_opt,mode = "undirected")
#Graficas de complejo con geometria cuadrada plana (matriz incidencia)
grafica_Co2_CP_sin_opt <- graph_from_biadjacency_matrix(matriz_Co2_CP_inc)
grafica_Cr3_CP_sin_opt <- graph_from_biadjacency_matrix(matriz_Cr3_CP_inc)
grafica_Cu2_CP_sin_opt <- graph_from_biadjacency_matrix(matriz_Cu2_CP_inc)
grafica_Fe3_CP_sin_opt <- graph_from_biadjacency_matrix(matriz_Fe3_CP_inc)
grafica_Mn2_CP_sin_opt <- graph_from_biadjacency_matrix(matriz_Mn2_CP_inc)
grafica_Mn4_CP_sin_opt <- graph_from_biadjacency_matrix(matriz_Mn4_CP_inc)
grafica_Ni2_CP_sin_opt <- graph_from_biadjacency_matrix(matriz_Ni2_CP_inc)
grafica_Ni3_CP_sin_opt <- graph_from_biadjacency_matrix(matriz_Ni3_CP_inc)
grafica_Sc2_CP_sin_opt <- graph_from_biadjacency_matrix(matriz_Sc2_CP_inc)
grafica_Sc3_CP_sin_opt <- graph_from_biadjacency_matrix(matriz_Sc3_CP_inc)
grafica_Ti3_CP_sin_opt <- graph_from_biadjacency_matrix(matriz_Ti3_CP_inc)
grafica_V2_CP_sin_opt <- graph_from_biadjacency_matrix(matriz_V2_CP_inc)
grafica_V4_CP_sin_opt <- graph_from_biadjacency_matrix(matriz_V4_CP_inc)
grafica_Zn2_CP_sin_opt <- graph_from_biadjacency_matrix(matriz_Zn2_CP_inc)
#Grafica de complejo con geometria tetraedrica (matriz incidencia)
grafica_Co2_T_sin_opt <- graph_from_biadjacency_matrix(matriz_Co2_T_inc) 
grafica_Cr2_T_sin_opt <- graph_from_biadjacency_matrix(matriz_Cr2_T_inc)
grafica_cr3_T_sin_opt <- graph_from_biadjacency_matrix(matriz_Cr3_T_inc)
grafica_Cu2_T_sin_opt <- graph_from_biadjacency_matrix(matriz_Cu2_T_inc)
grafica_Fe3_T_sin_opt <- graph_from_biadjacency_matrix(matriz_Fe3_T_inc)
grafica_Mn2_T_sin_opt <- graph_from_biadjacency_matrix(matriz_Mn2_T_inc)
grafica_Mn3_T_sin_opt <- graph_from_biadjacency_matrix(matriz_Mn3_T_inc)
grafica_Mn4_T_sin_opt <- graph_from_biadjacency_matrix(matriz_Mn4_T_inc)
grafica_Ni3_T_sin_opt <- graph_from_biadjacency_matrix(matriz_Ni3_T_inc)
grafica_Sc2_T_sin_opt <- graph_from_biadjacency_matrix(matriz_Sc2_T_inc)
grafica_Sc3_T_sin_opt <- graph_from_biadjacency_matrix(matriz_Sc3_T_inc)
grafica_Ti3_T_sin_opt <- graph_from_biadjacency_matrix(matriz_Ti3_T_inc)
grafica_V4_T_sin_opt <- graph_from_biadjacency_matrix(matriz_V4_T_inc)
grafica_Zn2_T_sin_opt <- graph_from_biadjacency_matrix(matriz_Zn2_T_inc)
#Grafica de complejo con geometria cuadrada plana optimizado (matriz incidencia)
grafica_Co2_CP_opt <- graph_from_biadjacency_matrix(matriz_Co2_CP_inc_opt)
grafica_Cr2_CP_opt <- graph_from_biadjacency_matrix(matriz_Cr2_CP_inc_opt)
grafica_Cr3_CP_opt <- graph_from_biadjacency_matrix(matriz_Cr3_CP_inc_opt)
grafica_Cu2_CP_opt <- graph_from_biadjacency_matrix(matriz_Cu2_CP_inc_opt)
grafica_Fe2_CP_opt <- graph_from_biadjacency_matrix(matriz_Fe2_CP_inc_opt)
grafica_Fe3_CP_opt <- graph_from_biadjacency_matrix(matriz_Fe3_CP_inc_opt)
grafica_Mn2_CP_opt <- graph_from_biadjacency_matrix(matriz_Mn2_CP_inc_opt)
grafica_Mn3_CP_opt <- graph_from_biadjacency_matrix(matriz_Mn3_CP_inc_opt)
grafica_Mn4_CP_opt <- graph_from_biadjacency_matrix(matriz_Mn4_CP_inc_opt)
grafica_Ni2_CP_opt <- graph_from_biadjacency_matrix(matriz_Ni2_CP_inc_opt)
grafica_Ni3_CP_opt <- graph_from_biadjacency_matrix(matriz_Ni3_CP_inc_opt)
grafica_Sc2_CP_opt <- graph_from_biadjacency_matrix(matriz_Sc2_CP_inc_opt)
grafica_Sc3_CP_opt <- graph_from_biadjacency_matrix(matriz_Sc3_CP_inc_opt)
grafica_Ti2_CP_opt <- graph_from_biadjacency_matrix(matriz_Ti2_CP_inc_opt)
grafica_Ti3_CP_opt <- graph_from_biadjacency_matrix(matriz_Ti3_CP_inc_opt)
grafica_V2_CP_opt <- graph_from_biadjacency_matrix(matriz_V2_CP_inc_opt)
grafica_V3_CP_opt <- graph_from_biadjacency_matrix(matriz_V3_CP_inc_opt)
grafica_V4_CP_opt <- graph_from_biadjacency_matrix(matriz_V4_CP_inc_opt)
grafica_Zn2_CP_opt <- graph_from_biadjacency_matrix(matriz_Zn2_CP_inc_opt)
#Grafica de complejo con geometria tetraedrica optimizada (matriz incidencia)
grafica_Co2_T_opt <- graph_from_biadjacency_matrix(matriz_Co2_T_inc_opt)
grafica_Cr2_T_opt <- graph_from_biadjacency_matrix(matriz_Cr2_T_inc_opt)
grafica_Cr3_T_opt <- graph_from_biadjacency_matrix(matriz_Cr3_T_inc_opt)
grafica_Cu2_T_opt <- graph_from_biadjacency_matrix(matriz_Cu2_T_inc_opt)
grafica_Fe2_T_opt <- graph_from_biadjacency_matrix(matriz_Fe2_T_inc_opt)
grafica_Fe3_T_opt <- graph_from_biadjacency_matrix(matriz_Fe3_T_inc_opt)
grafica_Mn2_T_opt <- graph_from_biadjacency_matrix(matriz_Mn2_T_inc_opt)
grafica_Mn3_T_opt <- graph_from_biadjacency_matrix(matriz_Mn3_T_inc_opt)
grafica_Mn4_T_opt <- graph_from_biadjacency_matrix(matriz_Mn4_T_inc_opt)
grafica_Ni2_T_opt <- graph_from_biadjacency_matrix(matriz_Ni2_T_inc_opt)
grafica_Ni3_T_opt <- graph_from_biadjacency_matrix(matriz_Ni3_T_inc_opt)
grafica_Sc2_T_opt <- graph_from_biadjacency_matrix(matriz_Sc2_T_inc_opt)
grafica_Sc3_T_opt <- graph_from_biadjacency_matrix(matriz_Sc3_T_inc_opt)
grafica_Ti2_T_opt <- graph_from_biadjacency_matrix(matriz_Ti2_T_inc_opt)
grafica_Ti3_T_opt <- graph_from_biadjacency_matrix(matriz_Ti3_T_inc_opt)
grafica_V2_T_opt <- graph_from_biadjacency_matrix(matriz_V2_T_inc_opt)
grafica_V3_T_opt <- graph_from_biadjacency_matrix(matriz_V3_T_inc_opt)
grafica_V4_T_opt <- graph_from_biadjacency_matrix(matriz_V4_T_inc_opt)
grafica_Zn2_T_opt <- graph_from_biadjacency_matrix(matriz_Zn2_T_inc_opt)

# LISTA GRAFICAS ----------------------------------------------------------
#listas de graficas
lista_graficas_octaedrico <- list("Co2" = grafica_Co2,"Cr2" = grafica_Cr2,"Cr3" = grafica_Cr3,"Cu2" = grafica_Cu2,"Fe2" = grafica_Fe2,"Fe3" = grafica_Fe3,"Mn2" = grafica_Mn2,"Mn3" = grafica_Mn3,"Mn4" = grafica_Mn4,"Ni2" = grafica_Ni2,"Ni3" = grafica_Ni3,"Sc2" = grafica_Sc2,"Sc3" = grafica_Sc3,"Ti2" = grafica_Ti2,"Ti3" = grafica_Ti3,"V2" = grafica_V2,"V3" = grafica_V3,"V4" =  grafica_V4,"Zn2" = grafica_Zn2)  #MATRIZ DE ADYACENCIA
lista_graficas_cuadrado_plano <- list("Co2" = grafica_Co2_CP_A,"Cr2" = grafica_Cr2_CP_A,"Cr3" = grafica_Cr3_CP_A,"Cu2" = grafica_Cu2_CP_A,"Fe2" = grafica_Fe2_CP_A,"Fe3" = grafica_Fe3_CP_A,"Mn2" = grafica_Mn2_CP_A,"Mn3" = grafica_Mn3_CP_A,"Mn4" = grafica_Mn4_CP_A,"Ni2" = grafica_Ni2_CP_A,"Ni3" = grafica_Ni3_CP_A,"Sc2" = grafica_Sc2_CP_A,"Sc3" = grafica_Sc3_CP_A,"Ti2" = grafica_Ti2_CP_A,"Ti3" = grafica_Ti3_CP_A,"V2" = grafica_V2_CP_A,"V3" = grafica_V3_CP_A,"V4" = grafica_V4_CP_A,"Zn2" = grafica_Zn2_CP_A) #MATRIZ DE ADYACENCIA
lista_graficas_tetraedrico <- list("Co2" = grafica_Co2_T_A,"Cr2" = grafica_Cr2_T_A,"Cr3" = grafica_Cr3_T_A,"Cu2" = grafica_Cu2_T_A,"Fe2" = grafica_Fe2_T_A,"Fe3" = grafica_Fe3_T_A,"Mn2" = grafica_Mn2_T_A,"Mn3" = grafica_Mn3_T_A,"Mn4" = grafica_Mn4_T_A,"Ni2" = grafica_Ni2_T_A,"Ni3" = grafica_Ni3_T_A,"Sc2" = grafica_Sc2_T_A,"Sc3" = grafica_Sc3_T_A,"Ti2" = grafica_Ti2_T_A,"Ti3" = grafica_Ti3_T_A,"V2" = grafica_V2_T_A,"V3" = grafica_V3_T_A,"V4" = grafica_V4_T_A,"Zn2" = grafica_Zn2_T_A) #MATRIZ DE ADYACENCIA
lista_graficas_Cu_phen  <- list("s0" = grafica_Cu_phen_s0,"s1" = grafica_Cu_phen_s1,"s2" = grafica_Cu_phen_s2,"s3" = grafica_Cu_phen_s3,"s4" = grafica_Cu_phen_s4,"s5" = grafica_Cu_phen_s5,"s6" = grafica_Cu_phen_s6,"s7" = grafica_Cu_phen_s7,"s8" = grafica_Cu_phen_s8,"s9" = grafica_Cu_phen_s9,"s10" = grafica_Cu_phen_s10,"s11" = grafica_Cu_phen_s11,"s12" = grafica_Cu_phen_s12,"s13" = grafica_Cu_phen_s13,"s14" = grafica_Cu_phen_s14,"s15" = grafica_Cu_phen_s15,"s16" = grafica_Cu_phen_s16,"s17" = grafica_Cu_phen_s17,"s18" = grafica_Cu_phen_s18,"s19" = grafica_Cu_phen_s19,"s20" = grafica_Cu_phen_s20) 
lista_graficas_cuadrada_plana_octaedrica <- list("Co2_CP" = grafica_Co2_CP_A,"Co2_O" = grafica_Co2,"Cr2_CP" = grafica_Cr2_CP_A,"Cr2_O" = grafica_Cr2,"Cr3_CP" = grafica_Cr3_CP_A,"Cr3_O" = grafica_Cr3,"Cu2_CP" = grafica_Cu2_CP_A,"Cu2_O" = grafica_Cu2,"Fe2_CP" = grafica_Fe2_CP_A,"Fe2_O" = grafica_Fe2,"Fe3_CP" = grafica_Fe3_CP_A,"Fe3_O" = grafica_Fe3,"Mn2_CP" = grafica_Mn2_CP_A,"Mn2_O" = grafica_Mn2,"Mn3_CP" = grafica_Mn3_CP_A,"Mn3_O" = grafica_Mn3,"Mn4_CP" = grafica_Mn4_CP_A,"Mn4_O" = grafica_Mn4,"Ni2_CP" = grafica_Ni2_CP_A,"Ni2_O" = grafica_Ni2,"Ni3_CP" = grafica_Ni3_CP_A,"Ni3_O" = grafica_Ni3,"Sc2_CP" = grafica_Sc2_CP_A,"Sc2_O" = grafica_Sc2,"Sc3_CP" = grafica_Sc3_CP_A,"Sc3_O" = grafica_Sc3,"Ti2_CP" = grafica_Ti2_CP_A,"Ti2_O" = grafica_Ti2,"Ti3_CP" = grafica_Ti3_CP_A,"Ti3_O" = grafica_Ti3,"V2_CP" = grafica_V2_CP_A,"V2_O" = grafica_V2,"V3_CP" = grafica_V3_CP_A,"V3_O" = grafica_V3,"V4_CP" = grafica_V4_CP_A,"V4_O" = grafica_V4,"Zn2_CP" = grafica_Zn2_CP_A,"Zn2_O" = grafica_Zn2) 
lista_graficas_tetraedrico_octaedrica <- list("Co2_T" = grafica_Co2_T_A,"Co2_O" = grafica_Co2,"Cr2_T" = grafica_Cr2_T_A,"Cr2_O" = grafica_Cr2,"Cr3_T" = grafica_Cr3_T_A,"Cr3_O" = grafica_Cr3,"Cu2_T" = grafica_Cu2_T_A,"Cu2_O" = grafica_Cu2,"Fe2_T" = grafica_Fe2_T_A,"Fe2_O" = grafica_Fe2,"Fe3_T" = grafica_Fe3_T_A,"Fe3_O" = grafica_Fe3,"Mn2_T" = grafica_Mn2_T_A,"Mn2_O" = grafica_Mn2,"Mn3_T" = grafica_Mn3_T_A,"Mn3_O" = grafica_Mn3,"Mn4_T" = grafica_Mn4_T_A,"Mn4_O" = grafica_Mn4,"Ni2_T" = grafica_Ni2_T_A,"Ni2_O" = grafica_Ni2,"Ni3_T" = grafica_Ni3_T_A,"Ni3_O" = grafica_Ni3,"Sc2_T" = grafica_Sc2_T_A,"Sc2_O" = grafica_Sc2,"Sc3_T" = grafica_Sc3_T_A,"Sc3_O" = grafica_Sc3,"Ti2_T" = grafica_Ti2_T_A,"Ti2_O" = grafica_Ti2,"Ti3_T" = grafica_Ti3_T_A,"Ti3_O" = grafica_Ti3,"V2_T" = grafica_V2_T_A,"V2_O" = grafica_V2,"V3_T" = grafica_V3_T_A,"V3_O" = grafica_V3,"V4_T" = grafica_V4_T_A,"V4_O" = grafica_V4,"Zn2_T" = grafica_Zn2_T_A,"Zn_O" = grafica_Zn2) 
lista_graficas_geometrias <- list("Co2_CP" = grafica_Co2_CP_A,"Co2_T" = grafica_Co2_T_A,"Co2_O" = grafica_Co2,"Cr2_CP" = grafica_Cr2_CP_A,"Cr2_T" = grafica_Cr2_T_A,"Cr2_O" = grafica_Cr2,"Cr3_CP" = grafica_Cr3_CP_A,"Cr3_T" = grafica_Cr3_T_A,"Cr3_O" = grafica_Cr3,"Cu2_CP" = grafica_Cu2_CP_A,"Cu2_T" = grafica_Cu2_T_A,"Cu2_O" = grafica_Cu2,"Fe2_CP" = grafica_Fe2_CP_A,"Fe2_T" = grafica_Fe2_T_A,"Fe2_O" = grafica_Fe2,"Fe3_CP" = grafica_Fe3_CP_A,"Fe3_T" = grafica_Fe3_T_A,"Fe3_O" = grafica_Fe3,"Mn2_CP" = grafica_Mn2_CP_A,"Mn2_T" = grafica_Mn2_T_A,"Mn2_O" = grafica_Mn2,"Mn3_CP" = grafica_Mn3_CP_A,"Mn3_T" = grafica_Mn3_T_A,"Mn3_O" = grafica_Mn3,"Mn4_CP" = grafica_Mn4_CP_A,"Mn4_T" = grafica_Mn4_T_A,"Mn4_O" = grafica_Mn4,"Ni2_CP" = grafica_Ni2_CP_A,"Ni2_T" = grafica_Ni2_T_A,"Ni2_O" = grafica_Ni2,"Ni3_CP" = grafica_Ni3_CP_A,"Ni3_T" = grafica_Ni3_T_A,"Ni3_O" = grafica_Ni3,"Sc2_CP" = grafica_Sc2_CP_A,"Sc2_T" = grafica_Sc2_T_A,"Sc2_O" = grafica_Sc2,"Sc3_CP" = grafica_Sc3_CP_A,"Sc3_T" = grafica_Sc3_T_A,"Sc3_O" = grafica_Sc3,"Ti2_CP" = grafica_Ti2_CP_A,"Ti2_T" = grafica_Ti2_T_A,"Ti2_O" = grafica_Ti2,"Ti3_CP" = grafica_Ti3_CP_A,"Ti3_T" = grafica_Ti3_T_A,"Ti3_O" = grafica_Ti3,"V2_CP" = grafica_V2_CP_A,"V2_T" = grafica_V2_T_A,"V2_O" = grafica_V2,"V3_CP" = grafica_V3_CP_A,"V3_T" = grafica_V3_T_A,"V3_O" = grafica_V3,"V4_CP" = grafica_V4_CP_A,"V4_T" = grafica_V4_T_A,"V4_O" = grafica_V4,"Zn2_CP" = grafica_Zn2_CP_A,"Zn2_T" = grafica_Zn2_T_A,"Zn2_O" = grafica_Zn2)

# IMPORTANDO DATA FRAMES --------------------------------------------------
#PROPIEDADES DE PUNTOS CRITICOS.
data_CP_I <- read_xls('C:\\Users\\Hector Ramirez\\Documents\\codigo_R\\investigacion_graficas_atomicas\\Datos_CP_geometrias.xls', sheet = 'I')
data_CP_II <- read_xls('C:\\Users\\Hector Ramirez\\Documents\\codigo_R\\investigacion_graficas_atomicas\\Datos_CP_geometrias.xls', sheet = 'II')
data_CP_III <- read_xls('C:\\Users\\Hector Ramirez\\Documents\\codigo_R\\investigacion_graficas_atomicas\\Datos_CP_geometrias.xls', sheet = 'III')
data_CP_IV <- read_xls('C:\\Users\\Hector Ramirez\\Documents\\codigo_R\\investigacion_graficas_atomicas\\Datos_CP_geometrias.xls', sheet = 'IV')
data_CP_V <- read_xls('C:\\Users\\Hector Ramirez\\Documents\\codigo_R\\investigacion_graficas_atomicas\\Datos_CP_geometrias.xls', sheet = 'V')
data_CP_VI <- read_xls('C:\\Users\\Hector Ramirez\\Documents\\codigo_R\\investigacion_graficas_atomicas\\Datos_CP_geometrias.xls', sheet = 'VI')
data_CP_VII <- read_xls('C:\\Users\\Hector Ramirez\\Documents\\codigo_R\\investigacion_graficas_atomicas\\Datos_CP_geometrias.xls', sheet = 'VII')
data_CP_VIII <- read_xls('C:\\\\Users\\\\Hector Ramirez\\Documents\\codigo_R\\investigacion_graficas_atomicas\\Datos_CP_geometrias.xls', sheet = 'VIII')
data_CP_IX <- read_xls('C:\\Users\\Hector Ramirez\\Documents\\codigo_R\\investigacion_graficas_atomicas\\Datos_CP_geometrias.xls', sheet = 'IX')
#IMPORTANDO PROPIEDADES DE PUNTOS CRITICOS DE LAS DIFERENTES GEOMETRIAS.
propiedades_CP <- read_xls('C:\\Users\\Hector Ramirez\\Documents\\codigo_R\\investigacion_graficas_atomicas\\propiedades_CP.xls')
propiedades_moleculas <- read.csv('C:\\Users\\Hector Ramirez\\Documents\\Tesis\\datos\\datos_graficas_geometrias\\propiedades_molecula\\propiedades_molecula.csv')
propiedades_puntos_criticos <- read_xls('C:\\Users\\Hector Ramirez\\Documents\\Tesis\\datos\\datos_graficas_geometrias\\propiedades_CP\\propiedades_puntos_criticos.xls') #solo contiene valores de cuadrado plano y tetraedro
propiedades_prom_CP <- read_xls('C:\\Users\\Hector Ramirez\\Documents\\Tesis\\datos\\datos_graficas_geometrias\\propiedades_CP\\propiedades_puntos_criticos.xls',sheet = 'PROMEDIO') #promedio valores puntos criticos geometrias
propiedades_Cu_phen <- read_xls('C:\\Users\\Hector Ramirez\\Documents\\codigo_R\\investigacion_graficas_atomicas\\propiedades_Cu-phen.xls',sheet = 'molecula')
propiedades_Cu_phen_CP <- read_xls('C:\\Users\\Hector Ramirez\\Documents\\codigo_R\\investigacion_graficas_atomicas\\propiedades_Cu-phen.xls',sheet = 'CP')
propiedades_prom_Cu_phen_CP <- read_xls('C:\\Users\\Hector Ramirez\\Documents\\codigo_R\\investigacion_graficas_atomicas\\propiedades_Cu-phen.xls',sheet = 'PROMEDIO')
df_CP <- read_xls('C:\\Users\\Hector Ramirez\\Documents\\codigo_python\\investigacion_graficas_atomicas\\datos\\CP.xls')       #data sets unifcados con valores puntos criticos, incluye NA.
df_atom <- read_xls('C:\\Users\\Hector Ramirez\\Documents\\codigo_python\\investigacion_graficas_atomicas\\datos\\atom.xls')   #data sets unifcados con valores atomos, incluye NA.
df_mean <- read_xls('C:\\Users\\Hector Ramirez\\Documents\\codigo_python\\investigacion_graficas_atomicas\\datos\\mean.xls')   #data sets unifcados con valores promedio de puntos critico,incluye NA.
# MATRIZ DE PROPIEDADES GEOMETRIAS ----------------------------------------
#MATRIZ DE PESO CON VALORES DE DISTANCIA AL NUCLEO DEL CP (3,+1) 
Sc3_CP_radio <- matrix(c(0,0,0.833182,0.833189,0.833177,0.833193,
                         0,0,0.833188,0.833185,0.833184,0.83319,
                         0.833182,0.833188,0,0.845021,0,0,
                         0.833189,0.833185,0.845021,0,0,0,
                         0.833177,0.833184,0,0,0,0.845022,
                         0.833193,0.83319,0,0,0.845022,0),ncol = 6,nrow = 6,byrow = TRUE)
Sc2_CP_radio <- matrix(c(0,0,0.833307,0.833312,0.83331,0.833314,
                         0,0,0.833307,0.833312,0.83331,0.833314,
                         0.833307,0.833307,0,0.846142,0,0,
                         0.833312,0.833312,0.846142,0,0,0,
                         0.83331,0.83331,0,0,0,0.846138,
                         0.833314,0.833314,0,0,0.846138,0),ncol = 6,nrow = 6,byrow = TRUE)
V4_CP_radio  <- matrix(c(0,0,0.721105,0.721104,0.739952,0.739947,
                         0,0,0.721103,0.721107,0.739953,0.739948,
                         0.721105,0.721103,0,0,0.739951,0.73995,
                         0.721104,0.721107,0,0,0.739951,0.739946,
                         0.739952,0.739953,0.739951,0.739951,0,0,
                         0.739952,0.739948,0.73995,0.739946,0,0),ncol = 6,nrow = 6,byrow = TRUE)
Ti3_CP_radio  <- matrix(c(0,0,0.768723,0.768722,0.789412,0.789411,
                    0,0,0.768721,0.768724,0.789412,0.789411,
                    0.768723,0.768721,0,0,0.78929,0.789292,
                    0.768722,0.768724,0,0,0.789289,0.789292,
                    0.789412,0.789412,0.78929,0.789289,0,0,
                    0.789411,0.789411,0.789292,0.789292,0,0),ncol = 6,nrow = 6,byrow = TRUE)
Ti2_CP_radio  <- matrix(c(0,0,0.798676,0.79868,0.762661,0.762681,
                    0,0,0.798673,0.798678,0.762684,0.762662,
                    0.798676,0.798673,0,0,0.798663,0.798664,
                    0.79868,0.798678,0,0,0.798664,0.798666,
                    0.762661,0.762684,0.798663,0.798664,0,0,
                    0.762681,0.762662,0.798664,0.798666,0,0),ncol = 6,nrow = 6,byrow = TRUE)
V3_CP_radio  <- matrix(c(0,0,0.713498,0.713493,0.758422,0.758408,
                         0,0,0.713512,0.713501,0.758417,0.758404,
                         0.713498,0.713512,0,0,0.751347,0.751326,
                         0.713493,0.713501,0,0,0.751355,0.751334,
                         0.758422,0.758417,0.751347,0.751355,0,0,
                         0.758408,0.758404,0.751326,0.751334,0,0),ncol = 6,nrow = 6,byrow = TRUE)
Cr3_CP_radio <- matrix(c(0,0,0,0.68448,0,0.684481,0.690825,0,
                         0,0,0.684487,0,0.684474,0,0,0.690824,
                         0,0.684487,0,0.681284,0,0,0.684467,0,
                         0.68448,0,0.681284,0,0,0,0,0.684474,
                         0,0.684474,0,0,0,0.681283,0.684467,0,
                         0.684481,0,0,0,0.681283,0,0,0.684473,
                         0.690825,0,0.684467,0,0.684467,0,0,0,
                         0,0.690824,0,0.684474,0,0.684473,0,0),ncol = 8,nrow = 8,byrow = TRUE)
Mn4_CP_radio <- matrix(c(0,0.661507,0,0,0.648622,0,0.648619,0,
                         0.661507,0,0,0,0,0.648616,0,0.648619,
                         0,0,0,0.661507,0.648622,0,0.648619,0,
                         0,0,0.661507,0,0,0.648616,0,0.648619,
                         0.648622,0,0.648622,0,0,0.647921,0,0,
                         0,0.648616,0,0.648616,0.647921,0,0,0,
                         0.648619,0,0.648619,0,0,0,0,0.647921,
                         0,0.648619,0,0.648619,0,0,0.647921,0),ncol = 8,nrow = 8,byrow = TRUE)
V2_CP_radio <- matrix(c(0,0.732989,0,0,0.725639,0,0.725639,0,
                        0.732989,0,0,0,0,0.725634,0,0.725634,
                        0,0,0,0.73299,0.725632,0,0.725633,0,
                        0,0,0.73299,0,0,0.725638,0,0.725637,
                        0.725639,0,0.725632,0,0,0.732989,0,0,
                        0,0.725634,0,0.725638,0.732989,0,0,0,
                        0.725639,0,0.725633,0,0,0,0,0.72675,
                        0,0.725634,0,0.725637,0,0,0.72675,0),ncol = 8,nrow = 8,byrow = TRUE)
Cr2_CP_radio <- matrix(c(0,0,0.707793,0.707811,0.707092,0.707109,
                         0,0,0.707106,0.707089,0.707808,0.707793,
                         0.707793,0.707106,0,0.696731,0.696741,0,
                         0.707811,0.707089,0.696731,0,0,0.69674,
                         0.707092,0.707808,0.696741,0,0,0.69673,
                         0.707109,0.707793,0,0.69674,0.69673,0),ncol = 6,nrow = 6,byrow = TRUE)
Mn3_CP_radio <- matrix(c(0,0,0.66264,0.662636,0.674506,0.674499,
                         0,0,0.674499,0.674506,0.662636,0.662639,
                         0.66264,0.674499,0,0.659451,0.662564,0,
                         0.662636,0.674506,0.659451,0,0,0.662564,
                         0.674506,0.662636,0.662564,0,0,0.659451,
                         0.674499,0.662639,0,0.662564,0.659451,0),ncol = 6,nrow = 6,byrow = TRUE)
Fe3_CP_radio <- matrix(c(0,0.634809+0.61711,0.616363+0.616364,0,
                         0.634809+0.61711,0,0,0.616362+0.616365,
                         0.616363+0.616364,0,0,0.617109+0.634808,
                         0,0.616362+0.616365,0.616363+0.616364,0),ncol = 4,nrow = 4,byrow = TRUE)
Fe31_CP_radio <- matrix(c(0,0.61711,0.634809,0,0.616363,0,0.616364,0,
                          0,0.61711,0.634809,0,0,0.616365,0,0.616362,
                          0.617109,0,0,0.634808,0.616363,0,0.616364,0,
                          0.617109,0,0,0.634808,0,0.616365,0,0.616362),ncol = 8,nrow = 4,byrow = TRUE)
Mn2_CP_radio <- matrix(c(0,0.652626,1.310343,0,
                         0.652626,0,0,1.310343,
                         1.310343,0,0,1.310343,
                         0,1.310343,0.652626,0),ncol = 4,nrow = 4,byrow = TRUE)
Mn21_CP_radio <- matrix(c(0,0.652626,0.655172,0,0.655171,0,
                          0,0.652626,0,0.655172,0,0.655171,
                          0.652626,0,0.655172,0,0.655171,0,
                          0.652626,0,0,0.655172,0,0.655171),ncol = 6,nrow = 4,byrow = TRUE)
Fe2_CP_radio <- matrix(c(0,0.61483,0,0,0.607025,0,0.607021,0,
                         0.61483,0,0,0,0,0.607024,0,0.607023,
                         0,0,0,0.614827,0.607019,0,0.607028,0,
                         0,0,0.614827,0,0,0.607022,0,0.607027,
                         0.607025,0,0.607019,0,0,0.603368,0,0,
                         0,0.607024,0,0.607022,0.603368,0,0,0,
                         0.607021,0,0.607028,0,0,0,0,0.60337,
                         0,0.607023,0,0.607027,0,0,0.60337,0),ncol = 8,nrow = 8,byrow = TRUE)
Co2_CP_radio <- matrix(c(0,0.582687,0.58274,0,0.570717,0,0,0,
                         0.582687,0,0,0.582761,0,0.582761,0,0,
                         0.58274,0,0,0.582686,0,0,0.570711,0,
                         0,0.582761,0.582686,0,0,0,0,0.570704,
                         0.570717,0,0,0,0,0.582678,0.58275,0,
                         0,0.570709,0,0,0.582678,0,0,0.582757,
                         0,0,0.570711,0,0.58275,0,0,0.582701,
                         0,0,0,0.570704,0,0.582757,0.582701,0),ncol = 8,nrow = 8,byrow = TRUE)
Ni3_CP_radio <- matrix(c(0,0,0.552836,0,0.552836,0,0.575218,0,
                         0,0,0,0.552853,0,0.552853,0,0.575211,
                         0.552836,0,0,0.555476,0,0,0,0.552276,
                         0,0.552853,0.555476,0,0,0,0.552271,0,
                         0.552836,0,0,0,0,0.555476,0,0.552276,
                         0,0.552853,0,0,0.555476,0,0.552272,0,
                         0.575218,0,0,0.552271,0,0.552272,0,0,
                         0,0.575211,0.552276,0,0.552276,0,0,0),ncol = 8,nrow = 8,byrow = TRUE)
Ni2_CP_radio <- matrix(c(0,0,0.553938,0,0.556166,0,0.553939,0,
                         0,0,0,0.556167,0,0.553958,0,0.553904,
                         0.553938,0,0,0.553884,0,0,0,0.556167,
                         0,0.556167,0.553884,0,0,0,0.553937,0,
                         0.556166,0,0,0,0,0.553919,0,0.553916,
                         0,0.553958,0,0,0.553919,0,0.556162,0,
                         0.553939,0,0,0.553937,0,0.556162,0,0,
                         0,0.553904,0.556167,0,0.553916,0,0,0),ncol = 8,nrow = 8,byrow = TRUE)
Cu2_CP_radio  <- matrix(c(0,1.071006,1.070838,0,
                          1.071006,0,0,1.070813,
                          1.070838,0,0,1.071019,
                          0,1.070813,1.071019,0),ncol = 4,nrow = 4,byrow = TRUE)
Cu21_CP_radio <- matrix(c(0,0.535461,0.535369,0,0.535469,0,0,0.535545,
                          0.535447,0.535461,0,0.535366,0,0,0,0.535545,
                          0,0,0.535369,0,0.535469,0.535466,0.535553,0,
                          0.535447,0,0,0.535366,0,0.535466,0.535553,0),ncol = 8,nrow = 4,byrow = TRUE)
Zn2_CP_radio <- matrix(c(0,0,1.022487,1.022487,
                         0,0,1.022486,1.022486,
                         1.022487,1.022486,0,0,
                         1.022487,1.022486,0,0),ncol = 4,nrow = 4,byrow = TRUE)
Zn21_CP_radio <- matrix(c(0.511244,0,0.511244,0,0.511243,0,0.511243,0,
                          0,0.511243,0,0.511243,0,0.511243,0,0.511243,
                          0.511244,0.511243,0,0,0.511243,0.511243,0,0,
                          0,0,0.511244,0.511243,0,0,0.511243,0.511243),ncol = 8,nrow = 4,byrow = TRUE)
lista_matrices_CP_radio <- list("Sc3" = Sc3_CP_radio,"Sc2" = Sc2_CP_radio,"V4" = V4_CP_radio,"Ti3" = Ti3_CP_radio,"Ti2" = Ti2_CP_radio,"V3" = V3_CP_radio,"Cr3" = Cr3_CP_radio,"Mn4" = Mn4_CP_radio,"V2" = V2_CP_radio,"Cr2" = Cr2_CP_radio,"Mn3" = Mn3_CP_radio,"Fe3" = Fe3_CP_radio,"Mn2" = Mn2_CP_radio,"Fe2" = Fe2_CP_radio,"Co2" = Co2_CP_radio,"Ni3" = Ni3_CP_radio,"Ni2" = Ni2_CP_radio,"Cu2" = Cu2_CP_radio,"Zn2" = Zn2_CP_radio)

#MATRIZ DE PESO CON VALORES DE \\RHO\\ DEL CP (3,+1).
Sc3_CP_rho  <- matrix(c(0,0,0.990215,0.990227,0.990231,0.83319,
                        0,0,0.833182,0.833189,0.833177,0.833193,
                        0.990215,0.990288,0,0.947542,0,0,
                        0.990227,0.990181,0.947542,0,0,0,
                        0.990231,0.990311,0,0,0,0.947538,
                        0.990204,0.990165,0,0,0.947538,0),nrow = 6,ncol = 6,byrow = TRUE)
Sc2_CP_rho <- matrix(c(0,0,0.985107,0.985062,0.985093,0.985051,
                       0,0,0.985108,0.985066,0.985095,0.985055,
                       0.985107,0.985108,0,0.93885,0,0,
                       0.985062,0.985066,0.93885,0,0,0,
                       0.985093,0.985095,0,0,0,0.938894,
                       0.985051,0.985055,0,0,0.938894,0),nrow = 6,ncol = 6,byrow = TRUE)
V4_CP_rho <- matrix(c(0,0,1.741068,1.741088,1.552983,1.553064,
                      0,0,1.741094,1.741044,1.552988,1.553054,
                      1.741068,1.741094,0,0,1.553008,1.55303,
                      1.741088,1.741044,0,0,1.552998,1.553081,
                      1.552983,1.552988,1.553008,1.552998,0,0,
                      1.553064,1.553054,1.55303,1.553081,0,0),nrow = 6,ncol = 6,byrow = TRUE)
Ti3_CP_rho <- matrix(c(0,0,1.34912,1.349131,1.193393,1.193395,
                      0,0,1.349144,1.349097,1.193383,1.1934,
                      1.34912,1.349144,0,0,1.194433,1.194419,
                      1.349131,1.349097,0,0,1.194445,1.194414,
                      1.193393,1.193383,1.194433,1.194445,0,0,
                      1.193395,1.1934,1.194419,1.194414,0,0),nrow = 6,ncol = 6,byrow = TRUE)
Ti2_CP_rho <- matrix(c(0,0,1.13465,1.13463,1.412665,1.412384,
                       0,0,1.13468,1.134642,1.412354,1.412643,
                       1.13465,1.13468,0,0,1.134771,1.13468,
                       1.13463,1.134642,0,0,1.134765,1.134749,
                       1.412665,1.412354,1.134771,1.134755,0,0,
                       1.412384,1.412643,1.13468,1.134749,0,0),nrow = 6,ncol = 6,byrow = TRUE)
#MATRIZ DE PESO CON VALORES DELSQ\\RHO\\ DEL CP (3,+1).
Sc3_CP_DelSqRho <- matrix(c(0,0,-5.869398,-5.869845,-5.870332,-5.868758,
                            0,0,-5.870212,-5.869449,-5.871277,-5.868551,
                            -5.869398,-5.870212,0,-4.488918,0,0,
                            -5.869845,-5.869449,-4.488918,0,0,0,
                            -5.870332,-5.871277,0,0,0,-4.488857,
                            -5.868758,-5.868551,0,0,-4.488857,0),nrow = 6,ncol = 6,byrow = TRUE)
Sc2_CP_DelSqRho <- matrix(c(0,0,-5.848477,-5.847674,-5.848084,-5.847426,
                            0,0,-5.848488,-5.847722,-5.848129,-5.847478,
                            -5.848477,-5.848488,0,-4.318839,0,0,
                            -5.847674,-5.847722,-4.318839,0,0,0,
                            -5.848084,-5.848129,0,0,0,-4.31949,
                            -5.847426,-5.847478,0,0,-4.31949,0),nrow = 6,ncol = 6,byrow = TRUE) 
V4_CP_DelSqRho  <- matrix(c(0,0,-15.324214,-15.324542,-8.781911,-8.783377,
                            0,0,-15.324661,-15.323377,-8.781827,-8.782943,
                            -15.324214,-15.324661,0,0,-8.782639,-8.782439,
                            -15.324542,-15.323377,0,0,-8.78214,-8.783613,
                            -8.781911,-8.781827,-8.782639,-8.78214,0,0,
                            -8.783377,-8.782943,-8.782439,-8.783613,0,0),nrow = 6,ncol = 6,byrow = TRUE)
Ti3_CP_DelSqRho <- matrix(c(0,0,-10.713458,-10.713675,-5.875697,-5.875864,
                            0,0,-10.713972,-10.712968,-5.875548,-5.876014,
                            -10.713458,-10.713972,0,0,-5.902654,-5.902382,
                            -10.713675,-10.712968,0,0,-5.902961,-5.902365,
                            -5.875697,-5.875548,-5.902654,-5.902961,0,0,
                            -5.875864,-5.876014,-5.902382,-5.902365,0,0),nrow = 6,ncol = 6,byrow = TRUE)
Ti2_CP_DelSqRho <- matrix(c(0,0,-4.077673,-4.077463,-12.574003,-12.568131,
                            0,0,-4.078355,-4.077563,-12.567288,-12.573459,
                            -4.077673,-4.078355,0,0,-4.080325,-4.080212,
                            -4.077463,-4.077563,0,0,-4.080184,-4.079896,
                            -12.574003,-12.567288,-4.080325,-4.080184,0,0,
                            -12.568131,-12.573459,-4.078355,-4.079896,0,0),nrow = 6,ncol = 6,byrow = TRUE)
#MATRIZ DE PESO CON VALORES Ven(r) DEL CP (3,+1).
Sc3_CP_Ven <- matrix(c(0,0,-10509.080061,-9601.5946065,-2276.4580185,-4729639.857,
                       0,0,-2295.1373504,-10840.200183,-1905.4584296,-10.517861986,
                       -10509.080061,-2295.1373504,0,-53.569395134,0,0,
                       -9601.5946065,-10840.200183,-53.569395134,0,0,0,
                       -2276.4580185,-1905.4584296,0,0,0,-52.362050038,
                       -4729639.857,-10.517861986,0,0,-52.362050038,0),nrow = 6,ncol = 6,byrow = TRUE)

Sc2_CP_Ven  <- matrix(c(0,0,-33.956993215,-33.922765884,-33.956993215,-33.921921806,
                        0,0,-33.974967342,-33.971380245,-33.974036428,-33.970664612,
                        -33.956993215,-33.974967342,0,-31.789857275,0,0,
                        -33.922765884,-33.971380245,-31.789857275,0,0,0,
                        -33.956993215,-33.974036428,0,0,0,-31.79020399,
                        -33.921921806,-33.970664612,0,0,-31.79020399,0),nrow = 6,ncol = 6,byrow = TRUE)
V4_CP_Ven  <- matrix(c(0,0,-10594.720532,-10640.017884,-11770.670828,-11804.815672,
                       0,0,-10586.061806,-10649.722535,-11733.336332,-11734.407198,
                       -10594.720532,-10586.061806,0,0,-11812.459288,-11758.765681,
                       -10640.017884,-10649.722535,0,0,-11715.307072,-1957.8162001,
                       -11770.670828,-11733.336332,-11812.459288,-11715.307072,0,0,
                       -11804.815672,-11734.407198,-11758.765681,-1957.8162001,0,0),nrow = 6,ncol = 6,byrow = TRUE)
Ti3_CP_Ven  <- matrix(c(0,0,-52.42137876,-52.419049619,-45.272824976,-45.292942215,
                        0,0,-52.424007494,-52.424733176,-45.276451174,-45.297125061,
                        -52.42137876,-52.424007494,0,0,-45.318853215,-45.339445009,
                        -52.419049619,-52.424733176,0,0,-45.318516242,-45.339639768,
                        -45.272824976,-45.276451174,-45.318853215,-45.318516242,0,0,
                        -45.292942215,-45.318516242,-45.339445009,-45.339639768,0,0),nrow = 6,ncol = 6,byrow = TRUE)
Ti2_CP_Ven <- matrix(c(0,0,-20.881047626,-18.510288273,-1901.9619803,-1861.2921243,
                       0,0,-839102913.6,-20.906229906,-10193.755634,-99.075000756,
                       -20.881047626,-839102913.6,0,0,-2334.7982761,-11259.840952,
                       -18.510288273,-20.906229906,0,0,-2335.7664269,-11249.762599,
                       -1901.9619803,-10193.755634,-2334.7982761,-2335.7664269,0,0,
                       -1861.2921243,-99.075000756,-839102913.6,-11249.762599,0,0),nrow = 6,ncol = 6,byrow = TRUE)
V3_CP_Ven <- matrix(c(0,0,-10499.47556,-10318.731552,-107.7231896,-110.01050058,
                      0,0,-10925.938492,-10602.202347,-77.146297234,-77.289794855,
                      -10499.47556,-10925.938492,0,0,-13.63205809,-13.23717362,
                      -10318.731552,-10602.202347,0,0,-13.1894954,-13.556988743,
                      -107.7231896,-77.146297234,-13.63205809,-13.1894954,0,0,
                      -110.01050058,-77.289794855,-13.23717362,-13.556988743,0,0),nrow = 6,ncol = 6,byrow = TRUE)
Cr3_CP_Ven  <- matrix(c(0,0,0,-29.003655865,0,-28.956017129,-114.64011757,0,
                        0,0,-116.08190871,0,-28.764864406,0,0,-151.66987046,
                        0,-116.08190871,0,-28.648725323,0,0,-29.012828292,0,
                        -29.003655865,0,-28.648725323,0,0,0,0,-28.764864406,
                        0,-28.764864406,0,0,0,-28.64051818,-28.666751801,0,
                        -28.956017129,0,0,0,-28.64051818,0,0,-29.055010037,
                        -114.64011757,0,-29.012828292,0,-28.666751801,0,0,0,
                        0,-151.66987046,0,-28.764864406,0,-29.055010037,0,0),nrow = 8,ncol = 8,byrow = TRUE)
Mn4_CP_Ven <- matrix(c(0,-172.63610996,0,0,-172.55484423,0,-176.37575161,0,
                       -172.63610996,0,0,0,0,-185.26120181,0,-173.28737637,
                       0,0,0,-175.86884153,-172.0034505,0,-161.68610764,0,
                       0,0,-175.86884153,0,0,-175.84392866,0,-182.78477252,
                       -172.55484423,0,-172.0034505,0,0,-161.78910912,0,0,
                       0,-185.26120181,0,-175.84392866,-161.78910912,0,0,0,
                       -176.37575161,0,-161.68610764,0,0,0,0,-152.06221796,
                       0,-173.28737637,0,-182.78477252,0,0,-152.06221796,0),nrow = 8,ncol = 8,byrow = TRUE)
V2_CP_Ven <- matrix(c(0,-125.37301028,0,0,-137.04122944,0,-125.09276425,0,
                      -125.37301028,0,0,0,0,-130.35068321,0,-117.12559642,
                      0,0,0,-124.65378466,-112.88694897,0,-123.69636019,0,
                      0,0,-124.65378466,0,0,-116.92334959,0,-130.03611129,
                      -137.04122944,0,-112.88694897,0,0,-125.37301028,0,0,
                      0,-130.35068321,0,-116.92334959,-125.37301028,0,0,0,
                      -125.09276425,0,-123.69636019,0,0,0,0,-123.4770533,
                      0,-117.12559642,0,-130.03611129,0,0,-123.4770533,0),nrow = 8, ncol = 8,byrow = TRUE)
Cr2_CP_Ven <- matrix(c(0,0,-1647.6911413,-1647.7178768,-144.30835652,-144.21361561,
                       0,0,-84.772756387,-144.30139857,-1651.8385603,-1652.7687294,
                       -1647.6911413,-84.772756387,0,-12094.761821,-12094.756045,0,
                       -1647.7178768,-144.30139857,-12094.761821,0,0,-12047.01263,
                       -144.30835652,-1651.8385603,-12094.756045,0,0,-1670.4530873,
                       -144.21361561,-1652.7687294,0,-12047.01263,-1670.4530873,0),nrow = 6,ncol = 6,byrow = TRUE)
Mn3_CP_Ven <- matrix(c(0,0,-11948.361584,-11858.913207,-101.38087861,-111.82481918,
                       0,0,-112.49471918,-163.22777023,-11949.735795,-11856.536667,
                       -11948.361584,-112.49471918,0,-1243.6475878,-1348.5933118,0,
                       -11858.913207,-163.22777023,-1243.6475878,0,0,-11866.64008,
                       -101.38087861,-11949.735795,-1348.5933118,0,0,-1251.4945729,
                       -111.82481918,-11856.536667,0,-11866.64008,-1251.4945729,0),nrow = 6,ncol = 6,byrow = TRUE)
Fe3_CP_Ven  <- matrix(c(0,-37822.171856+-276.15621173,-269.32446431+-218.08855751,0,
                        -37822.171856+-276.15621173,0,0,-251.64653699+-270.71071096,
                        -269.32446431+-218.08855751,0,0,-201.60585378+-3985.0494675,
                        0,-251.64653699+-270.71071096,-269.32446431+-218.08855751,0),nrow = 4,ncol = 4,byrow = TRUE)
Mn2_CP_Ven <- matrix(c(0,-15091.254101,-15091.258586+-15031.030211,0,
                      -15091.254101,0,0,-15030.885556+-15091.275864,
                      -15091.258586+-15031.030211,0,0,-15031.005457,
                      0,-15030.885556+-15091.275864,-15031.005457,0),nrow = 4,ncol = 4,byrow = TRUE)
Fe2_CP_Ven <- matrix(c(0,-204.84437405,0,0,-173.90756214,0,-204.58213222,0,
                       -204.84437405,0,0,0,0,-195.10160234,0,-223.73766361,
                       0,0,0,-191.06386969,-26.377841225,0,-205.02194278,0,
                       0,0,-191.06386969,0,0,-224.01204997,0,-220.5768563,
                       -173.90756214,0,-26.377841225,0,0,-540078856370,0,0,
                       0,-195.10160234,0,-224.01204997,-540078856370,0,0,0,
                       -204.58213222,0,-205.02194278,0,0,0,0,-25.887611692,
                       0,-223.73766361,0,-220.5768563,0,0,-25.887611692,0),nrow = 8,ncol = 8,byrow = TRUE)
Co2_CP_Ven <- matrix(c(0,-258.63372873,-259.52987237,0,-14260.723833,0,0,0,
                       -258.63372873,0,0,-24.620485489,0,-14540.917846,0,0,
                       -259.52987237,0,0,-187.45120181,0,0,-14414.658044,0,
                       0,-24.620485489,-187.45120181,0,0,0,0,-14545.340153,
                       -14260.723833,0,0,0,0,-236.77285297,-249.67107994,0,
                       0,-14540.917846,0,0,-236.77285297,0,0,-24.583660706,
                       0,0,-14414.658044,0,-249.67107994,0,0,-162.06339943,
                       0,0,0,-14545.340153,0,-24.583660706,-162.06339943,0),nrow = 8,ncol = 8,byrow = TRUE)
Ni3_CP_Ven  <- matrix(c(0,0,-15412.994774,0,-17633.814626,0,-243.83637348,0,
                        0,0,0,-14831.807103,0,-15810.401356,0,-243.36900861,
                        -15412.994774,0,0,-146.57006449,0,0,0,-13257.521167,
                        0,-14831.807103,-146.57006449,0,0,0,-1358.5579656,0,
                        -17633.814626,0,0,0,0,-166.00637997,0,-15256.014474,
                        0,-15810.401356,0,0,-166.00637997,0,-1644.010829,0,
                        -243.83637348,0,0,-1358.5579656,0,-1644.010829,0,0,
                        0,-243.36900861,-13257.521167,0,-15256.014474,0,0,0),nrow = 8,ncol = 8,byrow = TRUE)
Ni2_CP_Ven  <- matrix(c(0,0,-8.615864398,0,-151.66091761,0,-11.017022418,0,
                        0,0,0,-132.21266249,0,-153.12825103,0,-10.73248113,
                        -8.615864398,0,0,-10.729575705,0,0,0,-154.3937516,
                        0,-132.21266249,-10.729575705,0,0,0,-11.771795136,0,
                        -151.66091761,0,0,0,0,-12.11519023,0,-144.28947492,
                        0,-153.12825103,0,0,-12.11519023,0,-144.28947492,0,
                        -11.017022418,0,0,-11.771795136,0,-144.28947492,0,0,
                        0,-10.73248113,-154.3937516,0,-10.68752543,0,0,0),nrow = 8,ncol = 8,byrow = TRUE)
Cu2_CP_Ven <- matrix(c(0,-66.374245323+-393.64399832,-439.55847153+-25366.502486,0,
                       -393.64399832+-66.374245323,0,0,-19246.668574+-1871630.3008,
                       -439.55847153+-25366.502486,0,0,-66.410074653+-439.38874887,
                       0,-1871630.3008+-19246.668574,-66.410074653+-439.38874887,0),nrow = 4,ncol = 4,byrow = TRUE)
Zn2_CP_Ven <- matrix(c(0,0,-460.19652837+-73.401321351,-73.289173037+-465.23052318,
                       0,0,-74.382137064+-26946.492732,-6764741396.2+-74.422245696,
                       -460.19652837+-73.401321351,-74.382137064+-26946.492732,0,0,
                       -73.289173037+-465.23052318,-74.422245696+-6764741396.2,0,0),nrow = 4,ncol = 4,byrow = TRUE)
lista_matrices_CP_Ven <- list("Sc3" = Sc3_CP_Ven,"Sc2" = Sc2_CP_Ven,"V4" = V4_CP_Ven,"Ti3" = Ti3_CP_Ven,"Ti2" = Ti2_CP_Ven,"V3" = V3_CP_Ven,"Cr3" = Cr3_CP_Ven,"Mn4" = Mn4_CP_Ven,"V2" = V2_CP_Ven,"Cr2" = Cr2_CP_Ven,"Mn3" = Mn3_CP_Ven,"Fe3" = Fe3_CP_Ven,"Mn2" = Mn2_CP_Ven,"Fe2" = Fe2_CP_Ven,"Co2" = Co2_CP_Ven,"Ni3" = Ni3_CP_Ven,"Ni2" = Ni2_CP_Ven,"Cu2" = Cu2_CP_Ven,"Zn2" = Zn2_CP_Ven) 
Sc3_T_Ven  <- matrix(c(0,-33.669823024,-33.66380478,-33.55532675,
                       -33.669823024,0,-33.555780845,-33.6778552,
                       -33.66380478,-33.555780845,0,-33.67207343,
                       -33.55532675,-33.6778552,-33.67207343,0),nrow = 4,ncol = 4,byrow = TRUE)
Sc2_T_Ven <- matrix(c(0,0,0,0,-33.394989342,-33.351888586,
                      0,0,0,0,-33.394484409,-33.353530716,
                      0,0,0,0,-33.657093211,-33.686906808,
                      0,0,0,0,-33.667344734,-33.690316421,
                      -33.394989342,-33.394484409,-33.657093211,-33.667344734,0,0,
                      -33.351888586,-33.353530716,-33.686906808,-33.690316421,0,0),nrow = 6,ncol = 6,byrow = TRUE)
V4_T_Ven <- matrix(c(0,0,-2062.4649196,-2032.7246649,-11857.054824,-11802.116992,
                     0,0,-11854.646608,-11864.790273,-2030.6119691,-11906.199403,
                     -2062.4649196,-11854.646608,0,0,0,0,
                     -2032.7246649,-11864.790273,0,0,0,0,
                     -11857.054824,-2030.6119691,0,0,0,0,
                     -11802.116992,-11906.199403,0,0,0,0),nrow = 6,ncol = 6,byrow = TRUE)
Ti3_T_Ven <- matrix(c(0,-47.810409645,0,0,0,-48.577929568,-47.880781206,0,
                      -47.810409645,0,0,0,-48.576336928,0,0,-47.878080152,
                      0,0,0,-47.820251675,-47.879542759,0,-48.57593935,0,
                      0,0,-47.820251675,0,0,-47.891960181,0,-48.582894325,
                      0,-48.576336928,-47.879542759,0,0,0,0,0,
                      -48.577929568,0,0,-47.891960181,0,0,0,0,
                      -47.880781206,0,-48.57593935,0,0,0,0,0,
                      0,-47.878080152,0,-48.582894325,0,0,0,0),nrow = 8,ncol = 8,byrow = TRUE)
Ti2_T_Ven <- matrix(c(0,0,-45.97055382,-45.860061196,-44.030608387,-44.229243635,
                      0,0,-44.033884473,-44.2357036,-46.03971888,-45.82190598,
                      -45.97055382,-44.033884473,0,0,-55.534071933,-55.478640698,
                      -45.860061196,-44.2357036,0,0,-55.605452171,-55.566057976,
                      -44.030608387,-46.03971888,-55.534071933,-55.605452171,0,0,
                      -44.229243635,-45.82190598,-55.478640698,-55.566057976,0,0),nrow = 6,ncol = 6,byrow = TRUE)
V3_T_Ven <- matrix(c(0,0,-67.583464984,-67.587309633,-66.590723037,-66.590090047,
                     0,0,-66.593640137,-66.588078423,-67.583274246,-67.588235736,
                     -67.583464984,-66.593640137,0,0,-67.213748513,-67.2156455,
                     -67.587309633,-66.588078423,0,0,-67.216137113,-67.21258829,
                     -66.590723037,-67.583274246,-67.213748513,-67.216137113,0,0,
                     -66.590090047,-67.588235736,-67.2156455,-67.21258829,0,0),nrow = 6,ncol = 6,byrow = TRUE)
Cr3_T_Ven <- matrix(c(0,0,-89.148423372,-88.357656705,-1674.3048309,-89.188582387,
                      0,0,-1669.7262415,-1609.6997293,-1594.5235115,-1672.8624332,
                      -89.148423372,-1669.7262415,0,0,-12035.730596,-12071.021897,
                      -88.357656705,-1609.6997293,0,0,-12059.308372,-12073.160305,
                      -1674.3048309,-1594.5235115,-12035.730596,-12059.308372,0,0,
                      -89.188582387,-1672.8624332,-12071.021897,-12073.160305,0,0),nrow = 6,ncol = 6,byrow = TRUE)
Mn4_T_Ven <- matrix(c(0,0,-115.32837092,-115.33198477,-123.29362378,-123.29161115,
                      0,0,-123.32112811,-123.33228292,-115.33828689,-115.37656282,
                      -115.32837092,-123.32112811,0,0,-128.9630561,-128.97986623,
                      -115.33198477,-123.33228292,0,0,-129.01028864,-128.99668473,
                      -123.29362378,-115.33828689,-128.9630561,-129.01028864,0,0,
                      -123.29161115,-115.37656282,-128.97986623,-128.99668473,0,0),nrow = 6,ncol = 6,byrow = TRUE)
V2_T_Ven <- matrix(c(0,0,-63.931372186,-63.244679886,-61.540968874,-60.732371658,
                     0,0,-60.874234275,-61.49733016,-63.331979382,-63.840857846,
                     -63.931372186,-60.874234275,0,0,-80.906851044,-80.542972557,
                     -63.244679886,-61.49733016,0,0,-80.662390026,-80.257719605,
                     -61.540968874,-63.331979382,-80.906851044,-80.662390026,0,0,
                     -60.732371658,-63.840857846,-80.542972557,-80.257719605,0,0),nrow = 6,ncol = 6,byrow = TRUE)
Cr2_T_Ven <- matrix(c(0,0,-86.585744752,-87.569706725,-87.557049548,-86.569317249,
                      0,0,-87.566697039,-86.581059508,-86.575761876,-87.556833745,
                      -86.585744752,-87.566697039,0,-87.051910337,-87.059926997,0,
                      -87.569706725,-86.581059508,-87.051910337,0,0,-87.049004528,
                      -87.557049548,-86.575761876,68.251495839,0,0,-87.019132255,
                      -86.569317249,-87.556833745,0,-87.049004528,-87.019132255,0),nrow = 6,ncol = 6,byrow = TRUE)
Mn3_T_Ven <- matrix(c(0,0,-107.11512162,-106.96096159,-110.05414371,-109.91469563,
                      0,0,-109.92418681,-110.08439483,-106.94366498,-107.17887522,
                      -107.11512162,-109.92418681,0,0,-148.61881534,-148.69032186,
                      -106.96096159,-110.08439483,0,0,-148.6486474,-148.7116306,
                      -110.05414371,-106.94366498,-148.61881534,-148.6486474,0,0,
                      -109.91469563,-107.17887522,-148.69032186,-148.7116306,0,0),nrow =6, ncol = 6,byrow = TRUE)
Fe3_T_Ven <- matrix(c(0,0,-164.47508877,-157.1523575,-156.29198155,-175.07669067,
                      0,0,-192.791292,-182.15910592,-158.7120788,-192.90724051,
                      -164.47508877,-192.791292,0,0,-185.15775171,0,
                      -157.1523575,-182.15910592,0,0,0,-172.01122885,
                      -156.29198155,-158.7120788,-185.15775171,0,0,0,
                      -175.07669067,-192.90724051,0,-172.01122885,0,0),nrow = 6,ncol = 6,byrow = TRUE)
Mn2_T_Ven <- matrix(c(0,0,-120.65899373,-126.71207928,-131.59414012,0,
                      0,0,-133.84845194,-142.07102538,0,-126.25366878,
                      -120.65899373,0,0,-126.71207928,-131.59414012,0,
                      -126.71207928,-142.07102538,0,0,-141.1001933,-132.60254486,
                      -131.59414012,0,-124.1313974,-141.1001933,0,0,
                      0,-124.1313974,-120.38122612,-132.60254486,0,0),nrow = 6,ncol = 6,byrow = TRUE)
Fe2_T_Ven <- matrix(c(0,0,-199.97405153,0,-189.17212614,0,-188.45977979,0,
                      0,0,0,-200.00051941,0,-189.19819465,0,-188.61803835,
                      -199.97405153,0,0,-211.04974802,0,0,0,-201.90308176,
                      0,-200.00051941,-211.04974802,0,0,0,-201.76296515,0,
                      -189.17212614,0,0,0,0,-209.55680426,0,-197.27937674,
                      0,-189.19819465,0,0,-209.55680426,0,-197.16153931,0,
                      -188.45977979,0,0,-201.76296515,0,-197.16153931,0,0,
                      0,-188.61803835,-201.90308176,0,-197.27937674,0,0,0),nrow = 8,ncol = 8,byrow = TRUE)
Co2_T_Ven <- matrix(c(0,-216.80628321,-214.70626085,0,-216.49447536,-231.68959618,
                      -216.80628321,0,0,-217.1595606,-221.72083672,-230.43145669,
                      -214.70626085,0,0,-216.72732836,-226.61746929,-226.19495454,
                      0,-217.1595606,-216.72732836,0,-226.83470136,-221.95631665,
                      -216.49447536,-221.72083672,-226.61746929,-226.83470136,0,0,
                      -231.68959618,-226.83470136,-226.19495454,-221.95631665,0,0),nrow = 6,ncol = 6,byrow = TRUE)
Ni3_T_Ven <- matrix(c(0,0,-298.29751557,-335.68108947,0,-324.11592347,
                      0,0,-322.4110902,-272.17353112,-329.85293151,0,
                      -298.29751557,-322.4110902,0,0,-349.1915688,-292.22404754,
                      -335.68108947,-272.17353112,0,0,-315.82249072,-318.12989361,
                      0,-329.85293151,-349.1915688,-315.82249072,0,0,
                      -324.11592347,0,-292.22404754,-318.12989361,0,0),nrow = 6,ncol = 6,byrow = TRUE)
Ni2_T_Ven <- matrix(c(0,0,-318.78032464,0,0,-324.57567552,0,0,
                      0,0,0,-318.77804168,-324.56922054,0,0,-312.60683406,
                      -318.78032464,0,0,-334.36205686,0,0,0,-312.72830043,
                      0,-318.77804168,-334.36205686,0,0,0,-312.72830043,0,
                      0,-324.56922054,0,0,0,-333.82254935,-327.81317111,0,
                      -324.57567552,0,0,0,-333.82254935,0,0,-327.81606021,
                      -312.59930019,0,0,-312.73620659,-327.81317111,0,0,0,
                      0,-312.60683406,-312.72830043,0,0,-327.81606021,0,0),nrow = 8,ncol = 8,byrow = TRUE)
Cu2_T_Ven <- matrix(c(0,-420.23455954,-404.48669484,0,-416.26138723,
                      -420.23455954,0,-377.89594773,-419.36752211+-384.09473309,0,
                      -404.48669484,-377.89594773,0,0,-399.6247585,
                      0,-419.36752211+-384.09473309,0,0,-377.89140041,
                      -416.26138723,0,-399.6247585,-377.89140041,0),nrow = 5,ncol = 5,byrow = TRUE)
Zn2_T_Ven <- matrix(c(0,-499.36229217,-503.7574839,-504.08365147,
                      -499.36229217,0,-503.50254574,-503.82172117,
                      -503.7574839,-503.50254574,0,-499.32219196,
                      -504.08365147,-503.82172117,-499.32219196,0),nrow = 4,ncol = 4,byrow = TRUE)
lista_matrices_T_Ven <- list("Sc3" = Sc3_T_Ven,"Sc2" = Sc2_T_Ven,"V4" = V4_T_Ven,"Ti3" = Ti3_T_Ven,"Ti2" = Ti2_T_Ven,"V3" = V3_T_Ven,"Cr3" = Cr3_T_Ven,"Mn4" = Mn4_T_Ven,"V2" = V2_T_Ven,"Cr2" = Cr2_T_Ven,"Mn3" = Mn3_T_Ven,"Fe3" = Fe3_T_Ven,"Mn2" = Mn2_T_Ven,"Fe2" = Fe2_T_Ven,"Co2" = Co2_T_Ven,"Ni3" = Ni3_T_Ven,"Ni2" = Ni2_T_Ven,"Cu2" = Cu2_T_Ven,"Zn2" = Zn2_T_Ven)
#MATRIZ DE PESO CON VALORES Vrep(r) DEL CP (3,+1).
Sc3_CP_Vrep  <- matrix(c(0,0,1682.8993277,1850.3533804,1092.9202374,-46842.283519,
                         0,0,1101.2968214,1607.6963506,938.6743237,9.2432300054,
                         1682.8993277,1101.2968214,0,40.342089621,0,0,
                         1850.3533804,1607.6963506,40.342089621,0,0,0,
                         1092.9202374,938.6743237,0,0,0,37.782529468,
                         -46842.283519,9.2432300054,0,0,37.782529468,0),nrow = 6,ncol = 6,byrow = TRUE)
Sc2_CP_Vrep <- matrix(c(0,0,28.221868101,28.198059597,28.221101666,28.197415364,
                        0,0,28.23525428,28.232123448,28.234530467,28.231552465,
                        28.221868101,28.23525428,0,26.24938481,0,0,
                        28.198059597,28.232123448,26.24938481,0,0,0,
                        28.221101666,28.234530467,0,0,0,26.249801128,
                        28.197415364,28.231552465,0,0,26.249801128,0),nrow = 6,ncol = 6,byrow = TRUE)
V4_CP_Vrep <- matrix(c(0,0,3253.3035602,3265.8124457,3286.2483009,3285.9938436,
                       0,0,3252.9943367,3266.3460816,3286.6041405,3276.4440621,
                       3253.3035602,3252.9943367,0,0,3285.7238079,3276.438658,
                       3265.8124457,3266.3460816,0,0,3274.0051212,965.6955971,
                       3286.2483009,3286.6041405,3285.7238079,3274.0051212,0,0,
                       3285.9938436,3276.4440621,3276.438658,965.6955971,0,0),nrow = 6,ncol = 6,byrow = TRUE)
Ti3_CP_Vrep <- matrix(c(0,0,43.027504116,43.025641189,36.91129502,36.925201301,
                        0,0,43.029170945,43.029762178,36.913774586,36.928138927,
                        43.027504116,43.029170945,0,0,36.949515143,36.963946537,
                        43.025641189,43.029762178,0,0,36.94927152,36.964110203,
                        36.91129502,36.913774586,36.949515143,36.94927152,0,0,
                        36.925201301,36.928138927,36.963946537,36.964110203,0,0),nrow = 6,ncol = 6,byrow = TRUE)
Ti2_CP_Vrep <- matrix(c(0,0,18.173979249,15.902392843,951.34588033,924.50304389,
                        0,0,150283707.59,18.199687042,2597.5710593,72.748129224,
                        18.173979249,150283707.59,0,0,1127.6768898,2490.0693241,
                        15.902392843,18.199687042,0,0,1128.102039,2481.2736283,
                        951.34588033,2597.5710593,1127.6768898,1128.102039,0,0,
                        924.50304389,55.876791241,150283707.59,2481.2736283,0,0),nrow = 6,ncol = 6,byrow = TRUE)
V3_CP_Vrep <- matrix(c(0,0,3262.2057151,3266.0693608,80.632799413,82.715963987,
                       0,0,3263.3874555,3267.572377,58.393985923,58.467195096,
                       3262.2057151,3263.3874555,0,0,12.08781023,11.720061237,
                       3266.0693608,3267.572377,0,0,11.678872364,12.022498109,
                       80.632799413,58.393985923,12.08781023,11.678872364,0,0,
                       82.715963987,58.467195096,11.720061237,12.022498109,0,0),nrow = 6,ncol = 6,byrow = TRUE)
Cr3_CP_Vrep <- matrix(c(0,0,0,24.761116383,0,24.720829727,79.604909507,0,
                        0,0,80.196961841,0,24.566587222,0,0,109.83088618,
                        0,80.196961841,0,24.465216805,0,0,24.764701207,0,
                        24.761116383,0,24.465216805,0,0,0,0,24.566587222,
                        0,24.566587222,0,0,0,24.460872095,24.481846835,0,
                        24.720829727,0,0,0,24.460872095,0,0,24.806215559,
                        79.604909507,0,24.764701207,0,24.481846835,0,0,0,
                        0,109.83088618,0,24.566587222,0,24.806215559,0,0),nrow = 8,ncol = 8,byrow = TRUE)
Mn4_CP_Vrep <- matrix(c(0,124.11770999,0,0,124.59982548,0,127.87549517,0,
                        124.11770999,0,0,0,0,135.47948651,0,124.95460258,
                        0,0,0,128.95460258,126.95460258,0,119.48950703,0,
                        0,0,128.95460258,0,0,127.64385503,0,125.95460258,
                        124.59982548,0,123.81420249,0,0,119.54327978,0,0,
                        0,135.47948651,0,127.64385503,119.54327978,0,0,0,
                        127.87549517,0,119.48950703,0,0,0,0,111.72565285,
                        0,124.95460258,0,133.63199791,0,0,111.72565285,0),nrow = 8,ncol = 8,byrow = TRUE)
V2_CP_Vrep <- matrix(c(0,82.522392893,0,0,90.492262451,0,82.399406223,0,
                       82.522392893,0,0,0,0,87.131751623,0,79.155809447,
                       0,0,0,84.624474027,78.176843172,0,83.757029943,0,
                       0,0,84.624474027,0,0,79.074966474,0,87.00177098,
                       90.492262451,0,78.176843172,0,0,82.522392893,0,0,
                       0,87.131751623,0,79.074966474,82.522392893,0,0,0,
                       82.399406223,0,83.757029943,0,0,0,0,83.66690709,
                       0,79.155809447,0,87.00177098,0,0,83.66690709,0),nrow = 8,ncol = 8,byrow = TRUE)
Cr2_CP_Vrep <- matrix(c(0,0,837.59248506,831.68473288,114.26457278,114.20418205,
                        0,0,66.394770313,114.263352,833.61263147,834.02849779,
                        837.59248506,66.394770313,0,3945.4529991,3945.4528522,0,
                        831.68473288,114.263352,3945.4529991,0,0,3902.03135,
                        114.26457278,833.61263147,3945.4528522,0,0,844.26699679,
                        114.20418205,834.02849779,0,3902.03135,844.26699679,0),nrow = 6,ncol = 6,byrow = TRUE)
Mn3_CP_Vrep <- matrix(c(0,0,4315.992181,4244.4130861,79.137547047,87.941259695,
                        0,0,88.464217711,128.37488297,4330.6400869,4248.1200661,
                        4315.992181,88.464217711,0,598.31427715,688.93389282,0,
                        4244.4130861,128.37488297,598.31427715,0,0,4244.9736019,
                        79.137547047,4330.6400869,688.93389282,0,0,602.61780813,
                        87.941259695,4248.1200661,0,4244.9736019,602.61780813,0),nrow = 6,ncol = 6,byrow = TRUE)
Fe3_CP_Vrep <- matrix(c(0,3036.7339905+184.61851735,180.06165949+157.38150559,0,
                        3036.7339905+184.61851735,0,0,175.8203799+187.57512921,
                        180.06165949+157.38150559,0,0,140.69994757+1808.8594659,
                        0,175.8203799+187.57512921,180.06165949+157.38150559,0),nrow = 4,ncol = 4,byrow = TRUE)
Mn2_CP_Vrep <- matrix(c(0,4743.4281099,4743.4299578+4709.8386995,0,
                        4743.4281099,0,0,4709.8289248+4743.4312167,
                        4743.4299578+4709.8386995,0,0,4709.8406371,
                        0,4709.8289248+4743.4312167,4709.8406371,0),nrow = 4,ncol = 4,byrow = TRUE)
Fe2_CP_Vrep <- matrix(c(0,153.66063254,0,0,132.4151501,0,154.11851949,0,
                        153.66063254,0,0,0,0,149.16341543,0,167.80772352,
                        0,0,0,145.57296503,22.974219787,0,154.37842318,0,
                        0,0,145.57296503,0,0,167.99900771,0,170.43419764,
                        132.4151501,0,22.974219787,0,0,538245702690,0,0,
                        0,149.16341543,0,167.99900771,538245702690,0,0,0,
                        154.11851949,0,154.37842318,0,0,0,0,22.541445964,
                        0,167.80772352,0,170.43419764,0,0,22.541445964,0),nrow = 8,ncol = 8,byrow = TRUE)
Co2_CP_Vrep <- matrix(c(0,194.03216826,194.58503283,0,5276.4305565,0,0,0,
                        194.03216826,0,0,20.943149864,0,5420.1110722,0,0,
                        194.58503283,0,0,134.44944501,0,0,5353.5363897,0,
                        0,20.943149864,134.44944501,0,0,0,0,5397.5341377,
                        5276.4305565,0,0,0,0,180.18965264,186.20059799,0,
                        0,5420.1110722,0,0,180.18965264,0,0,20.912010234,
                        0,0,5353.5363897,0,186.20059799,0,0,116.16843052,
                        0,0,0,5397.5341377,0,20.912010234,116.16843052,0),,nrow = 8,ncol = 8,byrow = TRUE)
Ni3_CP_Vrep <- matrix(c(0,0,5814.0287846,0,6358.6109326,0,183.81467248,0,
                        0,0,0,5656.5732421,0,5946.0742754,0,183.52177118,
                        5814.0287846,0,0,113.16322252,0,0,0,5167.827574,
                        0,5656.5732421,113.16322252,0,0,0,645.95375117,0,
                        6358.6109326,0,0,0,0,130.73502948,0,5770.4757144,
                        0,5946.0742754,0,0,130.73502948,0,773.45193309,0,
                        183.81467248,0,0,645.95375117,0,773.45193309,0,0,
                        0,183.52177118,5167.827574,0,5770.4757144,0,0,0),nrow = 8,ncol = 8,byrow = TRUE)
Ni2_CP_Vrep <- matrix(c(0,0,7.3078677397,0,120.3398842,0,9.5373722207,0,
                        0,0,0,102.63072773,0,120.16104074,0,9.2931225785,
                        7.3078677397,0,0,9.2751579236,0,0,0,122.29048028,
                        0,102.63072773,9.2751579236,0,0,0,10.2802888,0,
                        120.3398842,0,0,0,0,10.571076193,0,112.82094549,
                        0,120.16104074,0,0,10.571076193,0,9.2420209158,0,
                        9.5373722207,0,0,10.2802888,0,112.82094549,0,0,
                        0,9.2931225785,122.29048028,0,9.2420209158,0,0,0),nrow = 8,ncol = 8,byrow = TRUE)
Cu2_CP_Vrep <- matrix(c(0,55.093877058+278.30541663,313.41446666+8299.8787123,0,
                        278.30541663+55.093877058,0,0,6989.2252714+-154772.65336,
                        313.41446666+8299.8787123,0,0,55.125964465+313.2722868,
                        0,-154772.65336+6989.2252714,55.125964465+313.2722868,0),nrow = 4,ncol = 4,byrow = TRUE)
Zn2_CP_Vrep <- matrix(c(0,0,327.77251619+60.154839127,60.066471309+330.37828578,
                        0,0,60.869501802+9106.5656674,3125814968.5+60.905234677,
                        327.77251619+60.154839127,60.869501802+9106.5656674,0,0,
                        60.066471309+330.37828578,60.905234677+3125814968.5,0,0),nrow = 4,ncol = 4,byrow = TRUE)
lista_matrices_CP_Vrep <- list("Sc3" = Sc3_CP_Vrep,"Sc2" = Sc2_CP_Vrep,"V4" = V4_CP_Vrep,"Ti3" = Ti3_CP_Vrep,"Ti2" = Ti2_CP_Vrep,"V3" = V3_CP_Vrep,"Cr3" = Cr3_CP_Vrep,"Mn4" = Mn4_CP_Vrep,"V2" = V2_CP_Vrep,"Cr2" = Cr2_CP_Vrep,"Mn3" = Mn3_CP_Vrep,"Fe3" = Fe3_CP_Vrep,"Mn2" = Mn2_CP_Vrep,"Fe2" = Fe2_CP_Vrep,"Co2" = Co2_CP_Vrep,"Ni3" = Ni3_CP_Vrep,"Ni2" = Ni2_CP_Vrep,"Cu2" = Cu2_CP_Vrep,"Zn2" = Zn2_CP_Vrep) 
Sc3_T_Vrep <- matrix(c(0,28.076395673,28.072204577,27.966961083,
                       28.076395673,0,27.967365938,28.082407796,
                       28.072204577,27.967365938,0,28.078459619,
                       27.966961083,28.082407796,28.078459619,0),nrow = 4,ncol = 4,byrow = TRUE)
Sc2_T_Vrep <- matrix(c(0,0,0,0,27.842712805,27.805061913,
                       0,0,0,0,27.843407959,27.807359798,
                       0,0,0,0,28.041668791,28.069199564,
                       0,0,0,0,28.049272478,28.071074343,
                       27.842712805,27.843407959,28.041668791,28.049272478,0,0,
                       27.805061913,27.807359798,28.069199564,28.071074343,0,0),nrow = 6,ncol = 6,byrow = TRUE)
V4_T_Vrep <- matrix(c(0,0,1016.5840852,999.24645826,3274.5909607,3285.350834,
                      0,0,3274.5520694,3283.4153839,997.1541451,3283.4764259,
                      1016.5840852,3274.5520694,0,0,0,0,
                      999.24645826,3283.4153839,0,0,0,0,
                      3274.5909607,997.1541451,0,0,0,0,
                      3285.350834,3283.4764259,0,0,0,0),nrow = 6,ncol = 6,byrow = TRUE)
Ti3_T_Vrep <- matrix(c(0,39.392519308,0,0,0,39.893572942,39.299628078,0,
                       39.392519308,0,0,0,39.892754085,0,0,39.298454804,
                       0,0,0,39.399600187,39.296274126,0,39.892156417,0,
                       0,0,39.399600187,0,0,39.307161889,0,39.896989727,
                       0,39.892754085,39.296274126,0,0,0,0,0,
                       39.893572942,0,0,39.307161889,0,0,0,0,
                       39.299628078,0,39.892156417,0,0,0,0,0,
                       0,39.298454804,0,39.896989727,0,0,0,0),nrow = 8,ncol = 8,byrow = TRUE)
Ti2_T_Vrep  <- matrix(c(0,0,37.293303026,37.192408181,35.6163039,35.797088824,
                        0,0,35.616235148,35.800182024,37.353856083,37.154183907,
                        37.293303026,35.616235148,0,0,46.075944759,46.025750509,
                        37.192408181,35.800182024,0,0,46.142997188,46.106836254,
                        35.6163039,37.353856083,46.075944759,46.142997188,0,0,
                        35.797088824,37.154183907,46.025750509,46.106836254,0,0),nrow = 6,ncol = 6,byrow = TRUE)
V3_T_Vrep <- matrix(c(0,0,54.13106883,54.134073754,53.327059773,53.326744704,
                      0,0,53.329531198,53.325107743,54.130908471,54.134899086,
                      54.13106883,53.329531198,0,0,53.832480321,53.834048179,
                      54.134073754,53.325107743,0,0,53.834349356,53.831607898,
                      53.327059773,54.130908471,53.832480321,53.834349356,0,0,
                      53.326744704,54.134899086,53.834048179,53.325107743,0,0),nrow = 6,ncol = 6,byrow = TRUE)
Cr3_T_Vrep <- matrix(c(0,0,70.675023815,69.976497718,845.24969501,845.24969501,
                       0,0,843.08987911,809.53028656,802.3886842,844.56835677,
                       70.675023815,843.08987911,0,0,3893.9466349,3937.1760263,
                       69.976497718,809.53028656,0,0,3908.9405303,3937.3837039,
                       845.24969501,802.3886842,3893.9466349,3908.9405303,0,0,
                       70.668673668,844.56835677,3937.1760263,3937.3837039,0,0),nrow = 6,ncol = 6,byrow = TRUE)
Mn4_T_Vrep <- matrix(c(0,0,87.804530028,87.806395066,94.706312768,94.700399315,
                       0,0,94.720960756,94.727580132,87.804631979,87.837988743,
                       87.804530028,94.720960756,0,0,100.98404497,100.99582147,
                       87.806395066,94.727580132,0,0,101.02179564,101.00963152,
                       94.706312768,87.804631979,100.98404497,101.02179564,0,0,
                       94.700399315,87.837988743,100.99582147,101.00963152,0,0),nrow = 6,ncol = 6,byrow = TRUE)
V2_T_Vrep  <- matrix(c(0,0,50.922596286,50.261478039,48.86505594,48.124743949,
                       0,0,48.235928217,48.835621005,50.345370287,50.824895032,
                       50.922596286,48.235928217,0,0,66.632644587,66.30478859,
                       50.261478039,48.835621005,0,0,66.412298258,66.045369883,
                       48.86505594,50.345370287,66.632644587,66.4122982580,0,0,
                       48.124743949,50.824895032,66.30478859,66.045369883,0,0),nrow = 6,ncol = 6,byrow = TRUE)
Cr2_T_Vrep <- matrix(c(0,0,67.902271786,68.634606108,68.625226373,67.890957489,
                       0,0,68.630935782,67.89849705,67.89596218,68.625740369,
                       67.902271786,68.630935782,0,68.243686172,68.251495839,0,
                       68.634606108,67.89849705,68.243686172,0,0,68.244123814,
                       68.625226373,67.89596218,68.251495839,0,0,68.222479412,
                       67.890957489,68.625740369,0,68.244123814,68.222479412,0),nrow = 6,ncol = 6,byrow = TRUE)
Mn3_T_Vrep  <- matrix(c(0,0,81.067881893,80.931555007,83.509445467,83.377764239,
                        0,0,83.381424717,83.527995738,80.912357451,81.11400979,
                        81.067881893,83.381424717,0,0,118.05268839,-148.69032186,
                        80.931555007,83.527995738,0,0,118.08098428,-148.7116306,
                        83.509445467,80.912357451,118.05268839,118.08098428,0,0,
                        83.377764239,81.11400979,118.11042747,118.13175045,0,0),nrow = 6,ncol = 6,byrow = TRUE)
Fe3_T_Vrep <- matrix(c(0,0,123.42078433,120.64273931,120.1212084,132.17173485,
                       0,0,141.51976905,138.6399069,117.7297145,141.68279858,
                       123.42078433,141.51976905,0,0,141.89697972,0,
                       120.64273931,138.6399069,0,0,0,129.49771069,
                       120.1212084,117.7297145,141.89697972,0,0,0,
                       132.17173485,141.68279858,0,129.49771069,0,0),nrow = 6,ncol = 6,byrow = TRUE)
Mn2_T_Vrep <- matrix(c(0,0,93.964560825,107.31847668,101.99410151,0,
                       0,0,103.55766728,14.863910238,0,96.59063358,
                       93.964560825,0,0,96.852870567,101.99410151,0,
                       96.852870567,107.31847668,0,0,106.41683393,102.38017757,
                       101.99410151,0,95.157390628,106.41683393,0,0,
                       0,107.31847668,93.585093013,102.38017757,0,0),nrow = 6, ncol = 6,byrow = TRUE)
Fe2_T_Vrep <- matrix(c(0,0,151.02931933,0,143.2658531,0,142.67407732,0,
                       0,0,0,151.04433246,0,143.28170023,0,142.7700898,
                       151.02931933,0,0,159.19837726,0,0,0,152.43836858,
                       0,151.04433246,159.19837726,0,0,0,152.35294658,0,
                       143.2658531,0,0,0,0,158.48099516,0,149.23394205,
                       0,143.28170023,0,0,158.48099516,0,149.16346549,0,
                       142.67407732,0,0,152.35294658,0,149.16346549,0,0,
                       0,142.7700898,152.43836858,0,149.23394205,0,0,0),nrow = 8,ncol = 8,byrow = TRUE)
Co2_T_Vrep <- matrix(c(0,160.34764093,157.67055445,0,160.31009863,171.56602085,
                       160.34764093,0,0,158.8988832,164.74351439,170.53883778,
                       157.67055445,0,0,159.77129395,168.28567694,167.54407624,
                       0,167.54407624,159.77129395,0,168.73991308,164.22188336,
                       160.31009863,164.74351439,168.28567694,168.73991308,0,0,
                       171.56602085,170.53883778,167.54407624,164.22188336,0,0),nrow = 6,ncol = 6,byrow = TRUE)
Ni3_T_Vrep  <- matrix(c(0,0,210.3787012,244.43546422,0,238.43891078,
                        0,0,236.37848267,196.8121153,241.65408187,0,
                        210.3787012,236.37848267,0,0,254.45835593,212.86751376,
                        244.43546422,196.8121153,0,0,227.44830271,236.72605738,
                        0,241.65408187,254.45835593,227.44830271,0,0,
                        238.43891078,0,212.86751376,236.72605738,0,0),nrow = 6,ncol = 6,byrow = TRUE)
Ni2_T_Vrep  <- matrix(c(0,0,231.65062985,0,0,235.68759051,0,0,
                        0,0,0,231.64938957,235.68261577,0,0,227.28416649,
                        231.65062985,0,0,242.87016142,0,0,0,227.35771637,
                        0,231.64938957,242.87016142,0,0,0,227.3635711,0,
                        0,235.68261577,0,0,0,242.3149991,237.99166006,0,
                        235.68759051,0,0,0,242.3149991,0,0,237.99401956,
                        227.27894134,0,0,227.3635711,237.99166006,0,0,0,
                        0,227.28416649,227.35771637,0,0,237.99401956,0,0),nrow = 8,ncol = 8,byrow = TRUE) 
Cu2_T_Vrep <- matrix(c(0,301.16526893,285.25393031,0,294.81296767,
                       301.16526893,0,267.87314,296.66339747+270.55676643,0,
                       285.25393031,267.87314,0,0,285.10792,
                       0,296.66339747+270.55676643,0,0,270.32767585,
                       294.81296767,0,285.10792,270.32767585,0),nrow = 5,ncol = 5,byrow = TRUE)
Zn2_T_Vrep <- matrix(c(0,347.09339545,350.75543753,350.99622925,
                       347.09339545,0,350.51076196,350.74841415,
                       350.75543753,350.51076196,0,347.07001313,
                       350.99622925,350.74841415,347.07001313,0),nrow = 4,ncol = 4,byrow = TRUE)
lista_matrices_T_Vrep <- list("Sc3" = Sc3_T_Vrep,"Sc2" = Sc2_T_Vrep,"V4" = V4_T_Vrep,"Ti3" = Ti3_T_Vrep,"Ti2" = Ti2_T_Vrep,"V3" = V3_T_Vrep,"Cr3" = Cr3_T_Vrep,"Mn4" = Mn4_T_Vrep,"V2" = V2_T_Vrep,"Cr2" = Cr2_T_Vrep,"Mn3" = Mn3_T_Vrep,"Fe3" = Fe3_T_Vrep,"Mn2" = Mn2_T_Vrep,"Fe2" = Fe2_T_Vrep,"Co2" = Co2_T_Vrep,"Ni3" = Ni3_T_Vrep,"Ni2" = Ni2_T_Vrep,"Cu2" = Cu2_T_Vrep,"Zn2" = Zn2_T_Vrep)
#MATRIZ DE PESO CON VALORES G(r) DEL CP (3,+1).
Sc3_CP_G <- matrix(c(0,0,5115.1857104,4263.6991725,572.03323587,777483.38424,
                     0,0,574.82291668,5450.8243085,494.76580113,0.78517592114,
                     5115.1857104,574.82291668,0,8.6111501717,0,0,
                     4263.6991725,5450.8243085,8.6111501717,0,0,0,
                     572.03323587,494.76580113,0,0,0,10.845590275,
                     777483.38424,0.78517592114,0,0,10.845590275,0),nrow = 6,ncol = 6,byrow = TRUE)
Sc2_CP_G <- matrix(c(0,0,2.1365311878,2.1313353169,2.1364405056,2.1312706029,
                     0,0,2.1388707248,2.1386547884,2.1388070009,2.138614797,
                     2.1365311878,2.1388707248,0,2.2304088752,0,0,
                     2.1313353169,2.1386547884,2.2304088752,0,0,0,
                     2.1364405056,2.1388070009,0,0,0,2.230298827,
                     2.1312706029,2.138614797,0,0,2.230298827,0),nrow = 6,ncol = 6,byrow = TRUE)
V4_CP_G <- matrix(c(0,0,3257.374347,3269.3411927,3912.6265696,3933.3486775,
                    0,0,3252.694654,3274.3685352,3889.8734594,3902.6961698,
                    3257.374347,3252.694654,0,0,3938.269101,3917.2828919,
                    3269.3411927,3274.3685352,0,0,3894.0385873,600.27047345,
                    3912.6265696,3889.8734594,3938.269101,3894.0385873,0,0,
                    3933.3486775,3902.6961698,3917.2828919,600.27047345,0,0),nrow = 6,ncol = 6,byrow = TRUE)
Ti3_CP_G <- matrix(c(0,0,3.3535241631,3.3534039232,3.4474332125,3.4505894211,
                     0,0,3.3540637999,3.354101877,3.4480165804,3.4512018036,
                     3.3535241631,3.3540637999,0,0,3.448061738,3.4511106184,
                     3.3534039232,3.354101877,0,0,3.4480251651,3.4510956028,
                     3.4474332125,3.4480165804,3.448061738,3.4480251651,0,0,
                     3.4505894211,3.4512018036,3.4511106184,3.4510956028,0,0),nrow = 6,ncol = 6,byrow = TRUE)
Ti2_CP_G  <- matrix(c(0,0,1.3427765944,1.49613985,525.3193465,528.66772035,
                       0,0,642604.52363,1.3394176813,3694.8646789,15.079883826,
                       1.3427765944,642604.52363,0,0,633.77325305,4513.3311286,
                       1.49613985,1.3394176813,0,0,633.96358979,4517.5026349,
                       525.3193465,3694.8646789,633.77325305,633.96358979,0,0,
                       528.66772035,15.079883826,642604.52363,4517.5026349,0,0),nrow = 6,ncol = 6,byrow = TRUE)
#MATRIZ DE PESO CON VALORES K(r) DEL CP (3,+1).
Sc3_CP_K <- matrix(c(0,0,3710.995023,3487.5420536,611.5045452,3998998.7563,
                     0,0,619.01761232,3781.6795239,472.01830473,0.48945605933,
                     3710.995023,619.01761232,0,4.6161553417,0,0,
                     3487.5420536,3781.6795239,4.6161553417,0,0,0,
                     611.5045452,472.01830473,0,0,0,3.7339302953,
                     3998998.7563,0.48945605933,0,0,3.7339302953,0),nrow = 6,ncol = 6,byrow = TRUE)
Sc2_CP_K <- matrix(c(0,0,3.5985939259,3.5933709707,3.5984260852,3.5932358384,
                     0,0,3.6008423375,3.6006020088,3.6006989608,3.6004973497,
                     3.5985939259,3.6008423375,0,3.3100635896,0,0,
                     3.5933709707,3.6006020088,3.3100635896,0,0,0,
                     3.5984260852,3.6006989608,0,0,0,3.3101040354,
                     3.5932358384,3.6004973497,0,0,3.3101040354,0),nrow = 6,ncol = 6,byrow = TRUE)
V4_CP_K <- matrix(c(0,0,4084.0426246,4104.8642458,4571.795957,4585.4731506,
                    0,0,4080.3728158,4109.0079186,4556.8587325,4555.2669657,
                    4084.0426246,4080.3728158,0,0,4588.4663791,4565.0441315,
                    4104.8642458,4109.0079186,0,0,4547.2633632,391.8501296,
                    4571.795957,4556.8587325,4588.4663791,4547.2633632,0,0,
                    4585.4731506,4555.2669657,4565.0441315,391.8501296,0,0),nrow = 6,ncol = 6,byrow = TRUE)
Ti3_CP_K  <- matrix(c(0,0,6.0403504806,6.040004507,4.9140967432,4.9171514924,
                      0,0,6.0407727482,6.0408691205,4.9146600076,4.9177843302,
                      6.0403504806,6.0407727482,0,0,4.9212763345,4.9243878535,
                      6.040004507,6.0408691205,0,0,4.9212195572,4.9244339621,
                      4.9140967432,4.9146600076,4.9212763345,4.9212195572,0,0,
                      4.9171514924,4.9177843302,4.9243878535,4.9244339621,0,0),nrow = 6,ncol = 6,byrow = TRUE)
Ti2_CP_K  <- matrix(c(0,0,1.3642917833,1.11175558,425.2967535,408.12136007,
                      0,0,688176601.48,1.3671251833,3901.3198957,11.246987706,
                      1.3642917833,688176601.48,0,0,573.34813317,4256.4404997,
                      1.11175558,1.3671251833,0,0,573.70079815,4250.986336,
                      425.2967535,3901.3198957,573.34813317,573.70079815,0,0,
                      408.12136007,11.246987706,688176601.48,4250.986336,0,0),nrow = 6,ncol = 6,byrow = TRUE)
#MATRIZ DE PESO CON VALORES L(r) DEL CP (3,+1).
Sc3_CP_L <- matrix(c(0,0,-1404.1906874,-776.15711888,39.471309334,3221515.3721,
                     0,0,44.194695639,-1669.1447847,-22.747496394,-0.29571986181,
                     -1404.1906874,44.194695639,0,-3.99499483,0,0,
                     -776.15711888,-1669.1447847,-3.99499483,0,0,0,
                     39.471309334,-22.747496394,0,0,0,-7.1116599794,
                     3221515.3721,-0.29571986181,0,0,-7.1116599794,0),,nrow = 6,ncol = 6,byrow = TRUE)
Sc2_CP_L <- matrix(c(0,0,1.4620627381,1.4620356538,1.4619855796,1.4619652355,
                     0,0,1.4619716127,1.4619472203,1.4618919599,1.4618825527,
                     1.4620627381,1.4619716127,0,1.0796547144,0,0,
                     1.4620356538,1.4619472203,1.0796547144,0,0,0,
                     1.4619855796,1.4618919599,0,0,0,1.0798052084,
                     1.4619652355,1.4618825527,0,0,1.0798052084,0),nrow = 6,ncol = 6,byrow = TRUE)
V4_CP_L <- matrix(c(0,0,826.66827762,835.52305307,659.16938734,652.1244731,
                    0,0,827.67816179,834.63938344,666.98527307,652.5707959,
                    826.66827762,827.67816179,0,0,650.19727806,647.76123959,
                    835.52305307,834.63938344,0,0,653.22477593,-208.42034385,
                    659.16938734,666.98527307,650.19727806,653.22477593,0,0,
                    652.1244731,652.5707959,647.76123959,-208.42034385,0,0),nrow = 6,ncol = 6,byrow = TRUE)
Ti3_CP_L <- matrix(c(0,0,2.6868263175,2.6866005838,1.4666635307,1.4665620713,
                     0,0,2.6867089483,2.6867672435,1.4666434272,1.4665825267,
                     2.6868263175,2.6867089483,0,0,1.4732145965,1.473277235,
                     2.6866005838,2.6867672435,0,0,1.4731943921,1.4733383593,
                     1.4666635307,1.4666434272,1.4732145965,1.4731943921,0,0,
                     1.4665620713,1.4665825267,1.473277235,1.4733383593,0,0),nrow = 6,ncol = 6,byrow = TRUE)
Ti2_CP_L <- matrix(c(0,0,0.02151518892,-0.38438426996,-100.022593,-120.54636028,
                     0,0,688176601.48,0.02770750199,206.45521676,-3.8328961197,
                     0.02151518892,687533996.96,0,0,-60.425119875,-256.89062898,
                     -0.38438426996,0.02770750199,0,0,-60.262791647,-266.51629889,
                     -100.022593,206.45521676,-60.425119875,-60.262791647,0,0,
                     -120.54636028,-3.8328961197,687533996.96,-266.51629889,0,0),nrow = 6,ncol = 6,byrow = TRUE)

# MATRIZ DE PESO CU-PHEN --------------------------------------------------
#DistFronNuc
matriz_s0_r <- matrix(c(0,0.535825,0.535822,0.533382,
                        0.535825,0,0.533381,0.535823,
                        0.535822,0.533381,0,0.535878,
                        0.533382,0.535823,0.535878,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s1_r <- matrix(c(0,0,0.533165,0.534572,0.534912,0.532778,
                        0,0,0.534572,0.533163,0.532773,0.534915,
                        0.533165,0.534572,0,0,0.524234,0,
                        0.534572,0.533163,0,0,0,0.524233,
                        0.534912,0.532773,0.524234,0,0,0,
                        0.532778,0.534915,0,0.524233,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s2_r  <- matrix(c(0,0,0.533098,0.534718,0.534725,0.533091,
                         0,0,0.5348,0.532909,0.532897,0.53481,
                         0.533098,0.5348,0,0,0.524387,0,
                         0.534718,0.532909,0,0,0,0.524385,
                         0.534725,0.532897,0.524387,0,0,0,
                         0.533091,0.53481,0,0.524385,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s3_r <- matrix(c(0,0,0.534612+0.533695,0.53335+0.534512,
                        0,0,0.534509+0.533348,0.533689+0.534611,
                        0.534612+0.533695,0.533348+0.534509,0,0,
                        0.53335+0.534512,0.533689+0.534611,0,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s4_r  <- matrix(c(0,0,0.533395,0.533457,
                         0,0,0.533385,0.53346,
                         0.533395,0.533385,0,0.533853+0.533854,
                         0.533457,0.53346,0.533853+0.533854,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s5_r <- matrix(c(0,0,0.537897,0.537657,
                        0,0,0.537667,0.537906,
                        0.537897,0.537667,0,0.531171+0.531162,
                        0.537657,0.537906,0.531171+0.531162,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s6_r <- matrix(c(0,0,0.537568,0.537745,
                        0,0,0.537731,0.53756,
                        0.537568,0.537731,0,0.531258+0.531271,
                        0.537745,0.53756,0.531258+0.531271,0),nrow = 4,ncol = ,byrow = TRUE)
matriz_s7_r <- matrix(c(0,0,0.533989,0.534191,0,0.535193,
                        0,0,0,0.533138,0,0.533096,
                        0.533989,0,0,0,0.534026,0,
                        0.534191,0.533138,0,0,0.535136,0,
                        0,0,0.534026,0.535136,0,0.534187,
                        0.535193,0.533096,0,0,0.534187,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s8_r <- matrix(c(0,0.534091,0,0,0.534822,0.534576,
                        0.534091,0,0,0.534159,0,0,
                        0,0,0,0,0.533229,0.533169,
                        0,0.534159,0,0,0.534515,0.534812,
                        0.534822,0,0.533229,0.534515,0,0,
                        0.534576,0,0.533169,0.534812,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s9_r <- matrix(c(0,0,0.537459,0.537421,
                        0,0,0.537459,0.537408,
                        0.537459,0.537459,0,0.531287+0.531287,
                        0.537421,0.537408,0.531287+0.531287,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s10_r <- matrix(c(0,0.535572+0.535574,0.535506+0.535603,0,0,
                         0.535572+0.535574,0,0,0.53309,0.533213,
                         0.535506+0.535603,0,0,0.533223,0.533264,
                         0,0.53309,0.533223,0,0.52717,
                         0,0.533213,0.533264,0.52717,0),nrow = 5,ncol = 5,byrow = TRUE)
matriz_s11_r <- matrix(c(0,0.535521+0.535596,0.535563+0.535586,0,0,
                         0.535521+0.535596,0,0,0.533226,0.53326,
                         0.535563+0.535586,0,0,0.53309,0.533221,
                         0,0.533226,0.53309,0,0.527187,
                         0,0.53326,0.533221,0.527187,0),nrow = 5,ncol = 5,byrow = TRUE)
matriz_s12_r <- matrix(c(0,0,0.535772,0.535596,
                         0,0,0.53531,0.535522,
                         0.535772,0.53531,0,0.532818+0.532821,
                         0.535596,0.535522,0.532818+0.532821,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s13_r  <- matrix(c(0,0,0.53727,0.537081,
                          0,0,0.537686,0.537869,
                          0.53727,0.537686,0,0.531373+0.531372,
                          0.537081,0.537869,0.531372+0.531373,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s14_r <- matrix(c(0,0.536741,0,0.536548,
                         0.536741,0,0.538795,0.531222+0.531163,
                         0,0.538795,0,0.538724,
                         0.536548,0.531222+0.531163,0.538724,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s15_r <- matrix(c(0,0,0.53561,0.536657,
                         0,0,0.534802,0.535388,
                         0.53561,0.534802,0,0.532511+0.532604,
                         0.536657,0.535388,0.532511+0.532604,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s16_r <- matrix(c(0,0.532755,0.535958,0,0.533112,0,
                         0.532755,0,0,0.535957,0,0.533112,
                         0.535958,0,0,0,0,0.535839,
                         0,0.535957,0,0,0.535851,0,
                         0.533112,0,0,0.535851,0,0.53283,
                         0,0.533112,0.535839,0,0.53283,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s17_r <- matrix(c(0,0.536292+0.536142,0.535096+0.53504,0.534988+0.53507,
                         0.536292+0.536142,0,0.537766,0,
                         0.535096+0.53504,0.537766,0,0,
                         0.534988+0.53507,0,0,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s18_r <- matrix(c(0,0.536228+0.536095,0.535383+0.53474,0.535412+0.534791,
                         0.536228+0.536095,0,0,0,
                         0.535383+0.53474,0,0,0,
                         0.535412+0.534791,0,0,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s19_r <- matrix(c(0,0,0.53164,0.53321,0.533277,0.531574,
                         0,0,0.533208,0.531642,0.531574,0.533278,
                         0.53164,0.533208,0,0.52498,0,0,
                         0.53321,0.531642,0.52498,0,0,0,
                         0.533277,0.531574,0,0,0,0.525,
                         0.531574,0.533278,0,0,0.525,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s20_r <- matrix(c(0,0.535572+0.535574,0.535603+0.535506,0,0,
                         0.535572+0.535574,0,0,0.53309,0.533213,
                         0.535603+0.535506,0,0,0.533223,0.533264,
                         0,0.53309,0.533223,0,0.52717,
                         0,0.533213,0.533264,0.52717,0),nrow = 5,ncol = 5,byrow = TRUE)
lista_matriz_Cu_phen_r <- list("s0" = matriz_s0_r,"s1" = matriz_s1_r,"s2" = matriz_s2_r,"s3" = matriz_s3_r,"s4" = matriz_s4_r,"s5" = matriz_s5_r,"s6" = matriz_s6_r,"s7" = matriz_s7_r,"s8" = matriz_s8_r,"s9" = matriz_s9_r,"s10" = matriz_s10_r,"s11" = matriz_s11_r,"s12" = matriz_s12_r,"s13" = matriz_s13_r,"s14" = matriz_s14_r,"s15" = matriz_s15_r,"s16" = matriz_s16_r,"s17" = matriz_s17_r,"s18" = matriz_s18_r,"s19" = matriz_s19_r,"s20" = matriz_s20_r)
#Rho
matriz_s0_Rho <- matrix(c(0,6.046059,6.046361,6.15196,
                          6.046059,0,6.152003,6.046294,
                          6.046361,6.152003,0,6.043163,
                          6.15196,6.046294,6.043163,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s1_Rho <- matrix(c(0,0,6.233386,6.179384,6.169013,6.247859,
                          0,0,6.179401,6.233455,6.248098,6.16886,
                          6.233386,6.179401,0,0,6.52977,0,
                          6.179384,6.233455,0,0,0,6.529841,
                          6.169013,6.248098,6.52977,0,0,0,
                          6.247859,6.16886,0,6.529841,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s2_Rho  <- matrix(c(0,0,6.233346,6.174116,6.173929,6.233586,
                           0,0,6.170182,6.243026,6.243507,6.169839,
                           6.233346,6.170182,0,0,6.523285,0,
                           6.174116,6.243026,0,0,0,6.523363,
                           6.173929,6.243507,6.523285,0,0,0,
                           6.233586,6.169839,0,6.523363,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s3_Rho <- matrix(c(0,0,6.181162+6.197621,6.209109+6.176249,
                          0,0,6.176365+6.209199,6.197881+6.181205,
                          6.181162+6.197621,6.209199+6.176365,0,0,
                          6.209109+6.176249,6.197881+6.181205,0,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s4_Rho <- matrix(c(0,0,6.212887,6.206684,
                          0,0,6.213217,6.206715,
                          6.212887,6.213217,0,6.229608+6.229584,
                          6.206684,6.206715,6.229608+6.229584,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s5_Rho <- matrix(c(0,0,5.9261,5.934844,
                          0,0,5.934438,5.92556,
                          6.290116,6.290307,0,6.290116+6.290307,
                          5.934844,5.92556,6.290116+6.290307,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s6_Rho <- matrix(c(0,0,5.937472,5.931533,
                          0,0,5.932134,5.937991,
                          5.937472,5.932134,0,6.279854+6.279459,
                          5.931533,5.937991,6.279854+6.279459,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s7_Rho <- matrix(c(0,0,6.115501,6.11399,0,6.085581,
                          0,0,0,6.160865,0,6.163056,
                          6.115501,0,0,0,6.113662,0,
                          6.11399,6.160865,0,0,6.088599,0,
                          0,0,6.113662,6.088599,0,6.114084,
                          6.085581,6.163056,0,0,6.114084,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s8_Rho <- matrix(c(0,6.109049,0,0,6.096843,6.101937,
                          6.109049,0,0,6.105027,0,0,
                          0,0,0,0,6.154654,6.158261,
                          0,6.105027,0,0,6.105216,6.097213,
                          6.096843,0,6.154654,6.105216,0,0,
                          6.101937,0,6.158261,6.097213,0,0),nrow = 6,ncol = 6,byrow = TRUE) 
matriz_s9_Rho <- matrix(c(0,0,5.947463,5.945009,
                          0,0,5.947554,5.945564,
                          5.947463,5.947554,0,6.284725+6.284717,
                          5.945009,5.945564,6.284725+6.284717,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s10_Rho <- matrix(c(0,6.100934+6.101719,6.104459+6.099125,0,0,
                           6.100934+6.101719,0,0,6.225481,6.218132,
                           6.104459+6.099125,0,0,6.21859,6.216669,
                           0,6.225481,6.21859,0,6.412587,
                           0,6.218132,6.216669,6.412587,0),nrow = 5,ncol = 5,byrow = TRUE)
matriz_s11_Rho <- matrix(c(0,6.103628+6.099337,6.101171+6.101041,0,0,
                           6.103628+6.099337,0,0,6.218454,6.21661,
                           6.101171+6.101041,0,0,6.225275,6.217728,
                           0,6.218454,6.225275,0,6.411857,
                           0,6.21661,6.217728,6.411857,0),nrow = 5,ncol = 5,byrow = TRUE)
matriz_s12_Rho <- matrix(c(0,0,6.048922,6.054141,
                           0,0,6.067772,6.060912,
                           6.048922,6.067772,0,6.233825+6.233353,
                           6.054141,6.060912,6.233825+6.233353,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s13_Rho  <- matrix(c(0,0,5.956184,5.962707,
                            0,0,5.931602,5.92488,
                            5.956184,5.931602,0,6.273111+6.272867,
                            5.962707,5.92488,6.273111+6.272867,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s14_Rho <- matrix(c(0,5.984268,0,5.989962,
                           5.984268,0,5.881778,6.288265+6.290894,
                           0,5.881778,0,5.877904,
                           5.989962,6.288265+6.290894,5.877904,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s15_Rho <- matrix(c(0,0,6.057862,6.000525,
                           0,0,6.097913,6.058357,
                           6.057862,6.097913,0,6.246358+6.250684,
                           6.000525,6.058357,6.246358+6.250684,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s16_Rho <- matrix(c(0,6.19344,6.01458,0,6.185075,0,
                           6.19344,0,0,6.014566,0,6.185046,
                           6.01458,0,0,0,0,6.024663,
                           0,6.014566,0,0,6.024093,0,
                           6.185075,0,0,6.024093,0,6.190134,
                           0,6.185046,6.024663,0,6.190134,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s17_Rho <- matrix(c(0,6.030574+6.035604,6.085912+6.086074,6.088364+6.086718,
                           6.030574+6.035604,0,5.920557,0,
                           6.085912+6.086074,5.920557,0,0,
                           6.088364+6.086718,0,0,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s18_Rho <- matrix(c(0,6.03416+6.038565,6.077511+6.093978,6.076571+6.091705,
                           6.03416+6.038565,0,0,0,
                           6.077511+6.093978,0,0,0,
                           6.076571+6.091705,0,0,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s19_Rho <- matrix(c(0,0,6.316036,6.258508,6.254649,6.319598,
                           0,0,6.258551,6.315892,6.319686,6.254644,
                           6.316036,6.258551,0,6.482466,0,0,
                           6.258508,6.315892,6.482466,0,0,0,
                           6.254649,6.319686,0,0,0,6.48109,
                           6.319598,6.254644,0,0,6.48109,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s20_Rho <- matrix(c(0,6.100934+6.101719,6.099125+6.104459,0,0,
                           6.100934+6.101719,0,0,6.225481,6.218132,
                           6.099125+6.104459,0,0,6.21859,6.216669,
                           0,6.225481,6.21859,0,6.412587,
                           0,6.218132,6.216669,6.412587,0),nrow = 5,ncol = 5,byrow = TRUE)
lista_matriz_Cu_phen_Rho <- list("s0" = matriz_s0_Rho,"s1" = matriz_s1_Rho,"s2" = matriz_s2_Rho,"s3" = matriz_s3_Rho,"s4" = matriz_s4_Rho,"s5" = matriz_s5_Rho,"s6" = matriz_s6_Rho,"s7" = matriz_s7_Rho,"s8" = matriz_s8_Rho,"s9" = matriz_s9_Rho,"s10" = matriz_s10_Rho,"s11" = matriz_s11_Rho,"s12" = matriz_s12_Rho,"s13" = matriz_s13_Rho,"s14" = matriz_s14_Rho,"s15" = matriz_s15_Rho,"s16" = matriz_s16_Rho,"s17" = matriz_s17_Rho,"s18" = matriz_s18_Rho,"s19" = matriz_s19_Rho,"s20" = matriz_s20_Rho)
#DelSqRho
matriz_s0_DelSqRho <- matrix(c(0,-63.585883,-63.582205,-68.201639,
                               -63.585883,0,-68.203375,-63.579769,
                               -63.582205,-68.203375,0,-63.47623,
                               -68.201639,-63.579769,-63.47623,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s1_DelSqRho <- matrix(c(0,0,-67.767033,-65.246864,-64.372743,-68.758811,
                               0,0,-65.247751,-67.769573,-68.768621,-64.3662,
                               -67.767033,-65.247751,0,0,-92.950914,0,
                               -65.246864,-67.769573,0,0,0,-92.955062,
                               -64.372743,-68.768621,-92.950914,0,0,0,
                               -68.758811,-64.3662,0,-92.955062,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s2_DelSqRho <- matrix(c(0,0,-68.001856,-64.883454,-64.865393,-68.021125,
                               0,0,-64.703499,-68.398132,-68.427502,-64.678362,
                               -68.001856,-64.703499,0,0,-92.504116,0,
                               -64.883454,-68.398132,0,0,0,-92.508857,
                               -64.865393,-68.427502,-92.504116,0,0,0,
                               -68.021125,-64.678362,0,-92.508857,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s3_DelSqRho <- matrix(c(0,0,-65.073502+-66.865436,-67.693696+-65.432016,
                               0,0,-65.43766+-67.696473,-66.876247+-65.078185,
                               -65.073502+-66.865436,-67.696473-65.43766,0,0,
                               -67.693696+-65.432016,-66.876247+-65.078185,0,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s4_DelSqRho <- matrix(c(0,0,-67.503778,-67.283017,
                               0,0,-67.526006,-67.273283,
                               -67.503778,-67.526006,0,-66.621848+-66.620106,
                               -67.503778,-67.273283,-66.621848+-66.620106,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s5_DelSqRho <- matrix(c(0,0,-57.359117,-57.882853,
                               0,0,-57.86426,-57.338462,
                               -57.359117,-57.86426,0,-74.037458+-74.058138,
                               -57.882853,-57.338462,-74.037458+-74.058138,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s6_DelSqRho <- matrix(c(0,0,-58.048216,-57.645266,
                               0,0,-57.672979,-58.067005,
                               -58.048216,-57.672979,0,-73.933105+-73.904014,
                               -57.645266,-58.067005,-73.933105+-73.904014,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s7_DelSqRho <- matrix(c(0,0,-66.038304,-67.253615,0,-64.692287,
                               0,0,0,-68.026741,0,-68.092833,
                               -66.038304,0,0,0,-65.986201,0,
                               -67.253615,-68.026741,0,0,-64.809423,0,
                               0,0,-65.986201,-64.809423,0,-67.264227,
                               -64.692287,-68.092833,0,0,-67.264227,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s8_DelSqRho <- matrix(c(0,-65.74111,0,0,-65.614613,-66.29212,
                               -65.74111,0,0,-65.630339,0,0,
                               0,0,0,0,-67.786595,-67.884366,
                               0,-65.630339,0,0,-66.420288,-65.637466,
                               -65.614613,0,-67.786595,-66.420288,0,0,
                               -66.29212,0,-67.884366,-65.637466,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s9_DelSqRho <- matrix(c(0,0,-58.312668,-58.266366,
                               0,0,-58.312071,-58.293756,
                               -58.312668,-58.312071,0,-73.739477+-73.738917,
                               -58.266366,-58.293756,-73.739477+-73.738917,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s10_DelSqRho <- matrix(c(0,-63.355278+-63.344028,-63.513221+-63.277941,0,0,
                                -63.355278+-63.344028,0,0,-68.507082,-68.200931,
                                -63.513221+-63.277941,0,0,-68.195422,-68.117348,
                                0,-68.507082,-68.195422,0,-84.728045,
                                0,-68.200931,-68.117348,-84.728045,0),nrow = 5,ncol = 5,byrow = TRUE)
matriz_s11_DelSqRho <- matrix(c(0,-63.482596+-63.293888,-63.375089+-63.321558,0,0,
                                -63.482596+-63.293888,0,0,-68.193343,-68.127455,
                                -63.375089+-63.321558,0,0,-68.507465,-68.186998,
                                0,-68.193343,-68.507465,0,-84.68163,
                                0,-68.127455,-68.186998,-84.68163,0),nrow = 5,ncol = 5,byrow = TRUE)
matriz_s12_DelSqRho <- matrix(c(0,0,-62.450405,-62.78908,
                                0,0,-63.349531,-62.904462,
                                -62.450405,-63.349531,0,-69.745699+-69.737987,
                                -62.78908,-62.904462,-69.745699+-69.737987,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s13_DelSqRho <- matrix(c(0,0,-58.664951,-59.088849,
                                0,0,-57.794173,-57.374824,
                                -58.664951,-57.794173,0,-73.596196+-73.538455,
                                -59.088849,-57.374824,-73.596196+-73.538455,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s14_DelSqRho <- matrix(c(0,-59.729824,0,-60.156127,
                                -59.729824,0,-55.647129,-73.708304+-73.842131,
                                0,-55.647129,0,-55.481926,
                                -60.156127,-73.708304+-73.842131,-55.481926,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s15_DelSqRho <- matrix(c(0,0,-62.875902,-60.477836,
                                0,0,-64.392743,-62.937933,
                                -62.875902,-64.392743,0,-70.192961+-70.394789,
                                -60.477836,-62.937933,-70.394789+-70.192961,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s16_DelSqRho <- matrix(c(0,-70.449181,-61.686032,0,-69.481964,0,
                                -70.449181,0,0,-61.686686,0,-69.480199,
                                -61.686032,0,0,0,0,-61.840057,
                                0,-61.686686,0,0,-61.814112,0,
                                -69.481964,0,0,-61.814112,0,-70.27459,
                                0,-69.480199,-61.840057,0,-70.27459,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s17_DelSqRho <- matrix(c(0,-62.436908+-62.776448,-64.841441+-65.024562,-65.131778+-64.893809,
                                -62.436908+-62.776448,0,-57.602692,0,
                                -64.841441+-65.024562,-57.602692,0,0,
                                -65.131778+-64.893809,0,0,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s18_DelSqRho <- matrix(c(0,-62.570427+-62.87203,-64.104426+-65.776156,-64.047515+-65.670153,
                                -62.570427+-62.87203,0,0,0,
                                -64.104426+-65.776156,0,0,0,
                                -64.047515+-65.670153,0,0,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s19_DelSqRho <- matrix(c(0,0,-70.620367,-67.61534,-67.492647,-70.736329,
                                0,0,-67.620113,-70.617062,-70.735239,-67.491956,
                                -70.620367,-67.620113,0,-91.92166,0,0,
                                -67.61534,-70.617062,-91.92166,0,0,0,
                                -67.492647,-70.735239,0,0,0,-91.870958,
                                -70.736329,-67.491956,0,0,-91.870958,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s20_DelSqRho <- matrix(c(0,-63.355278+-63.344028,-63.277941+-63.513221,0,0,
                                -63.355278+-63.344028,0,0,-68.195422,-68.200931,
                                -63.277941+-63.513221,0,0,-68.195422,-68.117348,
                                0,-68.507082,-68.195422,0,-84.728045,
                                0,-68.200931,-68.117348,-84.728045,0),nrow = 5,ncol = 5,byrow = TRUE)
lista_matriz_Cu_phen_DelSqRho <- list("s0" = matriz_s0_DelSqRho,"s1" = matriz_s1_DelSqRho,"s2" = matriz_s2_DelSqRho,"s3" = matriz_s3_DelSqRho,"s4" = matriz_s4_DelSqRho,"s5" = matriz_s5_DelSqRho,"s6" = matriz_s6_DelSqRho,"s7" = matriz_s7_DelSqRho,"s8" = matriz_s8_DelSqRho,"s9" = matriz_s9_DelSqRho,"s10" = matriz_s10_DelSqRho,"s11" = matriz_s11_DelSqRho,"s12" = matriz_s12_DelSqRho,"s13" = matriz_s13_DelSqRho,"s14" = matriz_s14_DelSqRho,"s15" = matriz_s15_DelSqRho,"s16" = matriz_s16_DelSqRho,"s17" = matriz_s17_DelSqRho,"s18" = matriz_s18_DelSqRho,"s19" = matriz_s19_DelSqRho,"s20" = matriz_s20_DelSqRho)
#Ven(r)
matriz_s0_Ven <- matrix(c(0,-499.14117436,-499.09538006,-510.30003634,
                          -499.14117436,0,-510.30418234,-499.08624381,
                          -499.09538006,-510.30418234,0,-498.70642685,
                          -510.30003634,-499.08624381,-498.70642685,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s1_Ven <- matrix(c(0,0,-516.44155763,-510.98188907,-509.76060631,-517.75711536,
                          0,0,-510.98173264,-516.44738674,-517.77919162,-509.74409157,
                          -516.44155763,-510.98173264,0,0,-547.80988597,0,
                          -510.98188907,-516.44738674,0,0,0,-547.81821049,
                          -509.76060631,-517.77919162,-547.80988597,0,0,0,
                          -517.75711536,-509.74409157,0,-547.81821049,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s2_Ven <- matrix(c(0,0,-516.45071208,-510.42500748,-510.4023516,-516.47215181,
                          0,0,-509.96644172,-517.31042352,-517.35599035,-509.92905713,
                          -516.45071208,-509.96644172,0,0,-547.16217956,0,
                          -510.42500748,-517.31042352,0,0,0,-547.16857877,
                          -510.4023516,-517.35599035,-547.16217956,0,0,0,
                          -516.47215181,-509.92905713,0,-547.16857877,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s3_Ven <- matrix(c(0,0,-510.92284749+-513.15339933,-514.19811645+-510.76317356,
                          0,0,-510.77509037+-514.20620183,-513.17845805+-510.92814274,
                          -510.92284749+-513.15339933,-514.20620183+-510.77509037,0,0,
                          -514.19811645+-510.76317356,-513.17845805+-510.92814274,0,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s4_Ven <- matrix(c(0,0,-514.57283286,-514.01761021,
                          0,0,-514.60492077,-514.01964839,
                          -514.57283286,-514.60492077,0,-515.46352821+-515.46305412,
                          -514.01761021,-514.01964839,-515.46352821+-515.46305412,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s5_Ven <- matrix(c(0,0,-488.44088909,-489.21379458,
                          0,0,-489.17426636,-488.39084961,
                          -488.44088909,-489.17426636,0,-522.10817189+-522.29966998,
                          -489.21379458,-488.39084961,-522.10817189+-522.29966998,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s6_Ven <- matrix(c(0,0,-489.50722603,-488.98756611,
                          0,0,-489.0433728,-489.55599741,
                          -489.50722603,-489.0433728,0,-521.36871744+-521.1589787,
                          -488.98756611,-489.55599741,-521.36871744+-521.1589787,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s7_Ven <- matrix(c(0,0,-506.41561383,-505.69140827,0,-502.62861174,
                          0,0,0,-510.77322405,0,-510.89669423,
                          -506.41561383,0,0,0,-506.32396789,0,
                          -505.69140827,-510.77322405,0,0,-503.07828739,0,
                          0,0,-506.32396789,-503.07828739,0,-505.69630362,
                          -502.62861174,-510.89669423,0,0,-505.69630362,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s8_Ven <- matrix(c(0,-505.85040193,0,0,-503.87964915,-504.36177626,
                          -505.85040193,0,0,-505.53519848,0,0,
                          0,0,0,0,-510.20852051,-510.48357653,
                          0,-505.53519848,0,0,-504.8355157,-503.91546595,
                          -503.87964915,0,-510.20852051,-504.8355157,0,0,
                          -504.36177626,0,-510.48357653,-503.91546595,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s9_Ven <- matrix(c(0,0,-490.42427606,-490.23107912,
                          0,0,-490.43454252,-490.28208551,
                          -490.42427606,-490.43454252,0,-521.67980663+-521.67649414,
                          -490.23107912,-490.28208551,-521.67980663+-521.67649414,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s10_Ven  <- matrix(c(0,-503.82248701+-503.97685382,-504.10650051+-503.70702157,0,0,
                            -503.82248701+-503.97685382,0,0,-515.79990912,-515.02751032,
                            -504.10650051+-503.70702157,0,0,-515.08544306,-514.80036485,
                            0,-515.79990912,-515.08544306,0,-536.01101882,
                            0,-515.02751032,-514.80036485,-536.01101882,0),nrow = 5,ncol = 5,byrow = TRUE)
matriz_s11_Ven <- matrix(c(0,-504.02676937+-503.73297733,-503.84683524+-503.91416289,0,0,
                           -504.026769376+-503.73297733,0,0,-515.07390732,-514.79674131,
                           -503.84683524+-503.91416289,0,0,-515.78304669,-514.98678814,
                           0,-515.07390732,-515.78304669,0,-535.93914264,
                           0,-514.79674131,-514.98678814,-535.93914264,0),nrow = 5,ncol =5,byrow = TRUE)
matriz_s12_Ven <- matrix(c(0,0,-499.76238692,-500.19361217,
                           0,0,-501.50755515,-500.91478306,
                           -499.76238692,-501.50755515,0,-516.38222326+-516.50781433,
                           -500.19361217,-500.91478306,-516.38222326+-516.50781433,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s13_Ven <- matrix(c(0,0,-491.29836575,-491.87983808,
                           0,0,-488.95798708,-488.36642586,
                           -491.29836575,-488.95798708,0,-520.5754234+-520.71540574,
                           -491.87983808,-488.36642586,-520.5754234+-520.71540574,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s14_Ven <- matrix(c(0,-493.92030272,0,-494.4497458,
                           -493.92030272,0,-484.21079042,-522.07560347+-522.23836545,
                           0,-484.21079042,0,-483.94158933,
                           -494.4497458,-522.07560347+-522.23836545,-483.94158933,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s15_Ven  <- matrix(c(0,0,-500.53775856,-495.17122987,
                            0,0,-504.38274165,-500.75183231,
                            -500.53775856,-504.38274165,0,-518.07668839+-517.63612885,
                            -495.17122987,-500.75183231,-518.07668839+-517.63612885,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s16_Ven <- matrix(c(0,-513.24306598,-496.89856924,0,-512.25588267,0,
                           -513.24306598,0,0,-496.89785807,0,-512.24950003,
                           -496.89856924,0,0,0,0,-497.78308105,
                           0,-496.89785807,0,0,-497.72939843,0,
                           -512.25588267,0,0,-497.72939843,0,-512.75397106,
                           0,-512.24950003,-497.78308105,0,-512.75397106,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s17_Ven <- matrix(c(0,-497.66774432+-498.290747,-502.81865693+-502.76837186,-503.1545866+-502.88735186,
                           -497.66774432+-498.290747,0,-488.06631394,0,
                           -502.81865693+-502.76837186,-488.06631394,0,0,
                           -503.1545866+-502.88735186,0,0,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s18_Ven <- matrix(c(0,-498.00322974+-498.55675121,-501.93909484+-503.7709571,-501.85050502+-503.38695726,
                           -498.00322974+-498.55675121,0,0,0,
                           -501.93909484+-503.7709571,0,0,0,
                           -501.85050502+-503.38695726,0,0,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s19_Ven <- matrix(c(0,0,-524.79833798,-518.99307724,-518.55165739,-525.10025989,
                           0,0,-518.9951993,-524.78352108,-525.10692923,-518.5493599,
                           -524.79833798,-518.9951993,0,-542.42906782,0,0,
                           -518.99307724,-524.78352108,-542.42906782,0,0,0,
                           -518.55165739,-525.10692923,0,0,0,-542.12941821,
                           -525.10025989,-518.5493599,0,0,-542.12941821,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s20_Ven <- matrix(c(0,-503.82248701+-503.97685382,-503.70702157+-504.10650052,0,0,
                           -503.82248701+-503.97685382,0,0,-515.79990912,-515.02751032,
                           -503.70702157+-504.10650052,0,0,-515.08544306,-514.80036486,
                           0,-515.79990912,-515.08544306,0,-536.01101882,
                           0,-515.02751032,-514.80036486,-536.01101882,0),nrow = 5,ncol = 5,byrow = TRUE)
lista_matriz_Cu_phen_Ven <- list("s0" = matriz_s0_Ven,"s1" = matriz_s1_Ven,"s2" = matriz_s2_Ven,"s3" = matriz_s3_Ven,"s4" = matriz_s4_Ven,"s5" = matriz_s5_Ven,"s6" = matriz_s6_Ven,"s7" = matriz_s7_Ven,"s8" = matriz_s8_Ven,"s9" = matriz_s9_Ven,"s10" = matriz_s10_Ven,"s11" = matriz_s11_Ven,"s12" = matriz_s12_Ven,"s13" = matriz_s13_Ven,"s14" = matriz_s14_Ven,"s15" = matriz_s15_Ven,"s16" = matriz_s16_Ven,"s17" = matriz_s17_Ven,"s18" = matriz_s18_Ven,"s19" = matriz_s19_Ven,"s20" = matriz_s20_Ven)
#Vrep
matriz_s0_Vrep <- matrix(c(0,384.98328677,384.92779214,393.61617407,
                           384.98328677,0,393.61935133,384.92021594,
                           384.92779214,393.61935133,0,384.61629741,
                           393.61617407,384.92021594,384.61629741,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s1_Vrep <- matrix(c(0,0,397.36489933,393.26817869,392.23449269,398.39488954,
                           0,0,393.26775634,397.36893854,398.41114007,392.22175472,
                           397.36489933,393.26775634,0,0,423.46310072,0,
                           393.26817869,397.36893854,0,0,0,423.46941772,
                           392.23449269,398.41114007,423.46310072,0,0,0,
                           398.39488954,392.22175472,0,423.46941772,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s2_Vrep <- matrix(c(0,0,397.40704665,392.81761058,392.79808091,397.4244217,
                           0,0,392.44812976,398.03849339,398.07383898,392.41741751,
                           397.40704665,392.44812976,0,0,422.94508928,0,
                           392.81761058,398.03849339,0,0,0,422.95039215,
                           392.79808091,398.07383898,422.94508928,0,0,0,
                           397.4244217,392.41741751,0,422.95039215,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s3_Vrep <- matrix(c(0,0,393.15028862+394.99859175,395.81109776+393.15973423,
                           0,0,393.16911745+395.81734908,395.01716869+393.15447879,
                           393.15028862+394.99859175,395.81734908+393.16911745,0,0,
                           395.81109776+393.15973423,395.01716869+393.15447879,0,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s4_Vrep <- matrix(c(0,0,396.05004841,395.62054104,
                           0,0,396.07488142,395.62096993,
                           396.05004841,396.07488142,0,396.51697689+396.51733202,
                           395.62054104,395.62096993,396.51697689+396.51733202,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s5_Vrep <- matrix(c(0,0,376.44907272,377.03892699,
                           0,0,377.00915093,376.41090015,
                           376.44907272,377.00915093,0,402.33404768+402.52019135,
                           377.03892699,376.41090015,402.33404768+402.52019135,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s6_Vrep <- matrix(c(0,0,377.27117957,376.86824237,
                           0,0,376.91087462,377.30818974,
                           377.27117957,376.91087462,0,401.87228618+401.67226617,
                           376.86824237,377.30818974,401.87228618+401.67226617,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s7_Vrep <- matrix(c(0,0,390.28690751,390.02864736,0,387.45998865,
                           0,0,0,393.63253918,0,393.69902116,
                           390.28690751,0,0,0,390.24428288,0,
                           390.02864736,393.63253918,0,0,387.83816949,0,
                           0,0,390.0313771,387.83816949,0,390.0313771,
                           387.45998865,393.69902116,0,0,390.0313771,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s8_Vrep <- matrix(c(0,389.8528836,0,0,388.50342017,388.92416666,
                           389.8528836,0,0,389.64092221,0,0,
                           0,0,0,0,393.20423461,393.3859404,
                           0,389.64092221,0,0,389.32075213,388.5318598,
                           388.50342017,0,393.20423461,389.32075213,0,0,
                           388.92416666,0,393.3859404,388.5318598,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s9_Vrep <- matrix(c(0,0,377.96660553,377.80496516,
                           0,0,377.97392593,377.84417372,
                           377.96660553,377.97392593,0,402.02010326+402.01670912,
                           377.80496516,377.84417372,402.02010326+402.01670912,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s10_Vrep <- matrix(c(0,388.05587298+388.18894973,388.2622358+387.98012227,0,0,
                            388.05587298+388.18894973,0,0,397.11102109,396.50301594,
                            388.2622358+387.98012227,0,0,396.55201026,396.31479672,
                            0,397.11102109,396.55201026,0,414.02725248,
                            0,396.50301594,396.31479672,414.02725248,0),nrow = 5,ncol = 5,byrow = TRUE)
matriz_s11_Vrep <- matrix(c(0,388.20283687+388.00111822,388.07508524+388.14311714,0,0,
                            388.20283687+388.00111822,0,0,396.54483508,396.31420491,
                            388.07508524+388.14311714,0,0,397.10001831,396.4722955,
                            0,396.54483508,397.10001831,0,413.97018704,
                            0,396.31420491,396.4722955,413.97018704,0),nrow = 5,ncol = 5,byrow = TRUE)
matriz_s12_Vrep <- matrix(c(0,0,385.1337085,385.4437343,
                           0,0,386.43274998,385.99122885,
                           385.1337085,386.43274998,0,397.68767962+397.82382333,
                           385.4437343,385.99122885,397.68767962+397.82382333,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s13_Vrep <- matrix(c(0,0,378.63964212,379.08862273,
                            0,0,376.8642442,376.40757488,
                            378.63964212,376.8642442,0,401.21988585+401.34789449,
                            379.08862273,376.40757488,401.21988585+401.34789449,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s14_Vrep <- matrix(c(0,380.60529745,0,381.01969385,
                            380.60529745,0,373.26679314,402.28277308+402.38595902,
                            0,373.26679314,0,373.01232848,
                            381.01969385,402.28277308+402.38595902,373.01232848,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s15_Vrep <- matrix(c(0,0,385.7209908,381.62049059,
                            0,0,388.59151286,385.82960942,
                            385.7209908,388.59151286,0,398.97716899+398.63780215,
                            381.62049059,385.82960942,398.97716899+398.63780215,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s16_Vrep <- matrix(c(0,395.73434573,383.00305527,0,394.86995767,0,
                            395.73434573,0,0,383.00243372,0,394.8644327,
                            383.00305527,0,0,0,0,383.62756451,
                            0,383.00243372,0,0,383.5866329,0,
                            394.86995767,0,0,383.5866329,0,395.32021693,
                            0,394.8644327,383.62756451,0,395.32021693,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s17_Vrep <- matrix(c(0,383.75238127+384.27292237,387.62078295+387.58923848,387.92160549+387.67079395,
                            383.75238127+384.27292237,0,376.24242215,0,
                            387.62078295+387.58923848,376.24242215,0,0,
                            387.92160549+387.67079395,0,0,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s18_Vrep <- matrix(c(0,384.00440399+384.46825062,386.88576185+388.45502732,386.81880532+388.12461323,
                            384.00440399+384.46825062,0,0,0,
                            386.88576185+388.45502732,0,0,0,
                            386.81880532+388.12461323,0,0,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s19_Vrep <- matrix(c(0,0,403.64711338,399.26907771,398.92410717,403.85776046,
                            0,0,399.27065332,403.6361426,403.86168368,398.92232355,
                            403.64711338,399.27065332,0,419.41250457,0,0,
                            399.26907771,403.6361426,419.41250457,0,0,0,
                            398.92410717,403.86168368,0,0,0,419.14600739,
                            403.85776046,398.92232355,0,0,419.14600739,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s20_Vrep <- matrix(c(0,388.05587298+388.18894974,387.98012227+388.26223581,0,0,
                            388.05587298+388.18894974,0,0,397.11102109,396.50301595,
                            387.98012227+388.26223581,0,0,396.55201026,396.31479672,
                            0,397.11102109,396.55201026,0,414.02725248,
                            0,396.50301595,396.31479672,414.02725248,0),nrow = 5,ncol = 5,byrow = TRUE)
lista_matriz_Cu_phen_Vrep <- list("s0" = matriz_s0_Vrep,"s1" = matriz_s1_Vrep,"s2" = matriz_s2_Vrep,"s3" = matriz_s3_Vrep,"s4" = matriz_s4_Vrep,"s5" = matriz_s5_Vrep,"s6" = matriz_s6_Vrep,"s7" = matriz_s7_Vrep,"s8" = matriz_s8_Vrep,"s9" = matriz_s9_Vrep,"s10" = matriz_s10_Vrep,"s11" = matriz_s11_Vrep,"s12" = matriz_s12_Vrep,"s13" = matriz_s13_Vrep,"s14" = matriz_s14_Vrep,"s15" = matriz_s15_Vrep,"s16" = matriz_s16_Vrep,"s17" = matriz_s17_Vrep,"s18" = matriz_s18_Vrep,"s19" = matriz_s19_Vrep,"s20" = matriz_s20_Vrep) 
#Vnuc
matriz_s0_Vnuc  <- matrix(c(0,82.556399964,82.544681877,82.94916969,
                            82.556399964,0,82.949278268,82.544078412,
                            82.544681877,82.949278268,0,82.523971546,
                            82.94916969,82.544078412,82.523971546,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s1_Vnuc  <- matrix(c(0,0,82.8509507,82.691308647,82.632424254,82.869562315,
                            0,0,82.691087255,82.850924081,82.869897073,82.631856787,
                            82.8509507,82.691087255,0,0,83.894269868,0,
                            82.691308647,82.850924081,0,0,0,83.894521762,
                            82.632424254,82.869897073,83.894269868,0,0,0,
                            82.869562315,82.631856787,0,83.894521762,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s2_Vnuc <- matrix(c(0,0,82.852822974,82.671767629,82.670614854,82.853139951,
                           0,0,82.650101973,82.862203488,82.863066666,82.648642097,
                           82.852822974,82.650101973,0,0,83.878259226,0,
                           82.671767629,82.862203488,0,0,0,83.878409568,
                           82.670614854,82.863066666,83.878259226,0,0,0,
                           82.853139951,82.648642097,0,83.878409568,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s3_Vnuc <- matrix(c(0,0,82.658068083+82.798363036,82.813498993+82.69795922,
                           0,0,82.69838917+82.813665071,82.798908434+82.658228503,
                           82.658068083+82.798363036,82.813665071+82.69838917,0,0,
                           82.813498993+82.69795922,82.798908434+82.658228503,0,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s4_Vnuc <- matrix(c(0,0,82.823432522,82.816769461,
                           0,0,82.82419022,82.816707238,
                           82.823432522,82.82419022,0,82.744056569+82.744402124,
                           82.816769461,82.816707238,82.744056569+82.744402124,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s5_Vnuc <- matrix(c(0,0,82.42193006,82.43081695,
                           0,0,82.429805118,82.420975345,
                           82.42193006,82.429805118,0,83.004533974+83.032396194,
                           82.43081695,82.420975345,83.004533974+83.032396194,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s6_Vnuc <- matrix(c(0,0,82.443736187,82.438639436,
                           0,0,82.439765107,82.444697845,
                           82.443736187,82.439765107,0,83.022480476+82.994268868,
                           82.438639436,82.444697845,83.022480476+82.994268868,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s7_Vnuc <- matrix(c(0,0,82.808505448,82.710581118,0,82.593406915,
                           0,0,0,82.90613978,0,82.896767482,
                           82.808505448,0,0,0,82.818404939,0,
                           82.710581118,82.90613978,0,0,82.626269933,0,
                           0,0,82.818404939,82.626269933,0,82.709965451,
                           82.593406915,82.896767482,0,0,82.709965451,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s8_Vnuc <- matrix(c(0,82.803414709,0,0,82.645893603,82.656043053,
                           82.803414709,0,0,82.806483253,0,0,
                           0,0,0,0,82.898060688,82.894063417,
                           0,82.806483253,0,0,82.689288562,82.64694928,
                           82.645893603,0,82.898060688,82.689288562,0,0,
                           82.656043053,0,82.894063417,82.64694928,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s9_Vnuc <- matrix(c(0,0,82.459433069,82.460935804,
                           0,0,82.459791345,82.46190953,
                           82.459433069,82.459791345,0,83.007610414+83.007107541,
                           82.460935804,82.46190953,83.007610414+83.007107541,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s10_Vnuc <- matrix(c(0,82.581256579+82.595876853,82.579996954+82.586877712,0,0,
                            82.581256579+82.595876853,0,0,82.85303661,82.826832075,
                            82.579996954+82.586877712,0,0,82.829977044,82.809581177,
                            0,82.85303661,82.829977044,0,83.587364668,
                            0,82.826832075,82.809581177,83.587364668,0),nrow = 5,ncol = 5,byrow = TRUE)
matriz_s11_Vnuc <- matrix(c(0,82.578212868+82.588149668,82.581949704+82.59479003,0,0,
                            82.578212868+82.588149668,0,0,82.829987284,82.809907214,
                            82.581949704+82.59479003,0,0,82.853112782,82.825546228,
                            0,82.829987284,82.853112782,0,83.585611068,
                            0,82.809907214,82.825546228,83.585611068,0),nrow = 5,ncol = 5,byrow = TRUE)
matriz_s12_Vnuc <- matrix(c(0,0,82.619919663,82.620181749,
                            0,0,82.650903558,82.646794855,
                            82.619919663,82.650903558,0,82.835517553+82.862051463,
                            82.620181749,82.646794855,82.835517553+82.862051463,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s13_Vnuc <- matrix(c(0,0,82.48545408,82.492761861,
                            0,0,82.432718547,82.426403547,
                            82.48545408,82.432718547,0,82.985254645+83.010854087,
                            82.492761861,82.426403547,82.985254645+83.010854087,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s14_Vnuc <- matrix(c(0,82.536581095,0,82.54635961,
                            82.536581095,0,82.323778348,83.023858993+83.014890434,
                            0,82.323778348,0,82.332505124,
                            82.54635961,83.023858993+83.014890434,82.332505124,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s15_Vnuc <- matrix(c(0,0,82.626145192,82.521427322,
                            0,0,82.713873195,82.654655913,
                            82.626145192,82.713873195,0,82.883174701+82.870167443,
                            82.521427322,82.654655913,82.883174701+82.870167443,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s16_Vnuc <- matrix(c(0,82.868787654,82.615691429,0,82.821308613,0,
                            82.868787654,0,0,82.615698833,0,82.820714868,
                            82.615691429,0,0,0,0,82.624125685,
                            0,82.615698833,0,0,82.623050943,0,
                            82.821308613,0,0,82.623050943,0,82.834131857,
                            0,82.820714868,82.624125685,0,82.834131857,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s17_Vnuc <- matrix(c(0,82.524211073+82.558615266,82.620148155+82.609653351,82.64202182+82.62033687,
                            82.524211073+82.558615266,0,82.435956031,0,
                            82.620148155+82.609653351,82.435956031,0,0,
                            82.64202182+82.62033687,0,0,0),nrow = 4,ncol = 4)
matriz_s18_Vnuc <- matrix(c(0,82.530630805+82.562032361,82.589566276+82.667000102,82.587769052+82.634858567,
                            82.530630805+82.562032361,0,0,0,
                            82.589566276+82.667000102,0,0,0,
                            82.587769052+82.634858567,0,0,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s19_Vnuc <- matrix(c(0,0,83.089775223,82.925916716,82.906596397,83.090723646,
                            0,0,82.92583029,83.089426835,83.090603793,82.906261963,
                            83.089775223,82.92583029,0,83.676374452,0,0,
                            82.925916716,83.089426835,83.676374452,0,0,0,
                            82.906596397,83.090603793,0,0,0,83.647797504,
                            83.090723646,82.906261963,0,0,83.647797504,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s20_Vnuc <- matrix(c(0,82.581256579+82.595876853,82.586877712+82.579996954,0,0,
                            82.581256579+82.595876853,0,0,82.85303661,82.826832075,
                            82.586877712+82.579996954,0,0,82.829977044,82.809581177,
                            0,82.85303661,82.829977044,0,83.587364668,
                            0,82.826832075,82.809581177,83.587364668,0),nrow = 5,ncol = 5,byrow = TRUE)
lista_matriz_Cu_phen_Vnuc <- list("s0" = matriz_s0_Vnuc,"s1" = matriz_s1_Vnuc,"s2" = matriz_s2_Vnuc,"s3" = matriz_s3_Vnuc,"s4" = matriz_s4_Vnuc,"s5" = matriz_s5_Vnuc,"s6" = matriz_s6_Vnuc,"s7" = matriz_s7_Vnuc,"s8" = matriz_s8_Vnuc,"s9" = matriz_s9_Vnuc,"s10" = matriz_s10_Vnuc,"s11" = matriz_s11_Vnuc,"s12" = matriz_s12_Vnuc,"s13" = matriz_s13_Vnuc,"s14" = matriz_s14_Vnuc,"s15" = matriz_s15_Vnuc,"s16" = matriz_s16_Vnuc,"s17" = matriz_s17_Vnuc,"s18" = matriz_s18_Vnuc,"s19" = matriz_s19_Vnuc,"s20" = matriz_s20_Vnuc)
#V(r)
matriz_s0_V <- matrix(c(0,-114.15788759,-114.16758792,-116.68386227,
                        -114.15788759,0,-116.68483101,-114.16602787,
                        -114.16758792,-116.68483101,0,-114.09012944,
                        -116.68386227,-114.16602787,-114.09012944,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s1_V <- matrix(c(0,0,-119.07665829,-117.71371037,-117.52611362,-119.36222583,
                        0,0,-117.71397629,-119.0784482,-119.36805155,-117.52233685,
                        -119.07665829,-117.71397629,0,0,-124.34678525,0,
                        -117.71371037,-119.0784482,0,0,0,-124.34879276,
                        -117.52611362,-119.36805155,-124.34678525,0,0,0,
                        -119.36222583,-117.52233685,0,-124.34879276,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s2_V <- matrix(c(0,0,-119.04366542,-117.6073969,-117.60427069,-119.04773012,
                        0,0,-117.51831196,-119.27193013,-119.28215137,-117.51163963,
                        -119.04366542,-117.51831196,0,0,-124.21709028,0,
                        -117.6073969,-119.27193013,0,0,0,-124.21818662,
                        -117.60427069,-119.28215137,-124.21709028,0,0,0,
                        -119.04773012,-117.51163963,0,-124.21818662,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s3_V <- matrix(c(0,0,-117.77255887+-118.15480758,-118.38701868+-117.60343932,
                         0,0,-117.60597292+-118.38885274,-118.16128936+-117.77366395,
                         -117.77255887+-118.15480758,-118.38885274+-117.60597292,0,0,
                         -118.38701868+-117.60343932,-118.16128936+-117.77366395,0,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s4_V <- matrix(c(0,0,-118.52278446,-118.39706918,
                        0,0,-118.53003936,-118.39867846,
                        -118.52278446,-118.53003936,0,-118.94655132+-118.9457221,
                        -118.39706918,-118.39867846,-118.94655132+-118.9457221,0),nrow = 4,ncol = 4,byrow = TRUE) 
matriz_s5_V <- matrix(c(0,0,-111.99181637,-112.17486758,
                        0,0,-112.16511543,-111.97994945,
                        -111.99181637,-112.16511543,0,-119.77412421+-119.77947863,
                        -112.17486758,-111.97994945,-119.77412421+-119.77947863,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s6_V <- matrix(c(0,0,-112.23604646,-112.11932375,
                        0,0,-112.13249818,-112.24780767,
                        -112.23604646,-112.13249818,0,-119.49643125+-119.48671253,
                        -112.11932375,-112.24780767,-119.49643125+-119.48671253,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s7_V <- matrix(c(0,0,-116.12870632,-115.66276091,0,-115.16862309,
                        0,0,0,-117.14068487,0,-117.19767307,
                        -116.12870632,0,0,0,-116.07968502,0,
                        -115.66276091,-117.14068487,0,0,-115.24011789,0,
                        0,0,-116.07968502,-115.24011789,0,-115.66492653,
                        -115.16862309-117.19767307,0,0,-115.66492653,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s8_V <- matrix(c(0,-115.99751834,0,0,-115.37622898,-115.4376096,
                        -115.99751834,0,0,-115.89427627,0,0,
                        0,0,0,0,-117.0042859,-117.09763612,
                        0,-115.89427627,0,0,-115.51476357,-115.38360614,
                        -115.37622898,0,-117.0042859,-115.51476357,0,0,
                        -115.4376096,0,-117.09763612,-115.38360614,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s9_V <- matrix(c(0,0,-112.45767053,-112.42611396,
                        0,0,-112.46061659,-112.43791178,
                        -112.45767053,-112.46061659,0,-119.65970337+-119.65978502,
                        -112.42611396,-112.43791178,-119.65970337+-119.65978502,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s10_V <- matrix(c(0,-115.76661402+-115.78790408,-115.84426471+-115.7268993,0,0,
                         -115.76661402+-115.78790408,0,0,-118.68888803,-118.52449437,
                         -115.84426471+-115.7268993,0,0,-118.5334328,-118.48556814,
                         0,-118.68888803,-118.5334328,0,-121.98376633,
                         0,-118.52449437,-118.48556814,-121.98376633,0),nrow = 5,ncol = 5,byrow = TRUE)
matriz_s11_V <- matrix(c(0,-115.8239325+-115.73185911,-115.77175+-115.77104575,0,0,
                         -115.8239325+-115.73185911,0,0,-118.52907225,-118.4825364,
                         -115.77175+-115.77104575,0,0,-118.68302838,-118.51449263,
                         0,-118.52907225,-118.68302838,0,-121.96895561,
                         0,-118.4825364,-118.51449263,-121.96895561,0),nrow = 5,ncol = 5,byrow = TRUE)
matriz_s12_V <- matrix(c(0,0,-114.62867841,-114.74987787,
                         0,0,-115.07480516,-114.92355421,
                         -114.62867841,-115.07480516,0,-118.69454364+-118.68399101,
                         -114.74987787,-114.92355421,-118.69454364+-118.68399101,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s13_V <- matrix(c(0,0,-112.65872363,-112.79121535,
                         0,0,-112.09374288,-111.95885099,
                         -112.65872363,-112.09374288,0,-119.35553755+-119.36751125,
                         -112.79121535,-111.95885099,-119.35553755+-119.36751125,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s14_V <- matrix(c(0,-113.31500527,0,-113.43005196,
                         -113.31500527,0,-110.94399727,-119.79283039+-119.85240643,
                         0,-110.94399727,0,-110.92926086,
                         -113.43005196,-119.79283039+-119.85240643,-110.92926086,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s15_V <- matrix(c(0,0,-114.81676776,-113.55073928,
                         0,0,-115.79122879,-114.92222288,
                         -114.81676776,-115.79122879,0,-119.0995194+-118.9983267,
                         -113.55073928,-114.92222288,-119.0995194+-118.9983267,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s16_V <- matrix(c(0,-117.50872025,-113.89551397,0,-117.385925,0,
                         -117.50872025,0,0,-113.89542435,0,-117.38506733,
                         -113.89551397,0,0,0,0,-114.15551654,
                         0,-113.89542435,0,0,-114.14276553,0,
                         -117.385925,0,0,-114.14276553,0,-117.43375413,
                         0,-117.38506733,-114.15551654,0,-117.43375413,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s17_V <- matrix(c(0,-113.91536305+-114.01782463,-115.19787397+-115.17913338,-115.2329811+-115.21655791,
                         -113.91536305+-114.01782463,0,-111.82389179,0,
                         -115.19787397+-115.17913338,-111.82389179,0,0,
                         -115.2329811+-115.21655791,0,0,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s18_V <- matrix(c(0,-113.99882576+-114.08850059,-115.053333+-115.31592977,-115.03169971+-115.26234404,
                         -113.99882576+-114.08850059,0,0,0,
                         -115.053333+-115.31592977,0,0,0,
                         -115.03169971+-115.26234404,0,0,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s19_V <- matrix(c(0,0,-121.1512246,-119.72399953,-119.62755021,-121.24249942,
                         0,0,-119.72454598,-121.14737847,-121.24524555,-119.62703635,
                         -121.1512246,-119.72454598,0,-123.01656325,0,0,
                         -119.72399953,-121.14737847,-123.01656325,0,0,0,
                         -119.62755021,-121.24524555,0,0,0,-122.98341081,
                         -121.24249942,-119.62703635,0,0,-122.98341081,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s20_V <- matrix(c(0,-115.76661403+-115.78790409,-115.7268993+-115.84426471,0,0,
                         -115.76661403+-115.78790409,0,0,-118.68888803-118.52449438,
                         -115.7268993+-115.84426471,0,0,-118.5334328,-118.48556814,
                         0,-118.68888803,-118.5334328,0,-121.98376633,
                         0,-118.52449438,-118.48556814,-121.98376633,0),nrow = 5,ncol = 5,byrow = TRUE)
lista_matriz_Cu_phen_V <- list("s0" = matriz_s0_V,"s1" = matriz_s1_V,"s2" = matriz_s2_V,"s3" = matriz_s3_V,"s4" = matriz_s4_V,"s5" = matriz_s5_V,"s6" = matriz_s6_V,"s7" = matriz_s7_V,"s8" = matriz_s8_V,"s9" = matriz_s9_V,"s10" = matriz_s10_V,"s11" = matriz_s11_V,"s12" = matriz_s12_V,"s13" = matriz_s13_V,"s14" = matriz_s14_V,"s15" = matriz_s15_V,"s16" = matriz_s16_V,"s17" = matriz_s17_V,"s18" = matriz_s18_V,"s19" = matriz_s19_V,"s20" = matriz_s20_V)
#G(r)
matriz_s0_G <- matrix(c(0,49.130708439,49.136018278,49.816726261,
                        49.130708439,0,49.816993683,49.13554277,
                        49.136018278,49.816993683,0,49.110536004,
                        49.816726261,49.13554277,49.110536004,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s1_G <- matrix(c(0,0,51.067450027,50.700997162,50.716463931,51.086261483,
                        0,0,50.701019312,51.068027428,51.087948124,50.715393466,
                        51.067450027,50.701019312,0,0,50.55452834,0,
                        50.700997162,51.068027428,0,0,0,50.555013638,
                        50.716463931,51.087948124,50.55452834,0,0,0,
                        51.086261483,50.715393466,0,50.555013638,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s2_G <- matrix(c(0,0,51.021600676,50.693266682,50.693961278,51.021224443,
                        0,0,50.671218611,51.08619858,51.087637971,50.671024556,
                        51.021600676,50.671218611,0,0,50.545530619,0,
                        50.693266682,51.08619858,0,0,0,50.54548615,
                        50.693961278,51.087637971,50.545530619,0,0,0,
                        51.021224443,50.671024556,0,50.54548615,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s3_G <- matrix(c(0,0,50.752091688+50.719224338,50.731797338+50.622717636,
                        0,0,50.623278934+50.73236728,50.721113808+50.752058864,
                        50.752091688+50.719224338,50.73236728+50.623278934,0,0,
                        50.731797338+50.622717636,50.721113808+50.752058864,0,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s4_G <- matrix(c(0,0,50.823420013,50.7881575,
                        0,0,50.824268937,50.790178815,
                        50.823420013,50.824268937,0,51.145544624+51.145347798,
                        50.7881575,50.790178815,51.145544624+51.145347798,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s5_G <- matrix(c(0,0,48.826018519,48.852077228,
                        0,0,48.849525203,48.822667029,
                        48.826018519,48.849525203,0,50.632379897+50.632472058,
                        48.852077228,48.822667029,50.632379897+50.632472058,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s6_G <- matrix(c(0,0,48.861996235,48.854003642,
                        0,0,48.857126725,48.865528218,
                        48.861996235,48.857126725,0,50.50657747+50.505354459,
                        48.854003642,48.865528218,50.50657747+50.505354459,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s7_G <- matrix(c(0,0,49.809565141,49.424678543,0,49.497775684,
                        0,0,0,50.066999851,0,50.087232453,
                        49.809565141,0,0,0,49.791567431,0,
                        49.424678543,50.066999851,0,0,49.518881099,0,
                        0,0,49.791567431,49.518881099,0,49.424434878,
                        49.497775684,50.087232453,0,0,49.424434878,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s8_G <- matrix(c(0,49.781120431,0,0,49.486287928,49.43228975,
                        49.781120431,0,0,49.74334575,0,0,
                        0,0,0,0,50.02881862,50.063272253,
                        0,49.74334575,0,0,49.454845812,49.487119814,
                        49.486287928,0,50.02881862,49.454845812,0,0,
                        49.43228975,0,50.063272253,49.487119814,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s9_G <- matrix(c(0,0,48.939751728,48.929761197,
                        0,0,48.941299434,48.932236365,
                        48.939751728,48.941299434,0,50.612417091+50.612527884,
                        48.929761197,48.932236365,50.612417091+50.612527884,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s10_G <- matrix(c(0,49.96389731+49.975948591,49.982979723+49.953707002,0,0,
                         49.96389731+49.975948591,0,0,50.781058731,50.737130869,
                         49.982979723+49.953707002,0,0,50.74228868,50.728115522,
                         0,50.781058731,50.74228868,0,50.400877546,
                         0,50.737130869,50.728115522,50.400877546,0),nrow = 5,ncol = 5,byrow = TRUE)
matriz_s11_G <- matrix(c(0,49.976641798+49.954193604,49.963988838+49.970328118,0,0,
                         49.976641798+49.954193604,0,0,50.740368219,50.725336374,
                         49.963988838+49.970328118,0,0,50.778081098,50.733871621,
                         0,50.740368219,50.778081098,0,50.399274091,
                         0,50.725336374,50.733871621,50.399274091,0),nrow = 5,ncol = 5,byrow = TRUE)
matriz_s12_G <- matrix(c(0,0,49.50803857,49.526303897,
                         0,0,49.618711268,49.598719293,
                         49.50803857,49.618711268,0,50.62905943+50.624747184,
                         49.526303897,49.598719293,50.629059432+50.624747184,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s13_G <- matrix(c(0,0,48.996242903,49.009501586,
                         0,0,48.822599813,48.807572544,
                         48.996242903,48.822599813,0,50.478244274+50.491448754,
                         49.009501586,48.807572544,50.478244274+50.491448754,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s14_G <- matrix(c(0,49.191274619,0,49.195510106,
                         49.191274619,0,48.516107484,50.682877173+50.695936839,
                         0,48.516107484,0,48.529389695,
                         49.195510106,50.682877173+50.695936839,48.529389695,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s15_G <- matrix(c(0,0,49.548896143,49.21564016,
                         0,0,49.846521553,49.593869876,
                         49.548896143,49.846521553,0,50.750411045+50.725043196,
                         49.21564016,49.593869876,50.750411045+50.725043196,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s16_G <- matrix(c(0,49.948212486,49.237002946,0,50.00771694,0,
                         49.948212486,0,0,49.236876439,0,50.007508773,
                         49.237002946,0,0,0,0,49.347751171,
                         0,49.236876439,0,0,49.344618779,0,
                         50.00771694,0,0,49.344618779,0,49.932553367,
                         0,50.007508773,49.347751171,0,49.932553367,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s17_G <- matrix(c(0,49.153068008+49.161856353,49.493756852+49.461496465,49.475018259+49.496552827,
                         49.153068008+49.161856353,0,48.711609388,0,
                         49.493756852+49.461496465,48.711609388,0,0,
                         49.475018259+49.496552827,0,0,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s18_G <- matrix(c(0,49.178109495+49.185246574,49.513613301+49.435945439,49.5099105+49.422402898,
                         49.178109495+49.185246574,0,0,0,
                         49.513613301+49.435945439,0,0,0,
                         49.5099105+49.422402898,0,0,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s19_G <- matrix(c(0,0,51.748066422,51.410082314,51.377194206,51.779208603,
                         0,0,51.409758885,51.746556476,51.780717919,51.377023702,
                         51.748066422,51.409758885,0,50.01807414,0,0,
                         51.410082314,51.746556476,50.01807414,0,0,0,
                         51.377194206,51.780717919,0,0,0,50.007835622,
                         51.779208603,51.377023702,0,0,50.007835622,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s20_G <- matrix(c(0,49.96389731+49.975948592,49.953707003+49.982979723,0,0,
                         49.96389731+49.975948592,0,0,50.781058732,50.73713087,
                         49.953707003+49.982979723,0,0,50.742288681,50.728115523,
                         0,50.781058732,50.742288681,0,50.400877546,
                         0,50.73713087,50.728115523,50.400877546,0),nrow = 5,ncol = 5,byrow = TRUE)
lista_matriz_Cu_phen_G <- list("s0" = matriz_s0_G,"s1" = matriz_s1_G,"s2" = matriz_s2_G,"s3" = matriz_s3_G,"s4" = matriz_s4_G,"s5" = matriz_s5_G,"s6" = matriz_s6_G,"s7" = matriz_s7_G,"s8" = matriz_s8_G,"s9" = matriz_s9_G,"s10" = matriz_s10_G,"s11" = matriz_s11_G,"s12" = matriz_s12_G,"s13" = matriz_s13_G,"s14" = matriz_s14_G,"s15" = matriz_s15_G,"s16" = matriz_s16_G,"s17" = matriz_s17_G,"s18" = matriz_s18_G,"s19" = matriz_s19_G,"s20" = matriz_s20_G)
#K(r)
matriz_s0_K <- matrix(c(0,65.027179147,65.031569644,66.867136006,
                        65.027179147,0,66.867837328,65.030485099,
                        65.031569644,66.867837328,0,64.979593434,
                        66.867136006,65.030485099,64.979593434,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s1_K <- matrix(c(0,0,68.009208267,67.012713211,66.809649686,68.275964344,
                        0,0,67.012956982,68.010420772,68.280103431,66.806943385,
                        68.009208267,67.012956982,0,0,73.792256907,0,
                        67.012713211,68.010420772,0,0,0,73.793779122,
                        66.809649686,68.280103431,73.792256907,0,0,0,
                        68.275964344,66.806943385,0,73.793779122,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s2_K <- matrix(c(0,0,68.022064748,66.914130219,66.910309413,68.026505674,
                        0,0,66.84709335,68.185731551,68.194513402,66.840615072,
                        68.022064748,66.84709335,0,0,73.671559658,0,
                        66.914130219,68.185731551,0,0,0,73.672700472,
                        66.910309413,68.194513402,73.671559658,0,0,0,
                        68.026505674,66.840615072,0,73.672700472,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s3_K <- matrix(c(0,0,67.020467179+67.435583243,67.655221345+66.980721688,
                        0,0,66.982693981+67.656485464,67.440175552+67.021605084,
                        67.020467179+67.435583243,67.656485464+66.982693981,0,0,
                        67.655221345+66.980721688,67.440175552+67.021605084,0,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s4_K <- matrix(c(0,0,67.699364442,67.608911677,
                        0,0,67.705770419,67.608499644,
                        67.699364442,67.705770419,0,67.801006695+67.800374305,
                        67.608911677,67.608499644,67.801006695+67.800374305,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s5_K <- matrix(c(0,0,63.165797853,63.322790355,
                        0,0,63.31559023,63.157282424,
                        63.165797853,63.31559023,0,69.141744317+69.147006573,
                        63.322790355,63.157282424,69.141744317+69.147006573,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s6_K <- matrix(c(0,0,63.37405023,63.265320104,
                        0,0,63.275371452,63.382279453,
                        63.37405023,63.275371452,0,68.989853785+68.981358073,
                        63.265320104,63.382279453,68.989853785+68.981358073,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s7_K <- matrix(c(0,0,66.319141182,66.238082367,0,65.670847404,
                        0,0,0,67.073685017,0,67.11044062,
                        66.319141182,0,0,0,66.288117586,0,
                        66.238082367,67.073685017,0,0,65.721236793,0,
                        0,0,66.288117586,65.721236793,0,66.240491648,
                        65.670847404,67.11044062,0,0,66.240491648,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s8_K <- matrix(c(0,66.216397905,0,0,65.889941056,66.005319851,
                        66.216397905,0,0,66.150930516,0,0,
                        0,0,0,0,66.975467282,67.03436387,
                        0,66.150930516,0,0,66.059917757,65.896486328,
                        65.889941056,0,66.975467282,66.059917757,0,0,
                        66.005319851,0,67.03436387,65.896486328,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s9_K <- matrix(c(0,0,63.517918798,63.496352763,
                        0,0,63.519317155,63.50567542,
                        63.517918798,63.519317155,0,69.047286279,69.047257137,
                        63.496352763,63.50567542,69.047286279+69.047257137,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s10_K <- matrix(c(0,65.802716715+65.811955493,65.861284985+65.773192298,0,0,
                         65.802716715+65.811955493,0,0,67.907829296,67.787363505,
                         65.861284985+65.773192298,0,0,67.79114412,67.757452617,
                         0,67.907829296,67.79114412,0,71.582888788,
                         0,67.787363505,67.757452617,71.582888788,0),nrow = 5,ncol = 5,byrow = TRUE)
matriz_s11_K <- matrix(c(0,65.847290703+65.77766551,65.80776116+65.800717634,0,0,
                         65.847290703+65.77766551,0,0,67.788704027,67.757200027,
                         65.80776116+65.800717634,0,0,67.90494728,67.780621011,
                         0,67.788704027,67.90494728,0,71.569681517,
                         0,67.757200027,67.780621011,71.569681517,0),nrow = 5,ncol = 5,byrow = TRUE)
matriz_s12_K <- matrix(c(0,0,65.120639845,65.223573977,
                         0,0,65.456093897,65.324834915,
                         65.120639845,65.456093897,0,68.065484212+68.059243826,
                         65.223573977,65.324834915,68.065484212+68.059243826,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s13_K <- matrix(c(0,0,63.66248073,63.781713763,
                         0,0,63.271143064,63.151278445,
                         63.66248073,63.271143064,0,68.877293277+68.876062497,
                         63.781713763,63.151278445,68.877293277+68.876062497,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s14_K <- matrix(c(0,64.123730647,0,64.23454185,
                         64.123730647,0,62.427889791,69.109953218+69.156469593,
                         0,62.427889791,0,62.399871161,
                         64.23454185,69.109953218+69.156469593,62.399871161,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s15_K <- matrix(c(0,0,65.267871615,64.335099123,
                         0,0,65.944707241,65.328353006,
                         65.267871615,65.944707241,0,68.349108353+68.273283501,
                         64.335099123,65.328353006,68.349108353+68.273283501,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s16_K <- matrix(c(0,67.560507765,64.658511025,0,67.378208057,0,
                         67.560507765,0,0,64.658547915,0,67.377558561,
                         64.658511025,0,0,0,0,64.807765367,
                         0,64.658547915,0,0,64.798146748,0,
                         67.378208057,0,0,64.798146748,0,67.501200764,
                         0,67.377558561,64.807765367,0,67.501200764,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s17_K <- matrix(c(0,64.762295042+64.855968281,65.704117121+65.717636916,65.757962845+65.720005084,
                         64.762295042+64.855968281,0,63.112282399,0,
                         65.704117121+65.717636916,63.112282399,0,0,
                         65.757962845+65.720005084,0,0,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s18_K <- matrix(c(0,64.820716263+64.903254016,65.539719695+65.879984334,65.521789205+65.83994114,
                         64.820716263+64.903254016,0,0,0,
                         65.539719695+65.879984334,0,0,0,
                         65.521789205+65.83994114,0,0,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s19_K <- matrix(c(0,0,69.403158174,68.313917211,68.250356007,69.463290819,
                         0,0,68.314787091,69.400821996,69.464527632,68.250012649,
                         69.403158174,68.314787091,0,72.998489108,0,0,
                         68.313917211,69.400821996,72.998489108,0,0,0,
                         68.250356007,69.464527632,0,0,0,72.975575192,
                         69.463290819,68.250012649,0,0,72.975575192,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s20_K  <- matrix(c(0,65.802716715+65.811955493,65.773192299+65.861284986,0,0,
                          65.802716715+65.811955493,0,0,67.907829297,67.787363506,
                          65.773192299+65.861284986,0,0,67.79114412,67.757452617,
                          0,67.907829297,67.79114412,0,71.582888788,
                          0,67.787363506,67.757452617,71.582888788,0),nrow = 5,ncol = 5,byrow = TRUE)
lista_matriz_Cu_phen_K <- list("s0" = matriz_s0_K,"s1" = matriz_s1_K,"s2" = matriz_s2_K,"s3" = matriz_s3_K,"s4" = matriz_s4_K,"s5" = matriz_s5_K,"s6" = matriz_s6_K,"s7" = matriz_s7_K,"s8" = matriz_s8_K,"s9" = matriz_s9_K,"s10" = matriz_s10_K,"s11" = matriz_s11_K,"s12" = matriz_s12_K,"s13" = matriz_s13_K,"s14" = matriz_s14_K,"s15" = matriz_s15_K,"s16" = matriz_s16_K,"s17" = matriz_s17_K,"s18" = matriz_s18_K,"s19" = matriz_s19_K,"s20" = matriz_s20_K)
#L(r)
matriz_s0_L <- matrix(c(0,15.896470708,15.895551366,17.050409745,
                        15.896470708,0,17.050843645,15.894942329,
                        15.895551366,17.050843645,0,15.86905743,
                        17.050409745,15.894942329,15.86905743,0),nrow = 4,ncol = 4,byrow = TRUE) 
matriz_s1_L <- matrix(c(0,0,16.94175824,16.311716049,16.093185755,17.18970286,
                        0,0,16.31193767,16.942393344,17.192155307,16.091549919,
                        16.94175824,16.31193767,0,0,23.237728567,0,
                        16.311716049,16.942393344,0,0,0,23.238765484,
                        16.093185755,17.192155307,23.237728567,0,0,0,
                        17.18970286,16.091549919,0,23.238765484,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s2_L <- matrix(c(0,0,17.000464072,16.220863536,16.216348135,17.005281232,
                        0,0,16.175874739,17.099532971,17.106875431,16.169590516,
                        17.000464072,16.175874739,0,0,23.126029039,0,
                        16.220863536,17.099532971,0,0,0,23.127214322,
                        16.216348135,17.106875431,23.126029039,0,0,0,
                        17.005281232,16.169590516,0,23.127214322,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s3_L <- matrix(c(0,0,16.268375492+16.716358905,16.923424007+16.358004052,
                        0,0,16.359415046+16.924118184,16.719061744+16.26954622,
                        16.268375492+16.716358905,16.924118184+16.359415046,0,0,
                        16.923424007+16.358004052,16.719061744+16.26954622,0,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s4_L <- matrix(c(0,0,16.875944428,16.820754177,
                        0,0,16.881501483,16.81832083,
                        16.875944428,16.881501483,0,16.655462072+16.655026506,
                        16.820754177,16.81832083,16.655462072+16.655026506,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s5_L <- matrix(c(0,0,14.339779334,14.470713127,
                        0,0,14.466065026,14.334615395,
                        14.339779334,14.466065026,0,18.50936442+18.514534516,
                        14.470713127,14.334615395,18.50936442+18.514534516,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s6_L <- matrix(c(0,0,14.512053994,14.411316462,
                        0,0,14.418244727,14.516751236,
                        14.512053994,14.418244727,0,18.483276315+18.476003614,
                        14.411316462,14.516751236,18.483276315+18.476003614,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s7_L <- matrix(c(0,0,16.509576041,16.813403824,0,16.173071719,
                        0,0,0,17.006685166,0,17.023208168,
                        16.509576041,0,0,0,16.496550156,0,
                        16.813403824,17.006685166,0,0,16.202355694,0,
                        0,0,16.496550156,16.202355694,0,16.81605677,
                        16.173071719,17.023208168,0,0,16.81605677,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s8_L <- matrix(c(0,16.435277474,0,0,16.407584766,0,0,
                        16.435277474,0,0,16.407584766,0,0,
                        0,0,0,0,16.946648662,16.971091617,
                        0,16.407584766,0,0,16.605071946,16.409366514,
                        16.403653129,0,16.946648662,16.605071946,0,0,
                        16.573030101,0,16.971091617,16.409366514,0,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s9_L <- matrix(c(0,0,14.578167071,14.566591566,
                        0,0,14.578017722,14.573439055,
                        14.578167071,14.578017722,0,18.434869188+18.434729253,
                        14.566591566,14.573439055,18.434869188+18.434729253,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s10_L <- matrix(c(0,15.838819405+15.836006901,15.878305262+15.819485296,0,0,
                         15.838819405+15.836006901,0,0,17.126770565,17.050232636,
                         15.878305262+15.819485296,0,0,17.048855439,17.029337095,
                         0,17.126770565,17.048855439,0,21.182011242,
                         0,17.050232636,17.029337095,21.182011242,0),nrow = 5,ncol = 5,byrow = TRUE)
matriz_s11_L <- matrix(c(0,15.870648905+15.823471905,15.843772322+15.830389516,0,0,
                         15.870648905+15.823471905,0,0,17.048335808,17.031863653,
                         15.843772322+15.830389516,0,0,17.126866183,17.04674939,
                         0,17.048335808,17.126866183,0,21.170407426,
                         0,17.031863653,17.04674939,21.170407426,0),nrow = 5,ncol = 5,byrow = TRUE)
matriz_s12_L <- matrix(c(0,0,15.612601275,15.69727008,
                         0,0,15.83738263,15.726115622,
                         15.612601275,15.83738263,0,17.43642478+17.434496642,
                         15.69727008,15.726115622,17.43642478+17.434496642,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s13_L  <- matrix(c(0,0,14.666237827,14.772212176,
                          0,0,14.448543251,14.343705901,
                          14.666237827,14.448543251,0,18.399049003+18.384613743,
                          14.772212176,14.343705901,18.399049003+18.384613743,0),nrow = 4,ncol = 4,byrow = TRUE )
matriz_s14_L <- matrix(c(0,14.932456028,0,15.039031744,
                         14.932456028,0,13.911782307,18.427076045+18.460532754,
                         0,13.911782307,0,13.870481466,
                         15.039031744,18.427076045+18.460532754,13.870481466,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s15_L <- matrix(c(0,0,15.718975472,15.119458963,
                         0,0,16.098185688,15.73448313,
                         15.718975472,16.098185688,0,17.598697307+17.548240305,
                         15.119458963,15.73448313,17.598697307+17.548240305,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s16_L <- matrix(c(0,17.612295279,15.421508078,0,17.370491117,0,
                         17.612295279,0,0,15.421671476,0,17.370049788,
                         15.421508078,0,0,0,0,15.460014195,
                         0,15.421671476,0,0,15.453527968,0,
                         17.370491117,0,0,15.453527968,0,17.568647397,
                         0,17.370049788,15.460014195,0,17.568647397,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s17_L <- matrix(c(0,15.609227034+15.694111928,16.21036027+16.25614045,16.282944586+16.223452256,
                         15.609227034+15.694111928,0,14.400673012,0,
                         16.21036027+16.25614045,14.400673012,0,0,
                         16.282944586+16.223452256,0,0,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s18_L <- matrix(c(0,15.642606769+15.718007442,16.026106395+16.444038895,16.011878705+16.417538242,
                         15.642606769+15.718007442,0,0,0,
                         16.026106395+16.444038895,0,0,0,
                         16.011878705+16.417538242,0,0,0),nrow = 4,ncol = 4,byrow = TRUE)
matriz_s19_L <- matrix(c(0,0,17.655091752,16.903834896,16.873161801,17.684082216,
                         0,0,16.905028206,17.65426552,17.683809713,16.872988946,
                         17.655091752,17.655091752,0,22.980414968,0,0,
                         16.903834896,17.65426552,22.980414968,0,0,0,
                         16.873161801,17.683809713,0,0,0,22.96773957,
                         17.684082216,16.872988946,0,0,22.96773957,0),nrow = 6,ncol = 6,byrow = TRUE)
matriz_s20_L <- matrix(c(0,15.838819405+15.836006902,15.819485296+15.878305262,0,0,
                         15.838819405+15.836006902,0,0,17.126770565,17.050232636,
                         15.819485296+15.878305262,0,0,17.048855439,17.029337095,
                         0,17.126770565,17.048855439,0,21.182011242,
                         0,17.050232636,17.029337095,21.182011242,0),nrow = 5,ncol = 5,byrow = TRUE)
lista_matriz_Cu_phen_L <- list("s0" = matriz_s0_L,"s1" = matriz_s1_L,"s2" = matriz_s2_L,"s3" = matriz_s3_L,"s4" = matriz_s4_L,"s5" = matriz_s5_L,"s6" = matriz_s6_L,"s7" = matriz_s7_L,"s8" = matriz_s8_L,"s9" = matriz_s9_L,"s10" = matriz_s10_L,"s11" = matriz_s11_L,"s12" = matriz_s12_L,"s13" = matriz_s13_L,"s14" = matriz_s14_L,"s15" = matriz_s15_L,"s16" = matriz_s16_L,"s17" = matriz_s17_L,"s18" = matriz_s18_L,"s19" = matriz_s19_L,"s20" = matriz_s20_L) 