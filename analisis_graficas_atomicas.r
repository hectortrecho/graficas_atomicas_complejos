#!/usr/bin/env Rscript

library(dplyr)

data <- source(file = "~/Documents/codigo_R/investigacion_graficas_atomicas/datos_graficas_atomicas.r")  # indicar la ruta correcta de datos_graficas_atomicas.r
prop_mol <- read.csv(file = "~/Documents/codigo_R/investigacion_graficas_atomicas/output-file.csv")

#for (i in lista_graficas_complejos_acuosos) {tkplot(i)} #generamos todas las graficas en tkplot.

#for (j in lista_graficas_Cu_phen) {tkplot(j)}  #generamos todas las graficas de tkplot.

prop_mol$class <- c(1,2,3,4,5,5,5,6,6,5,7,7,5,5,5,5,8,9,10,2,7)

#buscando filas donde se repiten valores.


#digrafica corregida 8_junio de las graficas de transicion energetica.
df_transicion_Cu_phen <- data.frame(from = c("s0","s1,s2,s19","s1,s2,s19","s3","s4,s5,s6,s9,s12,s13,s14,s15","s4,s5,s6,s9,s12,s13,s14,s15","s4,s5,s6,s9,s12,s13,s14,s15","s7,s8","s7,s8","s4,s5,s6,s9,s12,s13,s14,s15","s10","s11,s20","s4,s5,s6,s9,s12,s13,s14,s15","s4,s5,s6,s9,s12,s13,s14,s15","s4,s5,s6,s9,s12,s13,s14,s15","s4,s5,s6,s9,s12,s13,s14,s15","s16","s17","s18","s1,s2,s19"),
                                           to = c("s1,s2,s19","s1,s2,s19","s3","s4,s5,s6,s9,s12,s13,s14,s15","s4,s5,s6,s9,s12,s13,s14,s15","s4,s5,s6,s9,s12,s13,s14,s15","s7,s8","s7,s8","s4,s5,s6,s9,s12,s13,s14,s15","s10","s11,s20","s4,s5,s6,s9,s12,s13,s14,s15","s4,s5,s6,s9,s12,s13,s14,s15","s4,s5,s6,s9,s12,s13,s14,s15","s4,s5,s6,s9,s12,s13,s14,s15","s16","s17","s18","s1,s2,s19","s11,s20")
                                           )
df_transicion_Cu_phen_nodos <- data.frame(from = c(1,2,2,3,4,4,5,5,4,6,7,4,8,9,10),
                                          to = c(2,2,3,4,4,5,5,4,6,7,4,8,9,10,2)
                                          )
digrafica_transicion_Cu_phen <- graph_from_data_frame(df_transicion_Cu_phen,directed = TRUE)
digrafica_Cu_phen_nodos <- graph_from_data_frame(df_transicion_Cu_phen_nodos,directed = TRUE)

#Buscando que graficas son isomorficas.
lista_iso_oct_Co2 <- sapply(lista_graficas_complejos_acuosos, is_isomorphic_to,graph2 = lista_graficas_complejos_acuosos$Co2)
lista_iso_oct_Cr2 <- sapply(lista_graficas_complejos_acuosos, is_isomorphic_to,graph2 = lista_graficas_complejos_acuosos$Cr2)
lista_iso_oct_Cr3 <- sapply(lista_graficas_complejos_acuosos, is_isomorphic_to,graph2 = lista_graficas_complejos_acuosos$Cr3)
lista_iso_oct_Cu2 <- sapply(lista_graficas_complejos_acuosos, is_isomorphic_to,graph2 = lista_graficas_complejos_acuosos$Cu2)
lista_iso_oct_Mn2 <- sapply(lista_graficas_complejos_acuosos, is_isomorphic_to,graph2 = lista_graficas_complejos_acuosos$Mn2)
lista_iso_oct_Mn3 <- sapply(lista_graficas_complejos_acuosos, is_isomorphic_to,graph2 = lista_graficas_complejos_acuosos$Mn3)
lista_iso_oct_Mn4 <- sapply(lista_graficas_complejos_acuosos, is_isomorphic_to,graph2 = lista_graficas_complejos_acuosos$Mn4)
lista_iso_oct_Fe2 <- sapply(lista_graficas_complejos_acuosos, is_isomorphic_to,graph2 = lista_graficas_complejos_acuosos$Fe2)
lista_iso_oct_Fe3 <- sapply(lista_graficas_complejos_acuosos, is_isomorphic_to,graph2 = lista_graficas_complejos_acuosos$Fe3)
lista_iso_oct_Sc2 <- sapply(lista_graficas_complejos_acuosos, is_isomorphic_to,graph2 = lista_graficas_complejos_acuosos$Sc2)
lista_iso_oct_Sc3 <- sapply(lista_graficas_complejos_acuosos, is_isomorphic_to,graph2 = lista_graficas_complejos_acuosos$Sc3)
lista_iso_oct_Ni2 <- sapply(lista_graficas_complejos_acuosos, is_isomorphic_to,graph2 = lista_graficas_complejos_acuosos$Ni2)
lista_iso_oct_Ni3 <- sapply(lista_graficas_complejos_acuosos, is_isomorphic_to,graph2 = lista_graficas_complejos_acuosos$Ni3)
lista_iso_oct_Ti2 <- sapply(lista_graficas_complejos_acuosos, is_isomorphic_to,graph2 = lista_graficas_complejos_acuosos$Ti2)
lista_iso_oct_Ti3 <- sapply(lista_graficas_complejos_acuosos, is_isomorphic_to,graph2 = lista_graficas_complejos_acuosos$Ti3)
lista_iso_oct_V2 <- sapply(lista_graficas_complejos_acuosos, is_isomorphic_to,graph2 = lista_graficas_complejos_acuosos$V2)
lista_iso_oct_V3 <- sapply(lista_graficas_complejos_acuosos, is_isomorphic_to,graph2 = lista_graficas_complejos_acuosos$V3)
lista_iso_oct_V4 <- sapply(lista_graficas_complejos_acuosos, is_isomorphic_to,graph2 = lista_graficas_complejos_acuosos$V4)
lista_iso_oct_Zn2 <- sapply(lista_graficas_complejos_acuosos, is_isomorphic_to,graph2 = lista_graficas_complejos_acuosos$Zn2)

lista_iso_CP_Co2 <- sapply(lista_graficas_complejos_CP_opt, is_isomorphic_to,graph2 = lista_graficas_complejos_CP_opt$Co2)
lista_iso_CP_Cr3 <- sapply(lista_graficas_complejos_CP_opt, is_isomorphic_to,graph2 = lista_graficas_complejos_CP_opt$Cr3)
lista_iso_CP_Cu2 <- sapply(lista_graficas_complejos_CP_opt, is_isomorphic_to,graph2 = lista_graficas_complejos_CP_opt$Cu2)
lista_iso_CP_Fe3 <- sapply(lista_graficas_complejos_CP_opt, is_isomorphic_to,graph2 = lista_graficas_complejos_CP_opt$Fe3)
lista_iso_CP_Mn2 <- sapply(lista_graficas_complejos_CP_opt, is_isomorphic_to,graph2 = lista_graficas_complejos_CP_opt$Mn2)
lista_iso_CP_Mn4 <- sapply(lista_graficas_complejos_CP_opt, is_isomorphic_to,graph2 = lista_graficas_complejos_CP_opt$Mn4)
lista_iso_CP_Sc2 <- sapply(lista_graficas_complejos_CP_opt, is_isomorphic_to,graph2 = lista_graficas_complejos_CP_opt$Sc2)
lista_iso_CP_Sc3 <- sapply(lista_graficas_complejos_CP_opt, is_isomorphic_to,graph2 = lista_graficas_complejos_CP_opt$Sc3)
lista_iso_CP_Ti3 <- sapply(lista_graficas_complejos_CP_opt, is_isomorphic_to,graph2 = lista_graficas_complejos_CP_opt$Ti3)
lista_iso_CP_Ni2 <- sapply(lista_graficas_complejos_CP_opt, is_isomorphic_to,graph2 = lista_graficas_complejos_CP_opt$Ni2)
lista_iso_CP_Ni3 <- sapply(lista_graficas_complejos_CP_opt, is_isomorphic_to,graph2 = lista_graficas_complejos_CP_opt$Ni3)
lista_iso_CP_V2 <- sapply(lista_graficas_complejos_CP_opt, is_isomorphic_to,graph2 = lista_graficas_complejos_CP_opt$V2)
lista_iso_CP_V4 <- sapply(lista_graficas_complejos_CP_opt, is_isomorphic_to,graph2 = lista_graficas_complejos_CP_opt$V4)
lista_iso_CP_Zn2 <- sapply(lista_graficas_complejos_CP_opt, is_isomorphic_to,graph2 = lista_graficas_complejos_CP_opt$Zn2)

lista_iso_T_Cr2 <- sapply(lista_graficas_complejos_T_opt, is_isomorphic_to,graph2 = lista_graficas_complejos_T_opt$Cr2)
lista_iso_T_Cr3 <- sapply(lista_graficas_complejos_T_opt, is_isomorphic_to,graph2 = lista_graficas_complejos_T_opt$Cr3)
lista_iso_T_Cu2 <- sapply(lista_graficas_complejos_T_opt, is_isomorphic_to,graph2 = lista_graficas_complejos_T_opt$Cu2)
lista_iso_T_Mn3 <- sapply(lista_graficas_complejos_T_opt, is_isomorphic_to,graph2 = lista_graficas_complejos_T_opt$Mn3)
lista_iso_T_Mn4 <- sapply(lista_graficas_complejos_T_opt, is_isomorphic_to,graph2 = lista_graficas_complejos_T_opt$Mn4)
lista_iso_T_Sc2 <- sapply(lista_graficas_complejos_T_opt, is_isomorphic_to,graph2 = lista_graficas_complejos_T_opt$Sc2)
lista_iso_T_Sc3 <- sapply(lista_graficas_complejos_T_opt, is_isomorphic_to,graph2 = lista_graficas_complejos_T_opt$Sc3)
lista_iso_T_Ti3 <- sapply(lista_graficas_complejos_T_opt, is_isomorphic_to,graph2 = lista_graficas_complejos_T_opt$Ti3)
lista_iso_T_Ni3 <- sapply(lista_graficas_complejos_T_opt, is_isomorphic_to,graph2 = lista_graficas_complejos_T_opt$Ni3)
lista_iso_T_V4 <- sapply(lista_graficas_complejos_T_opt, is_isomorphic_to,graph2 = lista_graficas_complejos_T_opt$V4)
lista_iso_T_Zn2 <- sapply(lista_graficas_complejos_T_opt, is_isomorphic_to,graph2 = lista_graficas_complejos_T_opt$Zn2)

lista_iso_Cu_phen_s0 <- sapply(lista_graficas_Cu_phen, is_isomorphic_to,graph2 = lista_graficas_Cu_phen$s0)
lista_iso_Cu_phen_s1 <- sapply(lista_graficas_Cu_phen, is_isomorphic_to,graph2 = lista_graficas_Cu_phen$s1)
lista_iso_Cu_phen_s2 <- sapply(lista_graficas_Cu_phen, is_isomorphic_to,graph2 = lista_graficas_Cu_phen$s2)
lista_iso_Cu_phen_s3 <- sapply(lista_graficas_Cu_phen, is_isomorphic_to,graph2 = lista_graficas_Cu_phen$s3)
lista_iso_Cu_phen_s4 <- sapply(lista_graficas_Cu_phen, is_isomorphic_to,graph2 = lista_graficas_Cu_phen$s4)
lista_iso_Cu_phen_s5 <- sapply(lista_graficas_Cu_phen, is_isomorphic_to,graph2 = lista_graficas_Cu_phen$s5)
lista_iso_Cu_phen_s6 <- sapply(lista_graficas_Cu_phen, is_isomorphic_to,graph2 = lista_graficas_Cu_phen$s6)
lista_iso_Cu_phen_s7 <- sapply(lista_graficas_Cu_phen, is_isomorphic_to,graph2 = lista_graficas_Cu_phen$s7)
lista_iso_Cu_phen_s8 <- sapply(lista_graficas_Cu_phen, is_isomorphic_to,graph2 = lista_graficas_Cu_phen$s8)
lista_iso_Cu_phen_s9 <- sapply(lista_graficas_Cu_phen, is_isomorphic_to,graph2 = lista_graficas_Cu_phen$s9)
lista_iso_Cu_phen_s10 <- sapply(lista_graficas_Cu_phen, is_isomorphic_to,graph2 = lista_graficas_Cu_phen$s10)
lista_iso_Cu_phen_s11 <- sapply(lista_graficas_Cu_phen, is_isomorphic_to,graph2 = lista_graficas_Cu_phen$s11)
lista_iso_Cu_phen_s12 <- sapply(lista_graficas_Cu_phen, is_isomorphic_to,graph2 = lista_graficas_Cu_phen$s12)
lista_iso_Cu_phen_s13 <- sapply(lista_graficas_Cu_phen, is_isomorphic_to,graph2 = lista_graficas_Cu_phen$s13)
lista_iso_Cu_phen_s14 <- sapply(lista_graficas_Cu_phen, is_isomorphic_to,graph2 = lista_graficas_Cu_phen$s14)
lista_iso_Cu_phen_s15 <- sapply(lista_graficas_Cu_phen, is_isomorphic_to,graph2 = lista_graficas_Cu_phen$s15)
lista_iso_Cu_phen_s16 <- sapply(lista_graficas_Cu_phen, is_isomorphic_to,graph2 = lista_graficas_Cu_phen$s16)
lista_iso_Cu_phen_s17 <- sapply(lista_graficas_Cu_phen, is_isomorphic_to,graph2 = lista_graficas_Cu_phen$s17)
lista_iso_Cu_phen_s18 <- sapply(lista_graficas_Cu_phen, is_isomorphic_to,graph2 = lista_graficas_Cu_phen$s18)
lista_iso_Cu_phen_s19 <- sapply(lista_graficas_Cu_phen, is_isomorphic_to,graph2 = lista_graficas_Cu_phen$s19)
lista_iso_Cu_phen_s20 <- sapply(lista_graficas_Cu_phen, is_isomorphic_to,graph2 = lista_graficas_Cu_phen$s20)

matriz_iso_octaedrico <- rbind(lista_iso_oct_Co2,lista_iso_oct_Cr2,lista_iso_oct_Cr3,lista_iso_oct_Cu2,lista_iso_oct_Mn2,lista_iso_oct_Mn3,lista_iso_oct_Mn4,lista_iso_oct_Fe2,lista_iso_oct_Fe3,lista_iso_oct_Sc2,lista_iso_oct_Sc3,lista_iso_oct_Ni2,lista_iso_oct_Ni3,lista_iso_oct_Ti2,lista_iso_oct_Ti3,lista_iso_oct_V2,lista_iso_oct_V3,lista_iso_oct_V4,lista_iso_oct_Zn2)
matriz_iso_cuadrado_plano <- rbind(lista_iso_CP_Co2,lista_iso_CP_Cr3,lista_iso_CP_Cu2,lista_iso_CP_Fe3,lista_iso_CP_Mn2,lista_iso_CP_Mn4,lista_iso_CP_Sc2,lista_iso_CP_Sc3,lista_iso_CP_Ti3,lista_iso_CP_Ni2,lista_iso_CP_Ni3,lista_iso_CP_V2,lista_iso_CP_V4,lista_iso_CP_Zn2 )
matriz_iso_tetraedrico <- rbind(lista_iso_T_Cr2,lista_iso_T_Cr3,lista_iso_T_Cu2,lista_iso_T_Mn3,lista_iso_T_Mn4,lista_iso_T_Sc2,lista_iso_T_Sc3,lista_iso_T_Ti3,lista_iso_T_Ni3,lista_iso_T_V4,lista_iso_T_Zn2)
matriz_iso_Cu_phen <- rbind(lista_iso_Cu_phen_s0,lista_iso_Cu_phen_s1,lista_iso_Cu_phen_s2,lista_iso_Cu_phen_s3,lista_iso_Cu_phen_s4,lista_iso_Cu_phen_s5,lista_iso_Cu_phen_s6,lista_iso_Cu_phen_s7,lista_iso_Cu_phen_s8,lista_iso_Cu_phen_s9,lista_iso_Cu_phen_s10,lista_iso_Cu_phen_s11,lista_iso_Cu_phen_s12,lista_iso_Cu_phen_s13,lista_iso_Cu_phen_s14,lista_iso_Cu_phen_s15,lista_iso_Cu_phen_s16,lista_iso_Cu_phen_s17,lista_iso_Cu_phen_s18,lista_iso_Cu_phen_s19,lista_iso_Cu_phen_s20)
