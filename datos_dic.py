#!/usr/bin/python
#Librerias
import datos_networkx
import pandas as pd
import networkx as nx
from torch_geometric.utils import to_networkx
from networkx.algorithms import bipartite
from numpy.linalg import inv,matrix_rank

#Diccionarios Vacios, Loops al final.
dic_grados = {}                                #Diccionario con lista de grados de la grafica devuelto como un diccionario
dic_max_grados = {}                            #Diccionario par (nodo:grado vertice)
dic_mat_graficas = {}                          #Diccionario 'grafica': Matriz
dic_det_mat_graficas = {}                      #Dicionario de determinante de las graficas de conectividad
dic_inv_mat_graficas = {}                      #Diccionario de matrices inversas de las graficas.
dic_bipartite = {}                             #Diccionario de si la grafica es bipartita
dic_bridge = {}                                #Diccionario de si la grafica tiene un puente
dic_deg_centrality = {}                        #Dicionario con los valores del grado de centralidad por nodos.
dic_index_estrada = {}                         #Diccionario con el valor del indice de estrada.
dic_cliques = {}                               #Diccionario con el valor del numero maximo de cliques en la grafica.
dic_triangulos = {}                            #Diccionario con el numero de triangulos en la graficas.
dic_cluster = {}                               #Diccionario con el coeficiente de cluster de las graficas.
dic_cromatico = {}                             #Diccionario con el numero cromatico de graficas.
dic_weiner = {}                                #Diccionario con el indice de weiner.
dic_gutman = {}                                #Diccionario con el indice de gutman.
dic_pol_cromatico = {}                         #Diccionario con la representacion del polinomio cromatico
dic_pol_tutte = {}                             #Diccionario con la representacion del polinomio tutte
dic_eigen = {}                                 #Diccionario con los valores propios.
dic_rango = {}                                 #Diccionario con el rango de la matrices de conectividad de la grafica.
dic_mat_AA = {}                                #Diccionario producto matricial de AA.
dic_mat_AAA = {}                               #Diccionario producto matricial de AAA.
dic_mat_AAAA = {}                              #Diccionario producto matricial de AAAA.
dic_mat_AAAAA = {}                             #Diccionario producto matricial de AAAAA.
dic_densidad = {}                              #Diccionario de la densidad de las graficas.
dic_euleriana = {}                             #Diccionario con valores de falso o verdadero de si la grafica es euleriana
dic_s_metric = {}                              #Diccionario con los valores de  deg(u) * deg(v).
dic_eff_size = {}                              #Diccionario con el tamano efectivo de cada nodo.
dic_mat_lap = {}                               #Diccionario con matrices laplacianas de cada grafica.
dic_det_mat_lap = {}                           #Diccionario con determinante de la matrices laplacianas de cada grafica.
dic_alg_con = {}                               #Diccionario con el valor de conectividad algebraico de las graficas.
dic_mat_inc = {}                               #Diccionario de matriz de conectividad en forma matriz de incidencia.
dic_lap_spe = {}                               #Diccionario de array con valores propios de la matriz laplaciana de la grafica.
dic_data = {}                                  #Diccionario de dataset usado en torch con attributos en la estructura de datos.
dic_snap_aggregation = {}                      #Diccionario de resumen de graficas agregadas por attributos.
dic_eccentricity = {}
dic_radius = {}
dic_diameter = {}
dic_center = {}
dic_girth = {}
dic_list_cliques = {}
dic_number_cliques = {}
list_dic = [dic_max_grados,dic_det_mat_graficas,dic_det_mat_lap,dic_deg_centrality,dic_rango,dic_gutman,dic_weiner,dic_bridge,dic_alg_con,dic_cluster,dic_eff_size,dic_s_metric,dic_bipartite,dic_euleriana,dic_max_grados,dic_cliques,dic_deg_centrality,dic_index_estrada,dic_densidad]
def AppendList(names,matrices,dic):
    '''Loop para crear diccionario apartir de las listas [nombre,matrices]'''
    for key,value in zip(names,matrices):
        dic[key] = value
#Loop para obtener lista de los cliques de la grafica.
for grafica in dic_graficas.values():
    try:
        dic_number_cliques[grafica] = list(nx.number_of_cliques(grafica))
    except:
        continue
dic_number_cliques = dict(zip(dic_graficas.keys(),list(dic_number_cliques.values())))
#Loop para obtener lista de los cliques de la grafica.
for grafica in dic_graficas.values():
    try:
        dic_list_cliques[grafica] = list(nx.enumerate_all_cliques(grafica))
    except:
        continue
dic_list_cliques = dict(zip(dic_graficas.keys(),list(dic_list_cliques.values())))
#Loop para obtener el girth (longitud de su ciclo mas pequeno) de toda las graficas.
for grafica in dic_graficas.values():
    dic_girth[grafica] = nx.girth(grafica)
dic_girth = dict(zip(dic_graficas.keys(),list(dic_girth.values())))
#Loop para obtener el centro de toda las graficas.
for grafica in dic_graficas.values():
    dic_center[grafica] = nx.center(grafica)
dic_center = dict(zip(dic_graficas.keys(),list(dic_center.values())))
#Loop para obtener el diametro de toda las graficas.
for grafica in dic_graficas.values():
    dic_diameter[grafica] = nx.diameter(grafica)
dic_diameter = dict(zip(dic_graficas.keys(),list(dic_diameter.values())))
#Loop para obtener el radio de toda las graficas.
for grafica in dic_graficas.values():
    dic_radius[grafica] = nx.radius(grafica)
dic_radius = dict(zip(dic_graficas.keys(),list(dic_radius.values())))
#Loop para obtener la eccentricity de toda las graficas.
for grafica in dic_graficas.values():
    dic_eccentricity[grafica] = nx.eccentricity(grafica)
dic_eccentricity = dict(zip(dic_graficas.keys(),list(dic_eccentricity.values())))
#Loop para obtener el radio de toda las graficas.
for grafica in dic_graficas.values():
    dic_radius[grafica] = nx.radius(grafica)
dic_radius = dict(zip(dic_graficas.keys(),list(dic_radius.values())))
#Loop para obtener grado de todos los nodos.
for grafica in dic_graficas.values():
    dic_grados[grafica] = grafica.degree
dic_grados = dict(zip(dic_graficas.keys(),list(dic_grados.values())))
#Loop para obtener par (nodo:grado vertice)
for grafica in dic_graficas.values():
    dic_max_grados[grafica] = max(grafica.degree)
dic_max_grados = dict(zip(dic_graficas.keys(),list(dic_max_grados.values())))
#Loop para crear diccionario de determinantes de graficas.
for matriz in dic_mat_graficas:
    dic_det_mat_graficas[matriz] = np.linalg.det(dic_mat_graficas[matriz])
#Loop para crear diccionario de matrices inversas si aplica.
for matriz in dic_mat_graficas:
    try:
        dic_inv_mat_graficas[matriz] = inv(dic_mat_graficas[matriz])
    except:
        continue
#Loop para crear un diccionario si la grafica es bipartita
for grafica in dic_graficas.values():
    dic_bipartite[grafica] = bipartite.is_bipartite(grafica)
dic_bipartite = dict(zip(dic_graficas.keys(),list(dic_bipartite.values())))
#Loop para crear un diccionario de la conectividad algebraica.
'''for grafica in dic_graficas.values():
    try:
        dic_alg_con[grafica] = nx.algebraic_connectivity(grafica)
    except:
        continue
dic_alg_con = dict(zip(dic_graficas.keys(),list(dic_alg_con.values())))'''
#Loop para crear un diccionar con los polimio cromatico y tutte en sympy
'''for grafica in dic_graficas.values():
    dic_pol_cromatico[grafica] = nx.chromatic_polynomial(grafica)
for grafica in dic_graficas.values():
    dic_pol_tutte[grafica] = nx.tutte_polynomial(grafica)
dic_pol_cromatico = dict(zip(dic_graficas.keys(),list(dic_pol_cromatico.values())))
dic_pol_tutte = dict(zip(dic_graficas.keys(),list(dic_pol_tutte.values())))'''
#Loop para crear un diccionar de si una grafica tiene puente
for grafica in dic_graficas.values():
    dic_bridge[grafica] = nx.has_bridges(grafica)
dic_bridge = dict(zip(dic_graficas.keys(),list(dic_bridge.values())))
#Loop para crear un diccionar con los grados de la centralidad.
for grafica in dic_graficas.values():
    dic_deg_centrality[grafica] = nx.degree_centrality(grafica)
dic_deg_centrality = dict(zip(dic_graficas.keys(),list(dic_deg_centrality.values())))
#Loop para crear un diccionar con el indice estrada.
for grafica in dic_graficas.values():
    dic_index_estrada[grafica] = nx.estrada_index(grafica)
dic_index_estrada = dict(zip(dic_graficas.keys(),list(dic_index_estrada.values())))
#Loop para crear un diccionar con numero maximo de cliques.
for grafica in dic_graficas.values():
    dic_cliques[grafica] = max(len(c) for c in nx.find_cliques(grafica))
dic_cliques = dict(zip(dic_graficas.keys(),list(dic_cliques.values())))
#Loop para crear un diccionar con numero triangulos en la grafica.
for grafica in dic_graficas.values():
    dic_triangulos[grafica] = nx.triangles(grafica, 0)
dic_triangulos = dict(zip(dic_graficas.keys(),list(dic_triangulos.values())))
#Loop para crear un diccionar con el coeficiente de cluster de las graficas.
for grafica in dic_graficas.values():
    dic_cluster[grafica] = nx.clustering(grafica, 0)
dic_cluster = dict(zip(dic_graficas.keys(),list(dic_cluster.values())))
#Loop para crear un diccionar con el numero cromatico.
for grafica in dic_graficas.values():
    dic_cromatico[grafica] = nx.coloring.greedy_color(grafica, strategy="largest_first")
dic_cromatico = dict(zip(dic_graficas.keys(),list(dic_cromatico.values())))
#Loop para crear un diccionar con el indice weiner.
for grafica in dic_graficas.values():
    dic_weiner[grafica] = nx.wiener_index(grafica)
dic_weiner = dict(zip(dic_graficas.keys(),list(dic_weiner.values())))
#Loop para crear un diccionar con el indice de gutman.
for grafica in dic_graficas.values():
    dic_gutman[grafica] = nx.gutman_index(grafica)
dic_gutman = dict(zip(dic_graficas.keys(),list(dic_gutman.values())))
#Loop para crear diccionario valores propios de graficas.
for matriz in dic_mat_graficas:
    dic_eigen[matriz] = np.linalg.eig(dic_mat_graficas[matriz])
#Loop para crear diccionario rango de la matriz de conectividad.
for matriz in dic_mat_graficas:
    dic_rango[matriz] = matrix_rank(dic_mat_graficas[matriz])
#Loop para crear diccionario camino de los vertices de la matriz de conectividad.
for matriz in dic_mat_graficas:
    dic_mat_AA[matriz] = np.matmul(dic_mat_graficas[matriz], dic_mat_graficas[matriz])
for matriz in dic_mat_AA:
    dic_mat_AAA[matriz] = np.matmul(dic_mat_AA[matriz], dic_mat_graficas[matriz])
for matriz in dic_mat_AAA:
    dic_mat_AAAA[matriz] = np.matmul(dic_mat_AAA[matriz], dic_mat_graficas[matriz])
for matriz in dic_mat_AAAA:
    dic_mat_AAAAA[matriz] = np.matmul(dic_mat_AAAA[matriz], dic_mat_graficas[matriz])
#Loop para crear un diccionar con el indice weiner.
for grafica in dic_graficas.values():
    dic_densidad[grafica] = nx.density(grafica)
dic_densidad = dict(zip(dic_graficas.keys(),list(dic_densidad.values())))
#Loop para crear un diccionario si la grafica es euleriana
for grafica in dic_graficas.values():
    dic_euleriana[grafica] = nx.is_eulerian(grafica)
dic_euleriana = dict(zip(dic_graficas.keys(),list(dic_euleriana.values())))
#Loop para crear un diccionario con los valore de s-metric
for grafica in dic_graficas.values():
    dic_s_metric[grafica] = nx.s_metric(grafica)
dic_s_metric = dict(zip(dic_graficas.keys(),list(dic_s_metric.values())))
#Loop para crear un diccionario con el tamano efectivo de cada nodo.
for grafica in dic_graficas.values():
    dic_eff_size[grafica] = nx.effective_size(grafica)
dic_eff_size = dict(zip(dic_graficas.keys(),list(dic_eff_size.values())))
#Loop para crear un diccionar con matrices laplacianas de cada graficas.
for grafica in dic_graficas.values():
    dic_mat_lap[grafica] = nx.laplacian_matrix(grafica).toarray()
dic_mat_lap = dict(zip(dic_graficas.keys(),list(dic_mat_lap.values())))
#Loop para crear un diccionar con el determinante de matrices laplacianas de cada graficas.
for matriz in dic_mat_lap:
    dic_det_mat_lap[matriz] = np.linalg.det(dic_mat_lap[matriz])
#Loop para crear un diccionario con las matrices de conectividad en representacion matriz de incidencia.
for grafica in dic_graficas:
    dic_mat_inc[grafica] = nx.incidence_matrix(dic_graficas[grafica]).toarray()
dic_mat_inc = dict(zip(dic_graficas.keys(),list(dic_mat_inc.values())))
#Loop para crear un diccionario con array de specto de las matrices laplacianas de las graficas.
for grafica in dic_graficas:
    dic_lap_spe[grafica] = nx.laplacian_spectrum(dic_graficas[grafica])
dic_lap_spe = dict(zip(dic_graficas.keys(),list(dic_lap_spe.values())))

#Creando el diccionario de los datos de torch que tienen atributos en la estructura.
names_list = [
    'Co2_CP','Co2_O','Co2_T','Cr2_CP','Cr2_O','Cr2_T','Cr3_CP','Cr3_T','Cu2_CP','Cu2_O','Cu2_T','Fe2_CP','Fe2_O','Fe2_T','Fe3_CP','Fe3_T','Mn2_CP',
    'Mn2_O','Mn2_T','Mn3_CP','Mn3_T','Mn4_CP','Mn4_T','Ni2_CP','Ni2_O','Ni2_T','Ni3_CP','Ni3_O','Ni3_T','Sc2_CP','Sc2_T','Sc3_CP','Sc3_T','Ti2_CP',
    'Ti2_O','Ti2_T','Ti3_CP','Ti3_T','V2_CP','V2_O','V2_T','V3_CP','V3_T','V4_CP','V4_T','Zn2_CP','Zn2_O','Zn2_T','s0','s1','s2','s3','s4','s5','s6',
    's7','s8','s9','s10','s11','s12','s13','s14','s15','s16','s17','s18','s19','s20']
list_graphs = []
for graph in list_of_graphs:
    list_graphs.append(to_networkx(graph))
#Loop para crear diccionario con graficas que tienen attributos selecionados.
for key,value in zip(names_list,list_graphs):
    dic_data[key] = value

#Ejecucion de las funciones.
AppendList(names_matrices_adyacencia,lista_matrices_adyacencia,dic_mat_graficas)

list_dic_escalar = [dic_max_grados,dic_det_mat_graficas,dic_det_mat_lap,dic_deg_centrality,dic_rango,dic_gutman,dic_weiner,dic_bridge,dic_cluster,dic_eff_size,dic_s_metric,dic_bipartite,dic_euleriana,dic_max_grados,dic_cliques,dic_deg_centrality,dic_index_estrada,dic_densidad]
nombre = 'dic_max_grados,dic_det_mat_graficas,dic_det_mat_lap,dic_deg_centrality,dic_rango,dic_gutman,dic_weiner,dic_bridge,dic_cluster,dic_eff_size,dic_s_metric,dic_bipartite,dic_euleriana,dic_max_grados,dic_cliques,dic_deg_centrality,dic_index_estrada,dic_densidad'
nombres = nombre.split(",")
df_prop_graph = pd.DataFrame(list_dic_escalar,index = nombres) #Dataframe con propiedaddes de grafica.

#Diccionario de propiedades calculadas en formato {'Columna':list()}
datos = pd.read_excel(r'~\Documents\codigo_python\investigacion_graficas_atomicas\datos\datos.xlsx') #dataframe con datos conatenados de atomo.xls y CP.xls
dic_datos = datos.to_dict(orient='index')
dataframe_CP = pd.read_excel(r'~\Documents\codigo_python\investigacion_graficas_atomicas\datos\dic_CP.xlsx')#['Clase','Atomic LapCP #','LapCP Type','DistFromNuc','Rho','DelSqRho','Ven(r)','Vrep(r)','G(r)','K(r)']
dataframe_atom = pd.read_excel(r'~\Documents\codigo_python\investigacion_graficas_atomicas\datos\dic_atom.xlsx')#['Atom','clase','N,'%Loc','%Deloc','Q1','Q2','Q3','Q_XX','Q_YY','Q_ZZ','|Q|','DI_Bond','DI_NonBond','q','L','K','Mu_Intra','Ee','T','Ven']
dataframe_mean = pd.read_excel(r'~\Documents\codigo_python\investigacion_graficas_atomicas\datos\dic_mean.xlsx')#['Clase','Atomic LapCP #','LapCP Type','DistFromNuc','Rho','DelSqRho','Ven(r)','Vrep(r)','G(r)','K(r)']

#Diccionario de las propiedades [CP,atom,mean,graph]
dic_CP = dataframe_CP.to_dict(orient='index') #{INDEX:{'Prop':Valor, ...}}
dic_atom = dataframe_atom.to_dict(orient='index') #{INDEX:{'Prop':Valor, ...}}
dic_mean = dataframe_mean.to_dict(orient='index') #{INDEX:{'Prop':Valor, ...}}

#Creando diccionario de propiedades.
list_dic_propiedades_grafica = [dic_max_grados,dic_det_mat_graficas,dic_inv_mat_graficas,dic_bipartite,dic_bridge,dic_deg_centrality,dic_index_estrada,dic_cliques,dic_triangulos,dic_cluster,dic_cromatico,dic_weiner,dic_gutman,dic_eigen,dic_rango,dic_densidad,dic_euleriana,dic_s_metric,dic_eff_size,dic_mat_lap,dic_det_mat_lap,dic_mat_inc,dic_lap_spe,dic_eccentricity,dic_radius,dic_diameter,dic_center,dic_girth,dic_list_cliques]
names = 'dic_max_grados,dic_det_mat_graficas,dic_inv_mat_graficas,dic_bipartite,dic_bridge,dic_deg_centrality,dic_index_estrada,dic_cliques,dic_triangulos,dic_cluster,dic_cromatico,dic_weiner,dic_gutman,dic_eigen,dic_rango,dic_densidad,dic_euleriana,dic_s_metric,dic_eff_size,dic_mat_lap,dic_det_mat_lap,dic_mat_inc,dic_lap_spe,dic_eccentricity,dic_radius,dic_diameter,dic_center,dic_girth,dic_list_cliques'
names = names.split(",")
df_propiedades_grafica = pd.DataFrame(list_dic_propiedades_grafica,index = names)
df_propiedades_grafica_T = df_propiedades_grafica.transpose()
clase = [1,2,1,2,2,5,1,2,1,5,7,5,1,1,3,5,4,3,6,4,2,2,2,3,1,2,1,5,1,1,1,1,2,4,3,1,4,9,2,2,2,2,2,8,1,1,2,1,2,2,2,2,3,1,5,9,2,1,2,2,3,4,4,4,5,5,4,6,7,4,4,4,4,8,9,10,2,7]
df_propiedades_grafica_T = df_propiedades_grafica_T.assign(clase=clase)
df_propiedades_grafica.loc['clase'] = clase
dic_propiedades = df_propiedades_grafica.to_dict(orient='index')#{INDEX:{'Prop':Valor, ...}}
dic_propiedades_T = df_propiedades_grafica_T.to_dict(orient='index')#{INDEX:{'Prop':Valor, ...}}
df_propiedades_graficas_atomicas = pd.read_excel(r'~\Documents\codigo_python\investigacion_graficas_atomicas\datos\propiedades_graficas_atomicas.xlsx')



