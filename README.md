# graficas_atomicas_complejos
estudia la relación entre geometría molecular y gráficas atómicas para predecir propiedades moleculares. Incluye visualizaciones y análisis de QTAIM


# Estudio de graficas atomicas generadas apartir del Laplaciano de la densidad electronica en complejos metálicos.


# Resumen del proyecto:
---------------------
Dentro del marco de la teoría de átomos en moléculas (QTAIM), las gráficas atómicas se utilizan para describir y cuantificar la geometría molecular. En este trabajo, aplicamos técnicas de ciencia de datos a un conjunto de gráficas atómicas generadas para distintas geometrías: extraemos descriptores estructurales de QTAIM y de teoría de grafos, preprocesamos y seleccionamos variables relevantes, y empleamos métodos estadísticos y de aprendizaje automático para identificar patrones y relaciones entre los descriptores y la disposición geométrica. Los resultados permiten evaluar qué características topológicas y electrónicas mejor predicen o explican diferencias geométricas.

# Objetivo general:
-----------------
Aplicar métodos de ciencia de datos a gráficas atómicas derivadas de QTAIM para identificar y cuantificar relaciones entre descriptores topológicos/electrónicos y la geometría molecular.

## Objetivos específicos:
---------------------
1. -Extraer y unificar descriptores QTAIM (densidad electrónica, curvaturas, poblaciones, etc.) y descriptores de grafos (grado, centralidad, clustering, caminos, métricas globales) a partir de salidas de Gaussian16 y AIMAll.

2.- Implementar pipelines reproducibles en R (igraph, R Notebook) y Python (networkx/Network, Jupyter) para la construcción, almacenamiento y visualización de las redes atómicas.

3.- Preprocesar, limpiar y normalizar los conjuntos de descriptores; realizar selección de variables e ingeniería de características para análisis posteriores.

4.- Realizar análisis exploratorio y estadístico (correlaciones, PCA, UMAP/t-SNE) para caracterizar la distribución y relaciones entre descriptores y geometrías.

5.- Desarrollar modelos supervisados y no supervisados (regresión, clasificación, clustering) para predecir o agrupar geometrías según descriptores, y evaluar su desempeño con validación cruzada y métricas apropiadas.
Interpretar resultados mediante técnicas de interpretabilidad (importancia de variables, SHAP/LIME) para identificar descriptores más explicativos de la geometría molecular.

6.- Comparar implementaciones y resultados entre R y Python, documentando diferencias metodológicas y de rendimiento.

7.- Garantizar la reproducibilidad del proyecto mediante notebooks, scripts, gestión de dependencias (requirements/renv/Docker) y documentación clara para regenerar cálculos desde Gaussian16/AIMAll.

# Tabla de contenido:
---------------------
README.md
1.1 Resumen del proyecto
1.2 Objetivos
1.3 Estructura del repositorio
1.4 Requisitos y versiones
1.5 Cómo reproducir los resultados (instrucciones rápidas) / data

2.1 Descripción de los datos
2.2 Formatos y ejemplos
2.3 Enlace a datos grandes o instrucciones para descargarlos / notebooks

3.1 R_Notebook_analysis.ipynb (R Notebook) — generación de gráficas con igraph
3.2 Jupyter_analysis.ipynb — análisis en Python (networkx/Network)
3.3 Notebooks adicionales (EDA, visualización, modelado) / src

4.1 R/
4.1.1 scripts_igraph.R — creación y manipulación de grafos
4.1.2 preprocessing_R.R — preparación y limpieza de datos
4.1.3 descriptors_R.R — extracción de descriptores QTAIM y topológicos
4.2 python/
4.2.1 scripts_networks.py — creación y análisis de redes
4.2.2 preprocessing_py.py — preparación y limpieza de datos
4.2.3 descriptors_py.py — extracción de descriptores y funciones utilitarias / results

5.1 Figuras y visualizaciones
5.2 Tablas de descriptores
5.3 Salidas de modelos y métricas / external

6.1 Gaussian16_outputs/ — archivos de cálculo ( instrucciones para generar)
6.2 AIMAll_outputs/ — archivos de topología (ejemplos) / env

7.1 requirements.txt (Python)
7.2 renv.lock or DESCRIPTION (R) / install.R
7.3 Dockerfile / environment.yml (opcional) / tests

8.1 Unit tests para scripts R y Python
8.2 Test data y casos de prueba / docs

9.1 Manual de uso detallado
9.2 Notas sobre metodologías (QTAIM, teoría de grafos)
9.3 Informe técnico / publicación

LICENSE

CITATION.cff

CONTRIBUTING.md
.github
13.1 ISSUE_TEMPLATE.md
13.2 PULL_REQUEST_TEMPLATE.md
13.3 workflows/ — CI (GitHub Actions) para tests y linting
CHANGELOG.md
ANNEXES
15.1 fragmentos de código destacables
15.2 tablas completas de descriptores
15.3 instrucciones para regenerar datos desde Gaussian16/AIMAll

