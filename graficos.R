library(tidyverse)
library(ggplot2)
library(ggpie)


df <- data.frame(clase = c(1:14),count = c(17,21,7,5,5,1,1,1,2,8,3,3,1,1),stringsAsFactors = FALSE)


ggpie3D(data = df, group_key = "clase", count_type = "count", tilt_degrees = -10, 
        start_degrees = 180, label_size=2) + 
  ggtitle("Distribucion de las graficas atomicas") + 
  theme(plot.title = element_text(hjust = 0.5))

#grafica de estado de oxidacion vs clase

df_clase <- data.frame(
  estado_oxidacion = c("Co2","Cr2","Cr3","Cu2","Fe2","Fe3","Mn2","Mn3","Mn4","Ni2","Ni3","Sc2","Sc3","Ti2","Ti3","V2","V3","V4","Zn2"),
  Octaedro= c(1,5,1,5,3,2,2,3,1,1,2,1,2,2,1,1,2,1,2),
  Cuadrado_plano = c(1,2,1,5,1,5,6,2,1,5,1,4,4,4,2,1,2,2,5),
  Tetraedro = c(2,2,2,7,1,4,4,2,2,1,1,3,9,9,8,2,2,3,9),
  stringsAsFactors = FALSE
)
pivot_df <-
  df_clase %>% 
  pivot_longer(!estado_oxidacion)


ggplot(pivot_df,aes(x=estado_oxidacion,y=value,group=name,color=name)) + 
  scale_colour_manual(values = c("red", "blue","green")) +
  scale_y_continuous(breaks=c(1:14)) +
  geom_line() + 
  geom_point() +
  ggtitle("Estado de Oxidacion vs Clase de grafica") +
  xlab("Estado de Oxidacion del Metal de Transicion") + ylab("Clase de Grafica")


# Rpolyhedra
# URL:https://ropensci.github.io/Rpolyhedra/
# URL:extdata/www.netlib.org/polyhedra/index.html
library(Rpolyhedra)
library(rgl)
library(knitr)
#switchToFullDatabase()

#getAvailablePolyhedra()  #lista de poliedros disponibles en la base.

cube <- getPolyhedron(source = "netlib", polyhedron.name = "cube")

polyhedra.2.draw <- getAvailablePolyhedra(source = "netlib")
polyhedra.2.draw <- polyhedra.2.draw %>%
  filter(scraped.name %in%
           c("tetrahedron", "octahedron", "cube",
             "icosahedron", "dodecahedron"))

n <- nrow(polyhedra.2.draw)
polyhedron.colors <- rainbow(n)
polyhedron.scale <- 5

open3d()

par3d(FOV = 1)
bg3d( sphere =FALSE, fogtype = "none", color=c("black"))
view3d(theta = 0, phi=0, zoom=0.8, fov=1)

for (i in seq_len(n)) {
  # Obtain polyhedron
  polyhedron.row <- polyhedra.2.draw[i,]
  polyhedron.name <- polyhedron.row$scraped.name
  polyhedron <- getPolyhedron(source = polyhedron.row$source, polyhedron.name)
  
  # Setup angles, position into transformationMatrix
  current.angle <- i/n * 2 * pi
  tm <- rotationMatrix(current.angle, 1, 0, 0)
  x.pos <- round(polyhedron.scale * sin(current.angle), 2)
  y.pos <- round(polyhedron.scale * cos(current.angle), 2)
  tm <- tm %*% translationMatrix(x.pos, y.pos, 0)
  
  # Render
  print(paste("Drawing ", polyhedron.name, " rotated ", round(current.angle, 2),
              " in (1,0,0) axis. Translated to (", x.pos, ",", y.pos, ",0)",
              " with color ", polyhedron.colors[i], sep = ""))
  shape.rgl <- polyhedron$getRGLModel(transformation.matrix = tm)
  shade3d(shape.rgl, color = polyhedron.colors[i])
}

## [1] "Drawing tetrahedron rotated 1.26 in (1,0,0) axis. Translated to (4.76,1.55,0) with color #FF0000"
## [1] "Drawing octahedron rotated 2.51 in (1,0,0) axis. Translated to (2.94,-4.05,0) with color #CCFF00"
## [1] "Drawing cube rotated 3.77 in (1,0,0) axis. Translated to (-2.94,-4.05,0) with color #00FF66"
## [1] "Drawing icosahedron rotated 5.03 in (1,0,0) axis. Translated to (-4.76,1.55,0) with color #0066FF"
## [1] "Drawing dodecahedron rotated 6.28 in (1,0,0) axis. Translated to (0,5,0) with color #CC00FF"