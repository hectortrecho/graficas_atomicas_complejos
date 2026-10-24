#libreria
from pyscf import gto
#======================================
# Moleculas
#======================================
'''
Moleculas de estudio:
** LINEAR: [Cu(NH_{3})_{2}]^{+}
** TETRAHEDRAL: [CuCl_{4}]^{2-}
** TRIGONAL PLANAR: [CuCl_{3}]^{2-}
** SQUARE PLANAR: [Cu(NH_{3})_{4})]^{2+}
** TRIGONAL BYPIRAMIDAL: [Cu(H_{2}O)_{5}]^{2+}
** OCTAHEDRAL: [Cu(H_{2}O)_{6}]^{2+} ; [Cu(NH_{3})_{6}]^{2+} ; [CuCl_{6}]^{4-}
** JAHN-TELLER ELONGATED: [Cu(H_{2}O)_{5}Cl]^{+}
'''
# Dic = {Molecula : [Geometria,carga,SMILE,Nivel de teoria]}
obj_estudio = {
    "Cu(NH3)2" : ["Lineal",2,"[Cu(NH3)2]2+","cc-PVTDZ"],
    "" : 
}
#=====================================
# Proceedimiento:
#=====================================
'''
1.- Crear posiciones iniciales junto con sus coorenadas.(.xyz)
2.- Optimización geometrica de moleculas.
3.- Generar formato para analisis. (.wfx)
4.- Analisis topologicos para identificar los puntos criticos. (critic2)
5.- Generar graficas visuales usando script de python para construir objetos.
'''
#====================================
# Leyendo xyz
#====================================
with open("cucl3_tshape.xyz") as f:
    lines = f.readlines()[2:]

mol = gto.Mole()
mol.atom = "\n".join(lines)
mol.basis = "def2-svp"
mol.charge = -2
mol.spin = 1
mol.build()



