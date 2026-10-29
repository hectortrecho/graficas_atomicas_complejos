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

'''
Cobre: [Ar] 3d10 4s1 ; Z=29
Cu2+: 27 electrones
N: [He] 2s2 2p3 ; Z=7
H: 1s1 ; Z=1 
C: [He] 2s2 2p2 ; Z=6
O: [He] 2s2 2p4 ; Z=8
Multiplicity=2S+1; no. of unpaired electrons of the molecule (as a whole system);S=n/2
Dic = {Molecula : [Geometria,carga,electrones,spin,SMILE,ligante,Nivel de teoria]}
'''

dic_moleculas_estudio = {
    "Cu(NH3)2" : ["Lineal",2,47,1,"[Cu(NH3)2]2+","NH3","cc-PVTDZ"],
    "Cu(NH3)3" : ["Trigonal_plana",2,57,1,"[Cu(NH3)3]2+","NH3","cc-PVTDZ"],
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



