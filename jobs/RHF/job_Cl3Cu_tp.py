# Title: Cl3Cu | Trigonal planar | Single Point | RHF/STO-3G
from pyscf import gto,scf
mol = gto.Mole()
mol.atom = '''
   Cu     0.000000    0.000000    0.000000
   Cl     2.150000    0.000000    0.000000
   Cl    -1.075000    1.861000    0.000000
   Cl    -1.075000   -1.861000    0.000000
'''
mol.basis = 'ccpvdz'
mol.charge = 2
mol.spin = 0
mol.build()
mf = scf.RHF(mol)
mf.kernel()
