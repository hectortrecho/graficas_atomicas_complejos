# Title: Cl3Cu | Single Point | RHF/cc-pvdz
from pyscf import gto,scf
mol = gto.Mole()
mol.atom = '''
   Cu     0.000000    0.000000    0.000000
   Cl     2.150000    0.000000    0.000000
   Cl    -0.750000    1.900000   -0.000000
   Cl    -0.750000   -1.900000    0.000000
'''
mol.basis = 'ccpvdz'
mol.charge = 2
mol.spin = 0
mol.build()
mf = scf.RHF(mol)
mf.kernel()
