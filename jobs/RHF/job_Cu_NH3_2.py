# Title: CuH6N2 | Single Point | RHF/cc-pvdz
from pyscf import gto,scf
mol = gto.Mole()
mol.atom = '''
   Cu     0.000000    0.000000    0.000000
   N      1.950000    0.000000    0.000000
   H      2.280000    0.940000    0.000000
   H      2.280000   -0.470000    0.814000
   H      2.280000   -0.470000   -0.814000
   N     -1.950000    0.000000    0.000000
   H     -2.280000    0.940000    0.000000
   H     -2.280000   -0.470000    0.814000
   H     -2.280000   -0.470000   -0.814000
'''
mol.basis = 'ccpvdz'
mol.charge = 2
mol.spin = 1
mol.build()
mf = scf.RHF(mol)
mf.kernel()
