from pathlib import Path
from pyscf import gto, dft
from pyscf.tools.wfn_format import write_mo

xyz_path = Path(
    "~/projects/graficas_atomicas_complejos/avogadro/CuNH2_6_octaedro.xyz"
).expanduser()

lines = [line.strip() for line in xyz_path.read_text().splitlines() if line.strip()]

# Standard XYZ starts with an atom count and a comment line.
try:
    natoms = int(lines[0])
    atom_lines = lines[2 : 2 + natoms]
    if len(atom_lines) != natoms:
        raise ValueError("XYZ atom count does not match coordinate lines")
except ValueError:
    # Headerless XYZ: coordinate rows start on the first line.
    atom_lines = lines

mol = gto.M(
    atom="\n".join(atom_lines),
    basis="cc-pvtz",
    charge=2,
    spin=1,  # Singlet; choose the intended electronic state
    unit="Angstrom",
)

mf = dft.RKS(mol, xc="pbe0")
mf.kernel()

if not mf.converged:
    raise RuntimeError("PBE0 calculation did not converge")

occupied = mf.mo_occ > 0
with open("CuNH2_6_octaedro.wfn", "w") as f:
    write_mo(
        f,
        mol,
        mf.mo_coeff[:, occupied],
        mf.mo_energy[occupied],
        mf.mo_occ[occupied],
    )
