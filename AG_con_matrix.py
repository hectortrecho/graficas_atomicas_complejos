import os
import glob
import re
import argparse
import numpy as np
import pandas as pd

# Minimal CP dataclass
class CP:
    def __init__(self, internal_num=None, Type=None, Coords=None, DistFromNuc=None, Rho=None, DelSqRho=None):
        self.internal_num = internal_num
        self.Type = Type
        self.Coords = Coords
        self.DistFromNuc = DistFromNuc
        self.Rho = Rho
        self.DelSqRho = DelSqRho
        self.interaction = []

    def read_raw_from_file(self, file):
        """
        Reads following lines from an open .agpviz file handle to populate fields.
        Expects the same ordering used in original script:
         - internal_num on next line (3rd token)
         - Type on next line (3rd token)
         - Coords on next line after '='
         - DistFromNuc on next line after '='
         - Rho on next line after '='
         - DelSqRho on next line after '='
        This will raise if file format differs.
        """
        self.internal_num = int(file.readline().split()[2])
        self.Type = file.readline().split()[2]
        self.Coords = file.readline().split("=", 1)[1].split()
        self.DistFromNuc = float(file.readline().split("=", 1)[1])
        self.Rho = float(file.readline().split("=", 1)[1])
        self.DelSqRho = float(file.readline().split("=", 1)[1])
        self.Coords = [float(x) for x in self.Coords]

    def build_interaction_from_unions(self, unions):
        """Populate self.interaction from list of [a,b] unions (pairs of internal_nums)."""
        inter = []
        for u in unions:
            if u[0] == self.internal_num:
                inter.append(u[1])
            elif u[1] == self.internal_num:
                inter.append(u[0])
        self.interaction = inter


class AdjacencyBuilder:
    """
    Parses .agpviz files and constructs adjacency matrices for vertex CPs (Type == '(3,+3)').
    Two vertices are adjacent if there exists at least one edge CP (Type == '(3,+1)')
    whose interaction list contains both vertex internal numbers.
    """

    def __init__(self, agpviz_path):
        self.path = agpviz_path
        self.nom = None
        self.atom = None
        self.CPs = []     # list of CP objects
        self.unions = []  # list of [a,b] pairs from 'Starting CP#' sections

    def extract_names(self):
        """Extracts base name and atom symbol from file path similar to original _nom()."""
        # name before "_atomicfiles"
        self.nom = self.path.split("_atomicfiles")[0].split(os.sep)[-1]
        # atom guessed from parent folder name (split digits)
        parent = re.split(r"[\\/]", self.path)[-2]
        self.atom = re.split(r"\d", parent)[0]

    def read_agpviz(self):
        """
        Reads the .agpviz file and fills CPs and unions.
        Logic:
         - When a line contains "<CP of DelSqRho>" -> read following block into a CP
         - When a line contains "Starting CP#" -> read current and next line to form a union [ini, fin]
        """
        with open(self.path, "r") as f:
            for line in f:
                if "<CP of DelSqRho>" in line:
                    cp = CP()
                    cp.read_raw_from_file(f)
                    self.CPs.append(cp)
                if "Starting CP#" in line:
                    # original expected: "Starting CP# <num>" then next line has "Ending CP# <num>"
                    parts = line.split()
                    if len(parts) >= 4:
                        try:
                            ini = int(parts[3])
                        except ValueError:
                            continue
                        nextline = f.readline()
                        parts2 = nextline.split()
                        if len(parts2) >= 4:
                            try:
                                fin = int(parts2[3])
                                self.unions.append([ini, fin])
                            except ValueError:
                                continue

    def build_interactions(self):
        """Populate each CP.interaction from unions list."""
        for cp in self.CPs:
            cp.build_interaction_from_unions(self.unions)

    def vertex_and_edge_lists(self):
        """Return (vertices_list, edges_list) where types are matched by strings in file."""
        v = [cp for cp in self.CPs if cp.Type == "(3,+3)"]
        e = [cp for cp in self.CPs if cp.Type == "(3,+1)"]
        return v, e

    def build_adjacency_via_e(self):
        """
        Build adjacency matrix for vertices via edges.
        Returns:
         - adj: NxN numpy int array with 1 if adjacent, 0 otherwise
         - v_list: ordered list of vertex CPs corresponding to rows/cols
        Safe guards:
         - If no vertices, returns empty (0x0) array.
         - If no edges, returns zero matrix.
        """
        v, e = self.vertex_and_edge_lists()
        N = len(v)
        if N == 0:
            return np.zeros((0, 0), dtype=int), v

        # map internal_num -> index
        v_index = {cp.internal_num: idx for idx, cp in enumerate(v)}
        adj = np.zeros((N, N), dtype=int)

        # For each edge CP, find which vertices it references; mark adjacency for every pair
        for ecp in e:
            inter_vertices = [x for x in getattr(ecp, "interaction", []) if x in v_index]
            # unique
            inter_vertices = list(set(inter_vertices))
            L = len(inter_vertices)
            if L < 2:
                continue
            for i in range(L):
                for j in range(i + 1, L):
                    ii = v_index[inter_vertices[i]]
                    jj = v_index[inter_vertices[j]]
                    adj[ii, jj] = 1
                    adj[jj, ii] = 1

        return adj, v

def process_folder(root_folder, out_base):
    """
    Walks folders matching pattern "<root_folder>/*_atomicfiles" and processes any .agpviz files found.
    Saves per-file adjacency matrix to out_base + '_' + <nom> + '_adj.csv' and creates summary CSV.
    """
    rows = []
    for folder in glob.glob(os.path.join(root_folder, "*_atomicfiles")):
        for agfile in glob.glob(os.path.join(folder, "*.agpviz")):
            ab = AdjacencyBuilder(agfile)
            ab.extract_names()
            ab.read_agpviz()
            ab.build_interactions()
            adj, vlist = ab.build_adjacency_via_e()
            # save adjacency matrix (square NxN) even if N==0
            outname = f"{out_base}_{ab.nom}_adj.csv"
            np.savetxt(outname, adj, fmt="%d", delimiter=",")
            rows.append({"Name": ab.nom, "Atom": ab.atom, "N_vertices": len(vlist), "AdjFile": outname})
            print(f"Processed {agfile}: vertices={len(vlist)}; adj saved to {outname}")

    if rows:
        df = pd.DataFrame(rows)
        df.to_csv(out_base + "_summary.csv", index=False)
        print("Summary saved to", out_base + "_summary.csv")
    else:
        print("No .agpviz files found under", root_folder)

def parse_args():
    p = argparse.ArgumentParser(description="Build adjacency matrices for (3,+3) CPs connected via (3,+1) CPs.")
    p.add_argument("FOLDER", help="Root folder containing *_atomicfiles folders", type=str)
    p.add_argument("OUT_BASE", help="Base name for output files", type=str)
    return p.parse_args()

if __name__ == "__main__":
    args = parse_args()
    process_folder(args.FOLDER, args.OUT_BASE)

