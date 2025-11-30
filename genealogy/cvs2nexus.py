#!/usr/bin/env python3

'''
Need to add to nexus at the end:

BEGIN PAUP;

    [ Rooting / basic settings ]
    SET criterion=parsimony;
    set increase=auto warntree=no;

    log start file=code_phylo_current_paup.log replace;

    [ Heuristic search: branch-and-bound-like ]
    HSEARCH ADDSEQ=random NREPS=100 HOLD=10 swap=tbr;

    [ Save the best trees ]
    SAVETREES from=1 to=10 FILE=best_trees.tre REPLACE=yes brlens=yes;

    contree all/strict=yes majrule=yes savetrees treefile=code_phylo_current_consensus.tre;


END;
'''


import pandas as pd
import math
import argparse

def csv_to_nexus(input_csv, output_nexus):
    # Load CSV
    df = pd.read_csv(input_csv)

    # Require first column name
    if df.columns[0].lower() != "program":
        raise ValueError("First column must be named 'program'")

    programs = df["program"].tolist()
    char_cols = list(df.columns[1:])

    # Convert True/False strings or booleans to 0/1, missing -> NaN -> "?"
    bin_df = df.copy()
    for col in char_cols:
        bin_df[col] = bin_df[col].map({
            "True": 1, "False": 0,
            True: 1, False: 0
        })

    # Begin assembling NEXUS text
    lines = []
    lines.append("#NEXUS\n")

    # ------------------------
    # TAXA BLOCK
    # ------------------------
    lines.append("BEGIN TAXA;")
    lines.append(f"    DIMENSIONS NTAX={len(programs)};")
    lines.append("    TAXLABELS")
    for p in programs:
        lines.append(f"        {p}")
    lines.append("    ;")
    lines.append("END;\n")

    # ------------------------
    # CHARACTERS BLOCK
    # ------------------------
    lines.append("BEGIN CHARACTERS;")
    lines.append(f"    DIMENSIONS NCHAR={len(char_cols)};")
    lines.append('    FORMAT SYMBOLS="01" GAP=- MISSING=?;')
    lines.append("")
    lines.append("    CHARSTATELABELS")

    for i, col in enumerate(char_cols, start=1):
        lines.append(f"        {i} {col} /")

    lines.append("    ;\n")

    # MATRIX
    lines.append("    MATRIX")
    for _, row in bin_df.iterrows():
        vals = []
        for v in row[1:]:
            if isinstance(v, float) and math.isnan(v):
                vals.append("?")
            else:
                vals.append(str(int(v)))
        lines.append(f"        {row['program']}    {' '.join(vals)}")
    lines.append("    ;")
    lines.append("END;")
    lines.append("")

    # Write file
    with open(output_nexus, "w") as f:
        f.write("\n".join(lines))

    print(f"Wrote NEXUS file to: {output_nexus}")
    print(f"********* REMEMBER TO ADD THE NEXUS COMMANDS FROM MY SOURCE!!!!!!!!!!!!!!!!!!")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Convert CSV feature matrix to PAUP NEXUS format.")
    parser.add_argument("input_csv", help="Input CSV file (must contain 'program' column first).")
    parser.add_argument("output_nexus", help="Output NEXUS filename.")
    args = parser.parse_args()

    csv_to_nexus(args.input_csv, args.output_nexus)
