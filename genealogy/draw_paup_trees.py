import re
from io import StringIO

import matplotlib.pyplot as plt
from Bio import Phylo


def parse_translate_block(lines):
    """
    Given an iterator over lines, starting at the 'Translate' line,
    parse until the terminating ';' and return a dict: id (str) -> name (str).
    """
    translate = {}
    for line in lines:
        line = line.strip()
        if line == ';':
            break
        if not line:
            continue

        # Example line:
        # 1 1979_EricPlatt_DR_CHALLENGER_CLEAN,
        # 9 samson_M46_gross
        # Remove trailing comma, then split
        if line.endswith(','):
            line = line[:-1]
        parts = line.split(None, 1)
        if len(parts) != 2:
            continue
        num, name = parts
        translate[num] = name
    return translate


def extract_newick_from_tree_line(line):
    """
    Given a line like:
        tree 'PAUP_1' = [&U] (1:3,((2:1,...):0);
    extract the pure Newick string "(1:3,...):0"
    """
    # Find first '(' and the last ';'
    start = line.find('(')
    end = line.rfind(';')
    if start == -1 or end == -1 or end <= start:
        raise ValueError(f"Could not extract Newick from line: {line}")
    return line[start:end]


def relabel_tree_with_translate(tree, translate):
    """
    Given a Bio.Phylo tree and a translate dict (id->name),
    replace clade names that are numeric IDs with their full names.
    """
    for clade in tree.find_clades():
        if clade.name is None:
            continue
        # PAUP uses numeric labels; they arrive as strings
        key = clade.name
        if key in translate:
            clade.name = translate[key]

def read_paup_trees(filename):
    """
    Read a PAUP .tre / .nex file that has a Translate block and
    one or more 'tree' lines. Return (translate_dict, list_of_BioPhylo_trees).
    """
    with open(filename, 'r') as f:
        lines = f.readlines()

    translate = {}
    trees = []

    i = 0
    n = len(lines)
    while i < n:
        line = lines[i].strip()

        # Parse the Translate block
        if re.match(r'^\s*Translate\b', lines[i]):
            # Consume the "Translate" line and parse subsequent lines
            i += 1
            translate = parse_translate_block(iter(lines[i:]))
            # Advance i to the line after the ';' that ended Translate
            # (We already iterated over lines[i:], so we need to find that ';' again.)
            while i < n and lines[i].strip() != ';':
                i += 1
            # Skip the ';' line
            i += 1
            continue

        # Parse tree lines
        if line.startswith('tree '):
            newick_str = extract_newick_from_tree_line(lines[i])
            handle = StringIO(newick_str)
            tree = Phylo.read(handle, 'newick')
            trees.append(tree)

        i += 1

    return translate, trees


def draw_trees(trees, translate, prefix="tree"):
    """
    Draw each tree with Bio.Phylo and save to PNG files.
    """
    for idx, tree in enumerate(trees, start=1):
        # Relabel numeric tips with full names
        relabel_tree_with_translate(tree, translate)

        # Draw
        fig = plt.figure(figsize=(8, 6))
        ax = fig.add_subplot(1, 1, 1)
        Phylo.draw(tree, do_show=False, axes=ax)

        out_name = f"{prefix}_{idx}.png"
        plt.tight_layout()
        plt.savefig(out_name, dpi=300)
        plt.close(fig)
        print(f"Saved {out_name}")


if __name__ == "__main__":
    # Change this to your actual PAUP tree file, e.g. "eliza_trees.tre"
    filename = "best_trees.tre"

    translate, trees = read_paup_trees(filename)
    print("Translate mapping:")
    for k, v in translate.items():
        print(f"  {k} -> {v}")

    print(f"Read {len(trees)} trees.")
    draw_trees(trees, translate, prefix="eliza_tree")
