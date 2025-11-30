ELIZA-GEN: Two-Stage Feature Vocabulary Construction and Program
Classification

This project implements a two-stage, GPT-assisted pipeline for
extracting and classifying algorithmic features across a corpus of
program variants. The goal is to build a structured vocabulary of
“atomic algorithmic features” that can later be used for software
genealogy, lineage reconstruction, or other comparative analyses.

The pipeline is intentionally split into two separate programs. Stage
1 generates an initial draft vocabulary from the first program in the
directory. You then manually edit, refine, or expand that vocabulary
before Stage 2. Stage 2 takes the edited vocabulary and performs a
round-robin expansion followed by final classification of all
programs.

Overview of the Two-Stage Workflow

Stage 1 (elizagen_bootstrap.py):
- Loads all code files from a directory.
- Selects the first program (based on filename sorting).
- Generates an initial set of features using GPT.
- Writes those features to a YAML file.
- Stops. You edit the YAML by hand.

Stage 2 (elizagen_round_robin.py):
- Loads all code files again.
- Loads the user-edited YAML vocabulary.
- Runs round-robin feature expansion across all programs.
- Merges any new features discovered.
- Repeats until no new features appear or until max rounds is reached.
- Performs a final classification pass across all programs.
- Outputs the final vocabulary and a program-by-feature CSV matrix.

File Structure

elizagen.py
- Shared library containing:
- bootstrap pass
- round-robin expansion
- final classification
- YAML and CSV writers
- OpenAI wrapper
- code loading utilities

elizagen_bootstrap.py
- Runs only the initial first-pass feature extraction.

elizagen_round_robin.py
- Runs round-robin feature expansion and the final classification phase.

Your input program corpus is simply a directory of code files, e.g.:

my_code_dir/
    00_original_version.py
    01_variant_A.c
    02_variant_B.lisp
    ...

All files are treated as plain text; language does not matter.

Stage 1: Bootstrap Initial Vocabulary

Command:

python3 elizagen_bootstrap.py my_code_dir --model gpt-5.1

This produces:

initial_feature_vocab.yaml


You are expected to open and edit this YAML file before continuing. You may:

- refine feature names
- adjust definitions
- merge overlapping features
- delete irrelevant features
- add new features based on domain knowledge


Stage 2 will treat your version as authoritative.

Stage 2: Vocabulary Expansion + Final Classification

After editing the YAML, run:

python3 elizagen_round_robin.py my_code_dir initial_feature_vocab.yaml \
    --model gpt-5.1 \
    --max-rounds 3 \
    --output-prefix code_phylo

This performs:

1. Round-robin expansion:
    - For each program:
        - Assigns values to all existing features.
        - Optionally proposes up to 3 new features.
    - New features are added to the vocabulary.
    - The process repeats for multiple rounds until:
        - no new features appear, OR
        - --max-rounds is reached.

2. Final classification:
    - Once vocabulary is frozen, each program is classified again.
    - Unknown values are allowed if evidence is insufficient.

Stage 2 Output Files

Using an output prefix like "code_phylo", you get two outputs:

code_phylo_feature_vocab.yaml

The full, converged vocabulary.

Includes definitions and allowed states.

code_phylo_program_features.csv

A matrix where:

rows = programs

columns = features

cells = the feature’s state or "unknown"

This CSV is suitable for downstream analysis (PCA, clustering, phylogenetic trees, etc.).

Example End-to-End Workflow

Step 1: Bootstrap

python3 elizagen_bootstrap.py eliza_versions/


Step 2: Edit the resulting initial_feature_vocab.yaml

Step 3: Expand and classify

python3 elizagen_round_robin.py eliza_versions/ initial_feature_vocab.yaml \
    --output-prefix eliza_phylo


The results will appear in:

eliza_phylo_feature_vocab.yaml
eliza_phylo_program_features.csv

Notes and Best Practices

Ordering matters:
- Stage 1 uses the first program in the directory.
- Use numeric prefixes if you want to control which specimen drives the initial vocabulary.

Manual YAML editing:
- This step is part of the design.
- You can add human insight before automatic expansion.

Convergence:
- The system stops early if no new features appear.
- Increase or decrease --max-rounds depending on how much exploration you want.

Model selection:
- Use any Chat Completions model.
- Higher-end models give more stable vocabularies.
