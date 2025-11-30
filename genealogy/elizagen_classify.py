#!/usr/bin/env python3
import argparse
import yaml

from elizagen_library import (
    load_code_files,
    final_classification,
    write_matrix_csv,
)

def main():
    parser = argparse.ArgumentParser(
        description="Final classification stage: assign feature values using a fixed vocabulary."
    )
    parser.add_argument("--code_dir", default="code/")
    parser.add_argument("--vocab_yaml",
                        default="results/rr_feature_vocab.yaml")
    parser.add_argument("--model", default="gpt-5.1")
    parser.add_argument("--output",
                        default="results/program_features.csv")
    args = parser.parse_args()

    programs = load_code_files(args.code_dir)
    if not programs:
        print("No code files found.")
        return

    # Load the stable RR vocab
    with open(args.vocab_yaml, "r") as f:
        vocab = yaml.safe_load(f)
    print(f"[LOAD] Loaded vocabulary of {len(vocab)} features.")

    feature_matrix = final_classification(args.model, programs, vocab)

    write_matrix_csv(feature_matrix, vocab, args.output)
    print(f"[DONE] Wrote classification matrix to {args.output}")

if __name__ == "__main__":
    main()
