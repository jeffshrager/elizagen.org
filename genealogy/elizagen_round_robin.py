#!/usr/bin/env python3
import argparse
import yaml

from elizagen import (
    load_code_files,
    round_robin_expand_vocab,
    final_classification,
    write_vocab_yaml,
    write_matrix_csv,
)

def main():
    parser = argparse.ArgumentParser(
        description="Run round-robin vocabulary expansion and final classification starting from an edited initial vocabulary."
    )
    parser.add_argument("code_dir")
    parser.add_argument("initial_vocab_yaml", help="YAML file from bootstrap (possibly hand-edited)")
    parser.add_argument("--model", default="gpt-5.1")
    parser.add_argument("--max-rounds", type=int, default=3)
    parser.add_argument("--output-prefix", default="code_phylo")
    args = parser.parse_args()

    programs = load_code_files(args.code_dir)
    if not programs:
        print("No code files found.")
        return

    with open(args.initial_vocab_yaml, "r") as f:
        initial_vocab = yaml.safe_load(f)
    print(f"[LOAD] Loaded {len(initial_vocab)} initial features (hand-edited).")

    # Round-robin WITHOUT bootstrap
    vocab = round_robin_expand_vocab(
        model=args.model,
        programs=programs,
        max_rounds=args.max_rounds,
        skip_bootstrap=True,
        initial_vocab=initial_vocab,
    )

    # Final classification
    feature_matrix = final_classification(args.model, programs, vocab)

    vocab_path = f"{args.output_prefix}_feature_vocab.yaml"
    matrix_path = f"{args.output_prefix}_program_features.csv"

    write_vocab_yaml(vocab, vocab_path)
    write_matrix_csv(feature_matrix, vocab, matrix_path)

    print("[DONE]")

if __name__ == "__main__":
    main()
