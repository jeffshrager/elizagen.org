#!/usr/bin/env python3
import argparse
import yaml

from elizagen_library import (
    load_code_files,
    round_robin_expand_vocab,
    write_vocab_yaml,
)

def main():
    parser = argparse.ArgumentParser(
        description="Run ONLY the round-robin vocabulary expansion using an edited initial YAML file."
    )
    parser.add_argument("--code_dir", default="code/")
    parser.add_argument("--initial_vocab_yaml",
                        default="results/initial_feature_vocab.yaml",
                        help="YAML from bootstrap (possibly hand-edited)")
    parser.add_argument("--model", default="gpt-5.1")
    parser.add_argument("--max-rounds", type=int, default=3)
    parser.add_argument("--output",
                        default="results/rr_vocab/rr_feature_vocab.yaml")
    args = parser.parse_args()

    programs = load_code_files(args.code_dir)
    if not programs:
        print("No code files found.")
        return

    with open(args.initial_vocab_yaml, "r") as f:
        initial_vocab = yaml.safe_load(f)
    print(f"[LOAD] Loaded {len(initial_vocab)} initial features.")

    vocab = round_robin_expand_vocab(
        model=args.model,
        programs=programs,
        max_rounds=args.max_rounds,
        skip_bootstrap=True,
        initial_vocab=initial_vocab,
    )

    write_vocab_yaml(vocab, args.output)
    print(f"[DONE] Wrote RR-expanded vocabulary to {args.output}")

if __name__ == "__main__":
    main()
