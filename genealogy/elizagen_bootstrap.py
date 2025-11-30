#!/usr/bin/env python3
import argparse
import yaml

from elizagen_library import (
    load_code_files,
    bootstrap_vocab_for_first_program,
    write_vocab_yaml
)

def main():
    parser = argparse.ArgumentParser(
        description="Bootstrap initial feature vocabulary using only the first program."
    )
    parser.add_argument("--code_dir", default="code/")
    parser.add_argument("--model", default="gpt-5.1")
    parser.add_argument("--output",
                        default="results/initial_feature_vocab.yaml")
    args = parser.parse_args()

    programs = load_code_files(args.code_dir)
    if not programs:
        print("No code files found.")
        return

    (prog_name, code) = list(programs.items())[0]
    print(f"[BOOTSTRAP] Bootstrapping vocabulary from: {prog_name}")

    vocab = bootstrap_vocab_for_first_program(args.model, prog_name, code)
    write_vocab_yaml(vocab, args.output)

    print(f"[DONE] Wrote initial vocabulary to {args.output}")

if __name__ == "__main__":
    main()
