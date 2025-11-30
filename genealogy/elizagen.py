#!/usr/bin/env python3
import os
import argparse
import time
import csv
import yaml
from typing import Dict, List, Any

from openai import OpenAI

# -------------- CONFIG DEFAULTS --------------

DEFAULT_MODEL = "gpt-5.1"
DEFAULT_MAX_ROUNDS = 3

# -------------- PROMPT TEMPLATES --------------

FIRST_PASS_PROMPT = """You will analyze a piece of source code and extract a set of high-level,
algorithmic features.

Your goal is to propose up to 10 atomic features that describe the
algorithmic strategies, data structures, control flow patterns, and
noteworthy implementation techniques of the program.

IMPORTANT RULES:
- Each feature must be something that other implementations of the same task
  could plausibly either have or not have.
- Each feature must be observable from the code (no subjective judgments).
- Each feature must be atomic: not compound or hierarchical.
- Avoid synonyms or overlapping concepts.
- Use short, neutral feature names.
- Ignore the specific programming language used;
  extract features that are language-agnostic and algorithmic in nature.

For each feature, adhere to this schema:

- feature_name: <a short noun-phrase ID, snake_case>
  type: [binary | ordinal | categorical]
  states: <for ordinal/categorical only, list allowed states>
  definition: <1–2 sentence precise definition of what it means for the program
               to have each state>
  value_for_this_program: <one of the states>

Now here is the code to analyze:

<<<CODE>>>

Produce ONLY a YAML list of feature objects, following the schema above.
Do not add commentary or explanation.
"""

ROUND_ROBIN_PROMPT = """You will analyze a new program and assign values for an existing global
vocabulary of algorithmic features. You may introduce a very small number
of new features only if strictly necessary.

TASK
1. Reuse the existing feature vocabulary.
2. For each existing feature, assign the value for this new program.
3. If the program shows important algorithmic ideas not captured by any
   existing feature, propose at most 3 new features (using the same schema).
4. Be consistent across programs: if two programs appear to implement the
   same concept, they should receive the same feature state.

RULES
- Use only the provided feature names and definitions when assigning values.
  Do NOT reinterpret them.
- New features may be proposed only if they reflect concrete, observable,
  algorithmic aspects lacking from the vocabulary.
- No subjective judgments (e.g., “optimized”, “clean code”).
- Features should be atomic (non-compound) and must be properties that other
  implementations could also have.
- Ignore the programming language; classify features based only on algorithmic behavior,
  not syntactic or language-specific constructs.

SCHEMA (for both existing and new features):

- feature_name: <given or newly proposed>
  type: [binary | ordinal | categorical]
  states: <for ordinal/categorical only>
  definition: <short, precise definition>
  value_for_this_program: <one of the states>

INPUT
Here is the current global feature vocabulary (YAML):

<<<FEATURE_VOCABULARY>>>

Here is the program to analyze:

<<<CODE>>>

OUTPUT
Produce a YAML list with:
1. All existing features, with their assigned values.
2. Any newly proposed features, with full schema and values.

Do not add commentary or explanation.
"""

FINAL_CLASS_PROMPT = """You will classify a program according to a fixed feature vocabulary.
You are NOT allowed to propose new features, modify definitions, or
reinterpret meanings.

For each feature in the vocabulary:
- Read the definition carefully.
- Assign the correct state (binary/ordinal/categorical) based on the code.
- Use 'unknown' if the program does not contain enough evidence to decide.
- Ignore the programming language; evaluate features based solely on algorithmic content,
  not language-specific syntax.

SCHEMA:

- feature_name: <given>
  value_for_this_program: <state or 'unknown'>

INPUT
Global feature vocabulary (definitions and allowed states) in YAML:

<<<FEATURE_VOCABULARY>>>

Program to classify:

<<<CODE>>>

OUTPUT
Return ONLY a YAML list of features with assigned values.
"""

# -------------- OPENAI CLIENT WRAPPER --------------

def call_openai(model: str, prompt: str, temperature: float = 0.0) -> str:
    client = OpenAI()

    resp = client.chat.completions.create(
        model=model,
        messages=[
            {"role": "system",
             "content": "You are a careful assistant that follows schemas exactly and outputs valid YAML."},
            {"role": "user", "content": prompt},
        ],
        temperature=temperature,
    )
    return resp.choices[0].message.content

# -------------- UTILITIES --------------

def load_code_files(directory: str) -> Dict[str, str]:
    programs = {}
    for fname in sorted(os.listdir(directory)):
        path = os.path.join(directory, fname)
        if not os.path.isfile(path):
            continue
        if fname.startswith("."):
            continue
        prog_name, _ext = os.path.splitext(fname)
        with open(path, "r", encoding="utf-8", errors="ignore") as f:
            code = f.read()
        programs[prog_name] = code
    return programs

def parse_yaml_list(yaml_str: str) -> List[Dict[str, Any]]:
    try:
        data = yaml.safe_load(yaml_str)
        if not isinstance(data, list):
            raise ValueError("YAML output is not a list")
        return data
    except Exception as e:
        raise RuntimeError(f"Failed to parse YAML from model: {e}\n--- RAW ---\n{yaml_str}")

# -------------- MAIN PIPELINE PIECES --------------

def bootstrap_vocab_for_first_program(model: str, prog_name: str, code: str) -> List[Dict[str, Any]]:
    prompt = FIRST_PASS_PROMPT.replace("<<<CODE>>>", code)
    print(f"[BOOTSTRAP] Extracting initial features from {prog_name}...")
    yaml_str = call_openai(model, prompt)
    features = parse_yaml_list(yaml_str)
    print(f"[BOOTSTRAP] Got {len(features)} initial features.")
    return features

def round_robin_expand_vocab(
    model: str,
    programs: Dict[str, str],
    max_rounds: int = DEFAULT_MAX_ROUNDS,
    skip_bootstrap: bool = False,
    initial_vocab: List[Dict[str, Any]] = None,
) -> List[Dict[str, Any]]:

    prog_items = list(programs.items())
    if not prog_items:
        raise ValueError("No programs found.")

    # --- Bootstrap selection ---
    if skip_bootstrap:
        if initial_vocab is None:
            raise ValueError("skip_bootstrap=True but initial_vocab is None.")
        vocab_by_name = {f["feature_name"]: f for f in initial_vocab}
        start_index_for_round0 = 0
    else:
        first_name, first_code = prog_items[0]
        initial = bootstrap_vocab_for_first_program(model, first_name, first_code)
        vocab_by_name = {f["feature_name"]: f for f in initial}
        start_index_for_round0 = 1

    # Helper: run a single RR round
    def run_round(start_idx: int, vocab_map: Dict[str, Any]) -> int:
        new_features = 0
        for i in range(start_idx, len(prog_items)):
            prog_name, code = prog_items[i]
            print(f"[ROUND] Processing {prog_name}...")

            vocab_yaml = yaml.safe_dump(list(vocab_map.values()), sort_keys=False)
            prompt = ROUND_ROBIN_PROMPT.replace("<<<FEATURE_VOCABULARY>>>", vocab_yaml)\
                                       .replace("<<<CODE>>>", code)
            yaml_str = call_openai(model, prompt)
            features = parse_yaml_list(yaml_str)

            # Incorporate new features
            for f in features:
                fname = f.get("feature_name")
                if fname and fname not in vocab_map:
                    vocab_map[fname] = {
                        "feature_name": fname,
                        "type": f.get("type"),
                        "states": f.get("states"),
                        "definition": f.get("definition"),
                    }
                    new_features += 1
        return new_features

    # Round 0
    print("[ROUND 0] Expanding vocabulary...")
    new0 = run_round(start_index_for_round0, vocab_by_name)
    print(f"[ROUND 0] New features added: {new0}")

    # Rounds 1..N
    for r in range(1, max_rounds):
        print(f"[ROUND {r}] Starting full round...")
        new_f = run_round(0, vocab_by_name)
        print(f"[ROUND {r}] New features added: {new_f}")
        if new_f == 0:
            print(f"[STOP] No new features in round {r}; converged.")
            break
        if r == max_rounds - 1:
            print("[WARN] max_rounds reached; vocabulary may not be converged.")

    final_vocab = list(vocab_by_name.values())
    print(f"[RESULT] Final vocabulary size: {len(final_vocab)}")
    return final_vocab

def final_classification(model: str,
                         programs: Dict[str, str],
                         vocab: List[Dict[str, Any]]) -> Dict[str, Dict[str, Any]]:

    feature_matrix: Dict[str, Dict[str, Any]] = {}
    vocab_yaml = yaml.safe_dump(vocab, sort_keys=False)

    for prog_name, code in programs.items():
        print(f"[FINAL] Classifying {prog_name}...")
        prompt = FINAL_CLASS_PROMPT.replace("<<<FEATURE_VOCABULARY>>>", vocab_yaml)\
                                   .replace("<<<CODE>>>", code)
        yaml_str = call_openai(model, prompt)
        features = parse_yaml_list(yaml_str)

        row = {}
        for f in features:
            fname = f.get("feature_name")
            value = f.get("value_for_this_program")
            if fname is not None:
                row[fname] = value
        feature_matrix[prog_name] = row

    return feature_matrix

# -------------- WRITE FUNCTIONS --------------

def write_vocab_yaml(vocab: List[Dict[str, Any]], path: str) -> None:
    with open(path, "w", encoding="utf-8") as f:
        yaml.safe_dump(vocab, f, sort_keys=False)
    print(f"[WRITE] Feature vocabulary written to {path}")

def write_matrix_csv(feature_matrix: Dict[str, Dict[str, Any]],
                     vocab: List[Dict[str, Any]], path: str) -> None:
    feature_names = [f["feature_name"] for f in vocab]

    with open(path, "w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        header = ["program"] + feature_names
        writer.writerow(header)
        for prog_name, row in sorted(feature_matrix.items()):
            values = [row.get(fname, "") for fname in feature_names]
            writer.writerow([prog_name] + values)

    print(f"[WRITE] Program-feature matrix written to {path}")
