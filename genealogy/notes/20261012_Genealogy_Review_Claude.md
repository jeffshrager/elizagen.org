# Review: ELIZA genealogy project (paper, method, arguments) and what to improve

*Written by Claude (Opus 5.5) at Jeff Shrager's request, 2026-10-12. Status: **on hold**. The paper was marked "A Work in Progress" the same day; nothing below has been acted on yet.*

## Context
This is an honest assessment of the ELIZA code-phylogeny project:
- its approach (the LLM feature extraction → binary matrix → PAUP* pipeline);
- its arguments (the textual-tradition framing, the naming paradox, the interpretation of the tree);
- what could be improved.

Sources reviewed:
- `genealogy/paper/ELIZAGen/elizagen.tex`
- `genealogy/results/` (CSV, NEXUS, PAUP* log)
- `genealogy/elizagen_library.py`
- the elizagen.org site's own history of the clones

The idea is good and genuinely novel in the ELIZA context. The corpus is valuable, and the textual-tradition framing is the most interesting part. The empirical half, however, cannot yet carry the conclusions drawn from it. Findings are ranked by impact.

## A. Problems that affect the conclusions (fix first)

1. **The paper's description of the results doesn't match what was actually run.**
   - Methods says "all most-parsimonious trees were retained" and that "strict and majority-rule consensus trees were computed."
   - The PAUP* log (`results/code_phylo_current_paup.log`) shows **342 equally parsimonious trees**. Only 10 were saved, no consensus tree was computed (there is no `contree` in `results/elizas.nexus`), and the paper's figure is a single arbitrary one of those trees.
   - With 13 characters for 27 specimens the tree is badly underdetermined, so most groupings below the basal split are probably not supported.
   - *Fix:* compute strict and 50% majority-rule consensus trees. Run a bootstrap (e.g. 1000 replicates). Report tree length, CI, RI and the number of MP trees. Show the consensus tree, or a network, instead of one tree.

2. **"Basal" and "derived" describe a rooted tree, but the tree is unrooted.**
   - The log says "Trees are unrooted."
   - The drawing roots it arbitrarily at E01, so "basal polytomy" and "derived clade" have no meaning yet.
   - *Fix:* root it explicitly on Weizenbaum's original (E19, the known ancestor). Alternatively, use Dollo or irreversible characters, which suit a "features get lost" story. Then state the rooting assumption in the paper.

3. **The characters can only measure distance from ELIZA, not relationships among descendants.**
   - All 13 features are facets of Weizenbaum's own architecture, and they're heavily interdependent: ordered memory requires memory; backtracking requires a matcher.
   - A "complete" program therefore scores all-ones, and every other program differs only by losing parts of that bundle. That is closer to a fidelity score than to a phylogeny.
   - Interdependent characters also inflate apparent support.
   - *Fix:* collapse or weight the dependent characters, and add characters that vary *within* the descendant group (see B1).

4. **Real descent is known for several programs but isn't used, and the paper's interpretation contradicts some of it.**
   - The paper argues that the BASIC versions cluster "not necessarily because they copied from one another, but because they faced similar constraints."
   - The site's own history says many of them *are* knock-offs of Shrager's 1973 BASIC ELIZA.
   - Steve Cherry's P-Lisp ELIZA (E14) is documented on the site (News page) as an adaptation of Shrager's BASIC. Yet the tree places it with Babcock's BASIC, not with E12.
   - These known links are the best validation available, and they're currently unused.
   - *Fix:* assemble a ground-truth table of documented derivations (Shrager → BASIC descendants, Cherry ← Shrager, Cosell ← CACM, Hay ← MAD-SLIP, …). Score each method against it, as Jang et al.'s iEVAL scores lineage inference. Rewrite the convergence claim to match.

5. **The validation specimens are weak tests.**
   - **Clones:** byte-identical input given to a temperature-0 LLM will give identical features. That tests determinism, not the method.
   - **LadyMouth (all zeros) and the foil:** their maximal or odd distances follow almost by construction.
   - *Better tests:*
     - Run the feature extraction several times, and with two different models, and report agreement (κ) between runs.
     - Compare LLM codings against hand codings for a subset, using Anthony or Art as the human coders.
     - Include near-clones: lightly edited or ported copies with a known parent.

6. **The original ELIZA (E19) is scored "?" for synonym translation.**
   - That's the best-documented specimen of all, and Team ELIZA knows its script handles substitution and classes (DLIST).
   - The unknown suggests the LLM saw the code without the script, which questions how the specimens were defined.
   - *Fix:* settle what counts as a specimen (code alone, or code plus script), apply that rule to every program, and hand-check E19.

## B. Method improvements that would strengthen the paper's own argument

1. **Use "shared errors", the method of the textual criticism the paper cites.**
   - Stemmatology infers descent from shared *idiosyncrasies* (copied errors, odd readings), not from shared function.
   - Software has exact equivalents:
     - verbatim response strings and keyword lists;
     - conjugation tables;
     - typos;
     - odd variable names, and BASIC `DATA`-statement layout;
     - specific bugs.
   - These are the strongest evidence that one program was copied from another. Architecture is where independent reimplementations end up looking alike (convergence), which is exactly what confuses the current results.
   - *Proposal:* add a second character set drawn from the scripts and their content, extracted by an LLM or by simple text alignment. Analyze it separately from the architecture set, then together.
   - This directly answers the paper's own question, "descent versus convergence."

2. **Use network methods alongside trees.** NeighborNet/SplitsTree, or a reticulate analysis, would show conflicting signal and mixed parentage, e.g. a BASIC structure combined with the CACM script. The paper argues for this kind of mixing but uses a method that can't show it.

3. **Reproducibility.**
   - Report the model, version, prompts and temperature (the code uses `call_openai(..., temperature=0.0)`; the readme uses `gpt-5.1`).
   - Require each feature value to cite evidence (quoted lines of code), and publish those annotations.
   - Record every manual curation decision.
   - Publish the commands that run the full pipeline (`run.sh` plus the PAUP block).

4. **Corpus.** Report how specimens were chosen; there is likely a skew toward BASIC and Lisp. Add dates and provenance metadata so the tree can be checked against chronology: a descendant shouldn't predate its parent.

## C. Arguments and writing

1. **Balance between theory and data.** The textual-tradition, Kripke and naming-paradox sections are long, and nothing in the analysis tests them. Either cut them down, or connect each claim to a result: B1 would let "descent versus convergence versus affiliation" be measured rather than asserted.
2. **Structure.** "Software as Textual Tradition" opens with "However, we can reframe…" after the interpretation, so the argument is out of order. Move the framework into the introduction, keep Results purely empirical, and put the interpretation in the Discussion.
3. **Naming paradox.** LadyMouth is now described as ELIZA-like rather than as using the ELIZA name, so the naming argument rests on the BASIC programs that call themselves "ELIZA". Make that explicit, with real examples from the corpus.
4. **What the tree is evidence of.** The paper alternates between "maps a textual tradition" and "reveals genealogy." Say once, clearly: it measures architectural similarity, which can indicate descent, convergence or shared constraints. Then say which analyses tell those apart.
5. **Missing prior work.** These are directly relevant; *verify each before citing* (they come from Claude's memory and have not been checked):
   - Barbrook et al. 1998, *Nature*, "The phylogeny of The Canterbury Tales": phylogenetic software applied to manuscripts, the exact bridge the paper needs.
   - Tehrani 2013, *PLoS ONE*, "The Phylogeny of Little Red Riding Hood": cultural phylogenetics with mixing.
   - Goldberg et al. 1998, computer-virus phylogenies.
   - Lindorfer et al. 2012, malware lineage.
6. **Small fixes.**
   - The paper switches between "I" and "we".
   - A run-on sentence in the introduction ("…taxonomic analysis ELIZA provides…").
   - Citations print the author twice (`\cite` → `\citep`).
   - The E26/E23 names in the CSV and the tables don't match the file names (Duquet/Duguet, Jull/Jul).
   - The figure caption contains a raw Newick string.

## Suggested order of work (when resumed)
1. Re-run PAUP* with consensus trees, bootstrap and explicit rooting on E19, and report CI/RI. Small effort; changes what can be claimed.
2. Build the ground-truth lineage table and score the current tree against it.
3. Run the extraction several times and with two models, and report agreement (κ) and a hand-coded comparison.
4. Add the shared-content character set and a NeighborNet; compare it with the architecture set.
5. Restructure the paper and trim the theory to match what the data now supports. Verify and add the prior work.

The files involved:
- `genealogy/elizagen_library.py`, `cvs2nexus.py`, `draw_paup_trees.py`
- `results/elizas.nexus` (add CONTREE/BOOTSTRAP/OUTGROUP commands)
- `paper/ELIZAGen/elizagen.tex` and `.bib`

## Verification (when resumed)
- PAUP* log shows consensus and bootstrap output, with rooting on E19.
- A ground-truth score table appears in the paper.
- The κ (agreement) table appears in the paper.
- The PDF rebuilds cleanly.
- The Genealogy page figure is updated to the consensus tree.
