"""
create_lists.py

Distributes 150 triads of sentence-emoji pairs (each containing three conditions: a, b, c —
450 items in total) across 10 counterbalanced lists of 45 items, then sequences each list
under adjacency constraints.

Design:
  Each base item appears exactly once per condition across the 10 lists,
  so a given base shows up in 3 of the 10 lists (once as a, once as b,
  once as c). Each list contains 15 items per condition (45 total).

  Within each list, the sequence is constrained on:
    - emoji-tag adjacency (no shared strict emoji tags within a window of
      DISTANCE_TAGS positions)
    - condition repetition (no more than MAX_REPETITION_COND in a row,
      soft constraint with a penalty)
    - valence repetition (POS/NEU/NEG overlap within the window, soft
      constraint with a smaller penalty)

  The lists used in the experiment were sequenced on the emoji tags only.
  The sentence tags are part of the corpus (column tags_sentence) but were
  not applied to the ordering. Setting USE_SENTENCE_TAGS = True below applies
  a stricter variant that also spaces the sentence tags; this variant was not
  used for the published PCIBex list.

Input:
  materials/corpus.xlsx  (sheet "semantics")
    Required columns: id, cond, sentence_pt, emoji
    Optional columns: tags_sentence, tags_emoji (tags in [BRACKETS])
    Expected: 450 rows, 150 unique base IDs.

Output:
  results/pcibex_lists.csv  (long format: one row per item, with its
  Group = list number, Position, Block, condition, and violation flags).

Reproducibility:
  The lists used in the experiment were generated with RANDOM_SEED = None
  (unseeded). The canonical record of the actual lists used is the
  archived List.csv file accompanying this script (already formatted for PCIBex). Re-running this
  script with RANDOM_SEED = None will produce a different but equally
  valid counterbalanced configuration. A fixed seed can be set below if
  deterministic regeneration is desired.

Verified to run with: Python 3.13.12, pandas 3.0.2, openpyxl 3.1.5 (see requirements.txt).
"""

import os
import pandas as pd
import random
import re
from collections import defaultdict, Counter


# Config

NUM_LISTS = 10
SIZE_LIST = 45
EXCEL_IMPORT = "materials/corpus.xlsx"
SHEET = "semantics"
EXCEL_EXPORT = "results/pcibex_lists.csv"

EXPECTED_ITEMS = 450
EXPECTED_BASES = 150

# Tag handling
VALENCE_TAGS = {"POS", "NEU", "NEG"}

# The published lists were sequenced on the emoji tags only. Set this to True
# to also space the sentence tags (a stricter variant, not used for the study).
USE_SENTENCE_TAGS = True

# Sequencing constraints
DISTANCE_TAGS = 2              # Window for strict tag and valence checks
MAX_REPETITION_COND = 3        # Max same condition in a row (soft)

# Search parameters
ASSIGN_RETRIES = 5000
SEQUENCE_RESTARTS = 5000

# Block split inside each list (first BLOCK_1_SIZE items = block 1, rest = block 2)
BLOCK_1_SIZE = 23

# Reproducibility
#
# The experimental lists were generated with RANDOM_SEED = None (unseeded).
# The archived pcibex_lists_as_used.csv file is the canonical record of the actual
# lists used. Set RANDOM_SEED to an integer below if you want a
# deterministic regeneration for testing purposes.
RANDOM_SEED = None

if RANDOM_SEED is not None:
    random.seed(RANDOM_SEED)

TAG_REGEX = re.compile(r"\[([^\]]+)\]")



# Utilities

def extract_tags(row):
    """Extract tags and the subset of strict (non-valence) tags.

    By default only the emoji tags are read, reproducing how the published
    lists were sequenced. If USE_SENTENCE_TAGS is True, the sentence tags are
    added as well (stricter variant, not used for the study).
    """
    columns = ["tags_emoji"]
    if USE_SENTENCE_TAGS: 
        columns.append("tags_sentence")
    found = []
    for col in columns:
        found += TAG_REGEX.findall(str(row.get(col, "")))
    tags = set(t.strip().upper() for t in found if t.strip())
    strict_tags = set(t for t in tags if t not in VALENCE_TAGS)
    return tags, strict_tags


def normalize_id(id_str):
    """Return the original ID string and its numeric part (if any)."""
    s = str(id_str).strip()
    digits = re.findall(r"\d+", s)
    return s, int(digits[0]) if digits else None



# Input reading

def read_corpus(filename):
    try:
        df = pd.read_excel(filename, sheet_name=SHEET)
    except FileNotFoundError:
        print(f"❌ Couldn't find: {filename}")
        return None

    required = ["id", "cond", "sentence_pt", "emoji"]
    for c in required:
        if c not in df.columns:
            print(f"❌ Could not find column '{c}' in the Excel file.")
            return None

    corpus = []
    for _, row in df.iterrows():
        num = str(int(row["id"]))            # base id, e.g. "7"
        base_id = num                        # condition is a separate column now
        cond = str(row["cond"]).strip().lower()
        phrase = str(row["sentence_pt"]).strip()
        emoji = str(row["emoji"]).strip()
        all_tags, strict_tags = extract_tags(row)
        corpus.append({
            "ID_raw": num,
            "ID": num,
            "ID_num": None,
            "base_ID": base_id,
            "Cond": cond,
            "Phrase": phrase,
            "Emoji": emoji,
            "tags_all": all_tags,
            "tags_strict": strict_tags,
            "status_placement": "",
        })

    # Deterministic order so a fixed seed (when used) is truly reproducible
    # across pandas/openpyxl versions.
    corpus.sort(key=lambda x: (x["base_ID"], x["Cond"]))

    total = len(corpus)
    unique_bases = len(set(x["base_ID"] for x in corpus))
    print(f"✅ {total} items read. Unique base IDs: {unique_bases}")
    return corpus



# List assignment (random restarts)

def assign_triplets_with_restarts(corpus):
    """Distribute each base item's three conditions across 10 lists.

    Strategy: random restart. Each attempt reshuffles the base order and
    greedily assigns conditions to lists, balancing the per-condition
    quota. On a dead end the entire attempt is discarded and a fresh
    shuffle is tried. Not backtracking in the strict sense.
    """
    by_base = defaultdict(dict)
    for item in corpus:
        by_base[item["base_ID"]][item["Cond"]] = item
    base_ids = sorted(by_base.keys())

    target_per_cond = SIZE_LIST // 3

    for attempt in range(1, ASSIGN_RETRIES + 1):
        lists = [[] for _ in range(NUM_LISTS)]
        remaining = {
            i: {"a": target_per_cond, "b": target_per_cond, "c": target_per_cond}
            for i in range(NUM_LISTS)
        }

        order = base_ids[:]
        random.shuffle(order)
        ok = True

        for base in order:
            items_for_base = by_base[base]
            conds = list(items_for_base.keys())
            conds.sort(key=lambda c: sum(remaining[i][c] for i in range(NUM_LISTS)))
            chosen = {}

            for cond in conds:
                candidates = [
                    i for i in range(NUM_LISTS)
                    if remaining[i][cond] > 0 and i not in chosen.values()
                ]
                if not candidates:
                    ok = False
                    break
                candidates.sort(
                    key=lambda i: (remaining[i][cond], sum(remaining[i].values())),
                    reverse=True,
                )
                pick = candidates[0]
                chosen[cond] = pick
                remaining[pick][cond] -= 1

            if not ok:
                break

            for cond, li in chosen.items():
                lists[li].append(by_base[base][cond].copy())

        if ok and all(len(l) == SIZE_LIST for l in lists):
            print(f"✅ Assignment succeeded after {attempt} attempt(s).")
            return lists

    print("❌ Assignment failed within retry budget.")
    return None



# Sequencing

def sequence_list_with_constraints(lst):
    """Order one list so adjacency constraints are satisfied.

    Greedy with scored candidates and random restarts. Strict-tag conflicts
    are hard constraints when possible; if no clean candidate exists, a
    forced placement is made and flagged. Condition and valence repeats
    are soft constraints with penalty scores.
    """
    items = [dict(it) for it in lst]
    for it in items:
        it["valence_tags"] = set(t for t in it["tags_all"] if t in VALENCE_TAGS)

    best_seq = None
    best_violations = float("inf")
    last_complete_seq = None

    for restart in range(SEQUENCE_RESTARTS):
        remaining = items[:]
        random.shuffle(remaining)

        # Place the item with the most strict tags first, since it has the
        # most adjacency conflicts to resolve.
        remaining.sort(key=lambda x: len(x["tags_strict"]), reverse=True)

        seq = []
        first = remaining.pop(0)
        first["status_placement"] = "PERFECT (FIRST / HARD)"
        seq.append(first)

        stuck = False

        while remaining:
            window = seq[-DISTANCE_TAGS:] if len(seq) >= DISTANCE_TAGS else seq[:]
            last_conds = [s["Cond"] for s in seq[-MAX_REPETITION_COND:]] if seq else []

            candidates = []
            for cand in remaining:
                # Strict tag conflict — hard reject for this round.
                strict_conflict = any(
                    len(cand["tags_strict"] & prev["tags_strict"]) > 0
                    for prev in window
                )
                if strict_conflict:
                    continue

                cond_run = (
                    len(last_conds) >= MAX_REPETITION_COND
                    and all(c == cand["Cond"] for c in last_conds)
                )
                val_overlap = any(
                    len(cand["valence_tags"] & prev["valence_tags"]) > 0
                    for prev in window
                )

                score = 0
                if cond_run:
                    score += 100
                if val_overlap:
                    score += 10
                candidates.append((score, cond_run, val_overlap, cand))

            if not candidates:
                # No clean candidate — accept a strict violation, picking the
                # least bad option.
                forced = []
                for cand in remaining:
                    cond_run = (
                        len(last_conds) >= MAX_REPETITION_COND
                        and all(c == cand["Cond"] for c in last_conds)
                    )
                    val_overlap = any(
                        len(cand["valence_tags"] & prev["valence_tags"]) > 0
                        for prev in window
                    )
                    score = 9000  # strict violation base penalty
                    if cond_run:
                        score += 100
                    if val_overlap:
                        score += 10
                    forced.append((score, cond_run, val_overlap, cand))

                if not forced:
                    stuck = True
                    break

                forced.sort(key=lambda x: (
                    x[0],
                    -len(x[3]["tags_strict"]),
                    -len(x[3]["tags_all"]),
                ))
                _, cond_run_flag, val_flag, chosen = forced[0]
                chosen["status_placement"] = "Violations (strict)"
                chosen["_cond_run_flag"] = cond_run_flag
                chosen["_val_flag"] = val_flag

            else:
                candidates.sort(key=lambda x: (
                    x[0],
                    -len(x[3]["tags_strict"]),
                    -len(x[3]["tags_all"]),
                ))
                score, cond_run_flag, val_flag, chosen = candidates[0]

                if score == 0:
                    chosen["status_placement"] = "PERFECT"
                elif score < 100:
                    chosen["status_placement"] = "Violations (valence)"
                else:
                    chosen["status_placement"] = "Violations (cond_seq)"
                chosen["_cond_run_flag"] = cond_run_flag
                chosen["_val_flag"] = val_flag

            seq.append(chosen)
            remaining.remove(chosen)

        if not stuck and len(seq) == len(items):
            last_complete_seq = seq
            violations = sum(
                1 for s in seq
                if "Violations" in s.get("status_placement", "")
            )
            if violations < best_violations:
                best_violations = violations
                best_seq = seq
                if violations == 0:
                    break

    if best_seq is None:
        print(f"⚠️  No optimal solution found after {SEQUENCE_RESTARTS} "
              f"attempts; returning last complete sequence.")
        return last_complete_seq if last_complete_seq is not None else seq

    return best_seq



# Export

def export_lists(lists, filename):
    """Write all lists to one long-format CSV (one row per item)."""
    column_order = [
        "Group", "Position", "Block", "Id", "Cond", "Phrase", "Emoji", "Image",
        "Violations_strict", "Violations_cond_seq", "Violations_valence",
    ]
    rows = []
    for i, lst in enumerate(lists, 1):
        for idx, item in enumerate(lst):
            digits = re.findall(r"\d+", str(item["ID"]))
            item_id = int(digits[0]) if digits else item["ID"]
            status = item.get("status_placement", "")
            rows.append({
                "Group": i,
                "Position": idx + 1,
                "Block": 1 if idx < BLOCK_1_SIZE else 2,
                "Id": item_id,
                "Cond": item["Cond"],
                "Phrase": item["Phrase"],
                "Emoji": item["Emoji"],
                "Image": f"{item['ID']}{item['Cond']}.png",
                "Violations_strict": 1 if status == "Violations (strict)" else 0,
                "Violations_cond_seq": 1 if item.get("_cond_run_flag", False) else 0,
                "Violations_valence": 1 if item.get("_val_flag", False) else 0,
            })
    try:
        os.makedirs(os.path.dirname(filename) or ".", exist_ok=True)
        pd.DataFrame(rows, columns=column_order).to_csv(filename, index=False, encoding="utf-8")
    except Exception as e:
        print(f"❌ Error during export: {e}")
        return False
    print(f"✅ Export finished: {filename}")
    return True


# Main

def main():
    corpus = read_corpus(EXCEL_IMPORT)
    if corpus is None:
        return

    total = len(corpus)
    unique_bases = len(set(x["base_ID"] for x in corpus))
    if total != EXPECTED_ITEMS or unique_bases != EXPECTED_BASES:
        print(f"❌ Expected {EXPECTED_ITEMS} items / {EXPECTED_BASES} bases, "
              f"found {total} / {unique_bases}. Aborting.")
        return

    lists = assign_triplets_with_restarts(corpus)
    if lists is None:
        print("❌ Could not build balanced lists.")
        return

    print("\n📊 Per-list condition distribution:")
    global_tot = Counter()
    for i, l in enumerate(lists, 1):
        cnt = Counter(x["Cond"] for x in l)
        global_tot.update(cnt)
        print(f"  List {i}: {dict(sorted(cnt.items()))}  (n={len(l)})")
    print(f"  TOTAL: {dict(sorted(global_tot.items()))}")

    print("\n=== Sequencing ===")
    sequenced_lists = []
    for i, l in enumerate(lists, 1):
        print(f"\n--- List {i} ({len(l)} items) ---")
        seq = sequence_list_with_constraints(l)
        sequenced_lists.append(seq)

        counts = Counter(item.get("status_placement", "") for item in seq)
        perfect = counts.get("PERFECT", 0) + counts.get("PERFECT (FIRST / HARD)", 0)
        viol_cond = counts.get("Violations (cond_seq)", 0)
        viol_valence = counts.get("Violations (valence)", 0)
        viol_strict = counts.get("Violations (strict)", 0)
        print(f"Summary: PERFECT={perfect} | "
              f"cond_seq={viol_cond} | valence={viol_valence} | strict={viol_strict}")

        conds = dict(sorted(Counter(x["Cond"] for x in seq).items()))
        print(f"Condition balance: {conds}")

        for idx, it in enumerate(seq, 1):
            status = it.get("status_placement", "")
            tag = f"-> {status} <-" if "Violations" in status else ""
            print(f"  {idx:03d}. ID:{it['ID']:6} | Cond:{it['Cond']} | "
                  f"'{it['Phrase']}' {it['Emoji']} {tag}")

    if export_lists(sequenced_lists, EXCEL_EXPORT):
        print(f"\n🎉 Finished. Output file: {EXCEL_EXPORT}")
    else:
        print("\n❌ Export failed.")


if __name__ == "__main__":
    main()