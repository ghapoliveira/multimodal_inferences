"""
create_lists_eeg.py

Pseudorandomization script for the EEG experiment (Experiment 2).
Generates 3 Latin-square-style counterbalanced lists, merging fillers with the
targets selected in Experiment 1. Sequencing constraints: semantic-tag adjacency,
condition-run length, valence adjacency, and Target/Filler clumping.

Input (targets are derived live from Experiment 1 — no standalone copy):
  - ../exp1_norming/materials/corpus.xlsx          sheet "semantics"
        the full corpus (id, cond, sentence_pt, emoji, tags_sentence,
        tags_emoji, ...)
  - ../exp1_norming/results/analysis_ph/targets_selected_ph.csv
        the 120 selected ids; the corpus is filtered to these.
  - materials/eeg_materials.xlsx                   sheet "fillers"
        selected fillers only (used == "yes").

Output:
  - results/eeg_lists_generated.xlsx   4 sheets (Mixed/Targets x Emoji/Filenames)
  - results/sequencing_report.txt      per-list violation summary

Reproducibility:
  The lists actually presented were generated unseeded (RANDOM_SEED = None) and
  then finalized for E-Prime. The canonical record of what was presented is the
  archived results/eeg_lists_as_used.xlsx (the final E-Prime list). Re-running this
  script reproduces the method, not that file: it yields a valid but different
  counterbalanced configuration. Set RANDOM_SEED to an integer for deterministic
  testing.

Verified to run with: Python 3.13.12, pandas 3.0.2, openpyxl 3.1.5 (see requirements.txt).
"""

import pandas as pd
import os
import random
import re
import time
from collections import defaultdict, Counter


# Config

NUM_LISTS = 3
TARGETS_PER_LIST = 120

# Files
# Targets are derived live from Experiment 1 (the corpus filtered to the
# selected ids), so they can never drift from a stale copy.
TARGETS_CORPUS   = "../exp1_norming/materials/corpus.xlsx"
TARGETS_SHEET    = "semantics"
SELECTED_IDS_CSV = "../exp1_norming/results/analysis_ph/targets_selected_ph.csv"

FILLERS_FILE  = "materials/fillers_and_questions.xlsx"
FILLERS_SHEET = "fillers"

OUTPUT_FILE = "results/eeg_lists_generated.xlsx"
REPORT_FILE = "results/sequencing_report.txt"

# Tag categories
VALENCE_TAGS = {"POS", "NEU", "NEG"}
IGNORED_TAGS = {"OBJ", "OBJET", "OBJETS", "ANIMAL", "ANIMAIS", "NAT", "CONS"}

# Sequencing constraints
TAG_DISTANCE = 1            # No shared strict tags within this distance
MAX_CONDITION_REPEAT = 4    # Max same-condition in a row (e.g. a, a, a, a forbidden)
MAX_TYPE_REPEAT = 3         # Max same-type (Target/Filler) in a row

# Search parameters
ASSIGN_RETRIES = 5000
SEQUENCE_RESTARTS = 5000    # Used by the sequencer
STOP_THRESHOLD = 0          # Stop searching when this many "real" errors remain

# Generated unseeded for the experiment; the final E-Prime list
# (results/eeg_lists_as_used.xlsx) is the canonical record. Set RANDOM_SEED to an
# integer below only for deterministic testing.
RANDOM_SEED = None
if RANDOM_SEED is not None:
    random.seed(RANDOM_SEED)

TAG_REGEX = re.compile(r"\[([^\]]+)\]")


# Utilities

def get_col_flexible(row, candidates):
    """Return the first non-null column value from a list of candidate names."""
    for col in candidates:
        if col in row and pd.notna(row[col]):
            return str(row[col])
    return ""


def extract_tags(row, is_filler=False):
    """Extract all tags, strict tags (semantic), and valence tags from a row.

    Candidate column names cover both the old standalone files (Tags_Phrase /
    Tags_Emoji) and the Experiment-1 corpus (tags_sentence / tags_emoji).
    """
    phrase_tag_cols = ["tags_sentence"]
    raw_phrase_tags = get_col_flexible(row, phrase_tag_cols)
    phrase_tags = TAG_REGEX.findall(raw_phrase_tags)

    emoji_tag_cols = ["tags_emoji"]
    raw_emoji_tags = get_col_flexible(row, emoji_tag_cols)
    emoji_tags = TAG_REGEX.findall(raw_emoji_tags)

    all_tags = set(t.strip().upper() for t in phrase_tags + emoji_tags if t.strip())

    strict_tags = set(
        t for t in all_tags
        if t not in VALENCE_TAGS and t not in IGNORED_TAGS
    )

    valence = set(t for t in all_tags if t in VALENCE_TAGS)
    if is_filler:
        valence = set()

    return all_tags, strict_tags, valence


def get_segments(phrase, emoji_char, replacement, is_target):
    """Split a sentence into word segments for RSVP, placing the emoji/filename.

    For targets the replacement is appended at the end.
    For fillers the emoji character inside the sentence is swapped for the
    replacement (only the first occurrence, to be safe).
    """
    phrase = str(phrase).strip()
    emoji_char = str(emoji_char).strip()

    if not is_target and emoji_char in phrase:
        safe_phrase = phrase.replace(emoji_char, f" {replacement} ", 1)
        return safe_phrase.split()
    else:
        return phrase.split() + [replacement]


# Input reading

def read_targets(corpus_file, sheet, selected_csv):
    """Build the target corpus from Experiment 1: the normed corpus filtered to
    the selected ids."""
    print(f"Reading targets: {corpus_file} [{sheet}], filtered by {selected_csv}")
    try:
        selected = pd.read_csv(selected_csv)
    except FileNotFoundError:
        print(f"  ERROR: file not found: {selected_csv}")
        return None
    if "id" not in selected.columns:
        print("  ERROR: selected-ids file has no 'id' column.")
        return None
    selected_ids = set(str(i).strip() for i in selected["id"])

    try:
        df = pd.read_excel(corpus_file, sheet_name=sheet)
    except FileNotFoundError:
        print(f"  ERROR: file not found: {corpus_file}")
        return None
    if "id" not in df.columns:
        print("  ERROR: corpus sheet has no 'id' column.")
        return None

    df = df[df["id"].astype(str).str.strip().isin(selected_ids)]

    corpus = []
    for _, row in df.iterrows():
        num = str(row["id"]).strip()
        cond = str(row.get("cond", "unk")).strip().lower()
        phrase = str(row.get("sentence_pt", "")).strip()
        emoji = str(row.get("emoji", "")).strip()
        all_tags, strict_tags, valence = extract_tags(row, is_filler=False)

        corpus.append({
            "ID": num,
            "base_ID": num,         
            "Cond": cond,
            "Phrase": phrase,
            "Emoji": emoji,
            "all_tags": all_tags,
            "strict_tags": strict_tags,
            "valence_tags": valence,
            "Type": "Target",
            "placement_status": "",
        })

    corpus.sort(key=lambda x: (x["base_ID"], x["Cond"]))
    n_base = len(set(x["base_ID"] for x in corpus))
    print(f"  {len(corpus)} target rows ({n_base} base items) read.")
    return corpus


def read_fillers(filename, sheet):
    """Read the used fillers from eeg_materials.xlsx (used == 'yes'). The filler
    image filename is taken from the 'image' column so the export reconstructs it
    exactly."""
    print(f"Reading fillers: {filename} [{sheet}]")
    try:
        df = pd.read_excel(filename, sheet_name=sheet)
    except FileNotFoundError:
        print(f"  ERROR: file not found: {filename}")
        return []

    if "used" in df.columns:
        keep = df["used"].astype(str).str.strip().str.lower().isin(
            ["yes", "y", "sim", "true", "1"])
        df = df[keep]

    fillers = []
    for _, row in df.iterrows():
        img = str(row.get("image", "")).strip()
        if img:
            raw_id = re.sub(r"\.png$", "", img, flags=re.IGNORECASE)
        else:
            raw_id = str(row.get("id", "F")).strip()
        phrase_text = str(row.get("Fillers", row.get("Phrase",
                          row.get("sentence_pt", "")))).strip()
        emoji = str(row.get("emoji", "")).strip()
        all_tags, strict_tags, valence = extract_tags(row, is_filler=True)

        fillers.append({
            "ID": f"{raw_id}",
            "base_ID": f"fill_{raw_id}",
            "Cond": "filler",
            "Phrase": phrase_text,
            "Emoji": emoji,
            "all_tags": all_tags,
            "strict_tags": strict_tags,
            "valence_tags": valence,
            "Type": "Filler",
            "placement_status": "",
        })

    fillers.sort(key=lambda x: x["base_ID"])
    print(f"  {len(fillers)} filler items read.")
    return fillers



# Assignment (random-restart)

def assign_triplets_with_restarts(corpus):
    """Distribute condition triplets across NUM_LISTS lists.

    Strategy: random restarts (not backtracking). On a dead end the whole
    attempt is discarded and a new random shuffle is tried. With a clean
    triplet corpus this converges in a handful of attempts.
    """
    by_base = defaultdict(dict)
    for item in corpus:
        by_base[item["base_ID"]][item["Cond"]] = item

    base_ids = sorted(by_base.keys())
    per_condition = TARGETS_PER_LIST // 3

    for attempt in range(1, ASSIGN_RETRIES + 1):
        lists = [[] for _ in range(NUM_LISTS)]
        remaining = {
            i: {"a": per_condition, "b": per_condition, "c": per_condition}
            for i in range(NUM_LISTS)
        }
        order = base_ids[:]
        random.shuffle(order)
        ok = True

        for base in order:
            items_for_base = by_base[base]
            conds_available = list(items_for_base.keys())
            conds_available.sort(
                key=lambda c: sum(remaining[i].get(c, 0) for i in range(NUM_LISTS))
            )
            chosen_for_base = {}

            for cond in conds_available:
                candidates = [
                    i for i in range(NUM_LISTS)
                    if remaining[i].get(cond, 0) > 0
                    and i not in chosen_for_base.values()
                ]
                if not candidates:
                    ok = False
                    break
                candidates.sort(
                    key=lambda i: (remaining[i][cond], random.random()),
                    reverse=True,
                )
                pick = candidates[0]
                chosen_for_base[cond] = pick
                remaining[pick][cond] -= 1

            if not ok:
                break

            for cond, li in chosen_for_base.items():
                lists[li].append(by_base[base][cond].copy())

        if ok and all(len(l) == TARGETS_PER_LIST for l in lists):
            print(f"  Assignment succeeded after {attempt} attempt(s).")
            return lists

    return None



# Balanced Sequencing

def sequence_list_with_constraints(items):
    """Order one list so adjacency constraints are satisfied.

    Greedy with scored candidates: at each step prefer items that violate
    no constraint; if none exist, take the lowest-penalty option. Restart
    from a fresh random shuffle up to SEQUENCE_RESTARTS times and keep the
    sequence with the fewest "real" errors.
    """
    best_seq = None
    best_real_errors = float("inf")

    start_time = time.time()
    print("  Sequencing ", end="", flush=True)

    for restart in range(SEQUENCE_RESTARTS):
        if restart > 0 and restart % 500 == 0:
            print(".", end="", flush=True)

        remaining = items[:]
        random.shuffle(remaining)

        # First item: random pick (do not pre-sort by difficulty — that
        # tends to cluster targets at the beginning).
        seq = []
        first = remaining.pop(0)
        first["placement_status"] = "START"
        seq.append(first)

        attempt_failed = False

        while remaining:
            last_n_tags = [x["strict_tags"] for x in seq[-TAG_DISTANCE:]]
            last_conds = [x["Cond"] for x in seq[-MAX_CONDITION_REPEAT:]]
            last_types = [x["Type"] for x in seq[-MAX_TYPE_REPEAT:]]

            chosen = None
            chosen_score = float("inf")

            for cand in remaining:
                # 1. Strict semantic tag conflict — hard reject.
                strict_conflict = False
                cand_tags = cand["strict_tags"]
                if cand_tags:
                    for prev_tags in last_n_tags:
                        if not prev_tags.isdisjoint(cand_tags):
                            strict_conflict = True
                            break
                if strict_conflict:
                    continue

                score = 0

                # 2. Same condition repeated MAX_CONDITION_REPEAT times.
                if len(last_conds) >= MAX_CONDITION_REPEAT:
                    if all(c == cand["Cond"] for c in last_conds):
                        score += 100

                # 3. Valence repeat with immediate predecessor.
                if score == 0:
                    prev_val = seq[-1]["valence_tags"]
                    if prev_val and not prev_val.isdisjoint(cand["valence_tags"]):
                        score += 10

                # 4. Same Type (Target/Filler) repeated MAX_TYPE_REPEAT times.
                if len(last_types) >= MAX_TYPE_REPEAT:
                    if all(t == cand["Type"] for t in last_types):
                        score += 500

                if score == 0:
                    chosen = cand
                    chosen["placement_status"] = "PERFECT"
                    break

                if chosen is None or score < chosen_score:
                    chosen = cand
                    chosen_score = score
                    if chosen_score >= 500:
                        chosen["placement_status"] = "VIOLATION_TYPE_SEQ"
                    elif chosen_score >= 100:
                        if chosen["Cond"] == "filler":
                            chosen["placement_status"] = "INFO_FILLER_SEQ"
                        else:
                            chosen["placement_status"] = "VIOLATION_COND_TARGET"
                    else:
                        chosen["placement_status"] = "VIOLATION_VALENCE"

            if chosen:
                seq.append(chosen)
                remaining.remove(chosen)
            else:
                attempt_failed = True
                break

        if not attempt_failed:
            real_errors = 0
            for s in seq:
                stat = s["placement_status"]
                if stat in ("VIOLATION_COND_TARGET", "VIOLATION_VALENCE"):
                    real_errors += 1
                if stat == "VIOLATION_TYPE_SEQ" and s["Type"] == "Target":
                    real_errors += 1

            if real_errors < best_real_errors:
                best_real_errors = real_errors
                best_seq = seq

                if best_real_errors <= STOP_THRESHOLD:
                    print(f" done ({best_real_errors} real errors, "
                          f"{time.time() - start_time:.2f}s)")
                    return best_seq

    print(f" done (best: {best_real_errors} real errors)")
    return best_seq if best_seq else seq



# Position check (post-hoc)

def position_check(sequenced_lists):
    """Report condition distribution in first vs second half of each list.

    A serial-position confound would show up as a condition skewed heavily
    toward one half. Reported for archival/inspection purposes.
    """
    lines = ["", "Serial-position check (condition counts: first half | second half)"]
    for i, lst in enumerate(sequenced_lists, 1):
        half = len(lst) // 2
        first = Counter(x["Cond"] for x in lst[:half])
        second = Counter(x["Cond"] for x in lst[half:])
        lines.append(f"  List {i}: H1={dict(first)} | H2={dict(second)}")
    return lines



# Export (4 sheets)

def export_lists(sequenced_lists, filename):
    print(f"\nExporting: {filename}")

    rows_mixed_emoji = []
    rows_targets_emoji = []
    rows_mixed_filenames = []
    rows_targets_filenames = []

    for i, lst in enumerate(sequenced_lists, 1):
        for item in lst:
            # Image filename for the emoji slot.
            if item["Type"] == "Target":
                filename_img = f"{item['ID']}{item['Cond']}.png"
            else:
                filename_img = f"{item['ID']}.png"

            is_target = (item["Type"] == "Target")

            # Two segmentations: one with the emoji character, one with the
            # image filename in its place.
            segs_emoji = get_segments(item["Phrase"], item["Emoji"], item["Emoji"], is_target)
            segs_file = get_segments(item["Phrase"], item["Emoji"], filename_img, is_target)

            base_row = {
                "Group": i,
                "ID": item["ID"],
                "Condition": item["Cond"],
                "Type": item["Type"],
                "Sentence": item["Phrase"],
                "Emoji": item["Emoji"],
            }

            # Mixed (all items), emoji as character.
            row_me = base_row.copy()
            for s_idx, seg in enumerate(segs_emoji, 1):
                row_me[f"Word_{s_idx}"] = seg
            row_me["Status"] = item["placement_status"]
            rows_mixed_emoji.append(row_me)

            # Targets only, emoji as character.
            if is_target:
                row_te = {k: v for k, v in base_row.items() if k != "Type"}
                for s_idx, seg in enumerate(segs_emoji, 1):
                    row_te[f"Word_{s_idx}"] = seg
                row_te["Status"] = item["placement_status"]
                rows_targets_emoji.append(row_te)

            # Mixed, emoji as image filename.
            row_mf = base_row.copy()
            row_mf["Emoji"] = filename_img
            for s_idx, seg in enumerate(segs_file, 1):
                row_mf[f"Word_{s_idx}"] = seg
            row_mf["Status"] = item["placement_status"]
            rows_mixed_filenames.append(row_mf)

            # Targets only, emoji as image filename.
            if is_target:
                row_tf = {k: v for k, v in base_row.items() if k != "Type"}
                row_tf["Emoji"] = filename_img
                for s_idx, seg in enumerate(segs_file, 1):
                    row_tf[f"Word_{s_idx}"] = seg
                row_tf["Status"] = item["placement_status"]
                rows_targets_filenames.append(row_tf)

    def format_df(rows):
        df = pd.DataFrame(rows)
        cols = list(df.columns)
        start_cols = ["Group", "ID", "Condition", "Type", "Sentence", "Emoji"]
        word_cols = sorted(
            [c for c in cols if c.startswith("Word_")],
            key=lambda x: int(x.split("_")[1]),
        )
        end_cols = ["Status"]
        ordered = (
            [c for c in start_cols if c in cols]
            + word_cols
            + [c for c in end_cols if c in cols]
        )
        return df.reindex(columns=ordered)

    try:
        with pd.ExcelWriter(filename, engine="openpyxl") as writer:
            format_df(rows_mixed_emoji).to_excel(writer, sheet_name="Mixed_Emoji", index=False)
            format_df(rows_targets_emoji).to_excel(writer, sheet_name="Targets_Emoji", index=False)
            format_df(rows_mixed_filenames).to_excel(writer, sheet_name="Mixed_Filenames", index=False)
            format_df(rows_targets_filenames).to_excel(writer, sheet_name="Targets_Filenames", index=False)
        print("  4 sheets written.")
    except Exception as e:
        print(f"  ERROR during export: {e}")
        return

    print("Done.")


def write_report(sequenced_lists, filename):
    """Save a permanent per-list violation summary alongside the Excel output."""
    lines = ["Sequencing report", "=================", ""]
    for i, lst in enumerate(sequenced_lists, 1):
        cnt = Counter(x["placement_status"] for x in lst)
        lines.append(f"List {i}: {dict(cnt)}")
    lines.extend(position_check(sequenced_lists))
    with open(filename, "w", encoding="utf-8") as f:
        f.write("\n".join(lines) + "\n")
    print(f"Report written: {filename}")



# Main

def main():
    os.makedirs("results", exist_ok=True)

    targets = read_targets(TARGETS_CORPUS, TARGETS_SHEET, SELECTED_IDS_CSV)
    fillers = read_fillers(FILLERS_FILE, FILLERS_SHEET)
    if not targets or not fillers:
        return

    print("\n--- 1. Distribution across lists ---")
    target_lists = assign_triplets_with_restarts(targets)
    if not target_lists:
        print("  ERROR: assignment failed within the retry budget.")
        return

    print("\n--- 2. Balanced sequencing ---")
    final_lists = []
    for i, list_targets in enumerate(target_lists, 1):
        print(f"\nList {i}:")
        full_list = list_targets + [f.copy() for f in fillers]
        seq = sequence_list_with_constraints(full_list)
        final_lists.append(seq)
        cnt = Counter(x["placement_status"] for x in seq)
        print(f"  Status counts: {dict(cnt)}")

    export_lists(final_lists, OUTPUT_FILE)
    write_report(final_lists, REPORT_FILE)


if __name__ == "__main__":
    main()