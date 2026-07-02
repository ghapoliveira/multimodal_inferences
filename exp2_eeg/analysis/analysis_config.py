# Shared utilities for the ERP grand-average and cluster-stats notebooks

import re
import sys
from pathlib import Path
import numpy as np
import pandas as pd
import mne
from natsort import natsorted

# Project root + config import

def find_project_root(current_path, marker="config.py"):

    # Walk upward from 'current_path' until a folder containing 'marker' is found
    path = Path(current_path).resolve()
    for parent in [path] + list(path.parents):
        if (parent / marker).exists():
            return parent
    return path


_root = find_project_root(Path.cwd())
if str(_root) not in sys.path:
    sys.path.append(str(_root))

from config import EEG_DIR, BEHAV_DIR, conditions, TRIGGER_MAP, ITEMS_TO_DROP, BAD_SUBJ

mne.set_log_level("error")

# Global configuration

PIPELINE_CONFIGS = [
    {"name": "standard",     "subdir": None,                     "suffix": "-ave.fif",     "exclude": BAD_SUBJ},
    {"name": "baseline",     "subdir": "baseline_check",       "suffix": "-bc-ave.fif",  "exclude": BAD_SUBJ},
    {"name": "alt_pipeline", "subdir": "alternative_pipeline", "suffix": "_alt-ave.fif", "exclude": BAD_SUBJ},
]

def topo_window(config):

    # (tmin, tmax) for topomap time points; baseline inspects the pre-stimulus window, so it is different
    if config["name"] == "baseline":
        return -1.2, 0.0
    return -0.2, 1.0

# Subject exclusion + trigger alignment

def is_bad_subject(path, bad_list):

    # True if 'path' belongs to an excluded participant
    # Ensures the excluded participants ids match exactly the bad participants list
    text = str(path)
    for bad in bad_list:
        if re.search(rf"{re.escape(bad)}(?=[/\\_-]|$)", text):
            return True
    return False

# Evoked loading + difference waves

def pipeline_glob(config, filename_pattern):
    if config["subdir"] is not None:
        return f"participant_*/{config['subdir']}/{filename_pattern}"
    return f"participant_*/{filename_pattern}"

def load_evokeds(config, verbose=False):

    # Load per-subject evoked files for one pipeline, grouped by condition.
    data_files = list(EEG_DIR.glob(pipeline_glob(config, f"*{config['suffix']}")))
    data_files = natsorted([str(f) for f in data_files])

    evokeds = {}
    for c in conditions:
        relevant = [
            f for f in data_files
            if f"_{c}{config['suffix']}" in f
            and "-Condition" not in Path(f).name
            and not is_bad_subject(f, config["exclude"])
        ]
        if not relevant:
            print(f"⚠️ Warning: No files found for {c} in {config['name']}")
        evokeds[c] = [mne.read_evokeds(f)[0].set_montage("easycap-M1") for f in relevant]
        if verbose:
            print(f"{c} : {len(evokeds[c])} subjects loaded.")
    return evokeds

def compute_difference_waves(evokeds):
    # Per-subject paired difference waves (weights=[1, -1] => first minus second)
    n = len(evokeds["ConditionA"])
    return {
        "B_A": [mne.combine_evoked([evokeds["ConditionB"][s], evokeds["ConditionA"][s]], weights=[1, -1]) for s in range(n)],
        "C_A": [mne.combine_evoked([evokeds["ConditionC"][s], evokeds["ConditionA"][s]], weights=[1, -1]) for s in range(n)],
        "C_B": [mne.combine_evoked([evokeds["ConditionC"][s], evokeds["ConditionB"][s]], weights=[1, -1]) for s in range(n)],
    }