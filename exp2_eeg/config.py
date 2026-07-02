# config.py
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parent
REPO_ROOT = REPO_ROOT = PROJECT_ROOT.parent

# Core directories
EEG_DIR = PROJECT_ROOT / 'data' / 'eeg'
BEHAV_DIR = PROJECT_ROOT / 'data' / 'behavioral'
STIMULI_FILE = REPO_ROOT / 'exp1_norming' / 'results' / 'analysis_ph' / 'targets_selected_ph.csv'

# Experimental conditions
conditions = ['ConditionA', 'ConditionB', 'ConditionC']

# Behavioral TrigCode -> E-Prime and EEG trigger code correspondence 
TRIGGER_MAP = {
    64:  1004,  # ConditionA
    160: 1010,  # ConditionB
    112: 1007,  # ConditionC
    32:  1002   # Filler
}

# Items removed in the alternative pipeline
ITEMS_TO_DROP = [2, 61, 62]

# Participants excluded from group analyses (too few trials for a stable ERP)
BAD_SUBJ = ['participant_3']
