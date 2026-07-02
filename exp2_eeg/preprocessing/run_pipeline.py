import sys
import os
import time
import gc
import papermill as pm
from pathlib import Path

def find_project_root(current_path, marker='config.py'):
    path = Path(current_path).resolve()
    for parent in [path] + list(path.parents):
        if (parent / marker).exists():
            return parent
    return path 

root_dir = find_project_root(Path.cwd())
sys.path.append(str(root_dir))

from config import EEG_DIR 

# Force Matplotlib and MNE to run headlessly
os.environ['QT_QPA_PLATFORM'] = 'offscreen'
os.environ['MNE_BROWSER_BACKEND'] = 'matplotlib'

# 1. Participants
# range(1, 30) runs Participant 1 through 29
participants = [f'participant_{i}' for i in range(1, 30)]

# 2. Pipeline jobs (commenting and uncommenting here will change which pipeline is activated: 
# running all will run all three pipelines — the normal one, the one without items 2, 61 and 62 and the baseline check)
pipeline_jobs = [
    #{'notebook': '1_filtering.ipynb', 'label': 'standard', 'params': {}},
    #{'notebook': '2_removing_artifacts.ipynb', 'label': 'standard', 'params': {}},
    
    {'notebook': '3_segmenting_epochs.ipynb', 'label': 'standard', 'params': {'IS_BASELINE_CHECK': False}},
    {'notebook': '3_segmenting_epochs.ipynb', 'label': 'baseline', 'params': {'IS_BASELINE_CHECK': True}},
    
    {'notebook': '4_averaging.ipynb', 'label': 'standard', 'params': {'IS_BASELINE_CHECK': False, 'ALT_PIPELINE': False}},
    {'notebook': '4_averaging.ipynb', 'label': 'baseline', 'params': {'IS_BASELINE_CHECK': True, 'ALT_PIPELINE': False}},
    {'notebook': '4_averaging.ipynb', 'label': 'alt_pipeline', 'params': {'IS_BASELINE_CHECK': False, 'ALT_PIPELINE': True}}
]

notebook_dir = Path('.')

print(f"🚀 Starting batch preprocessing for {len(participants)} participants...")
start_time = time.time()
failures = []

for p in participants:
    iter_start = time.time()
    print(f"\n🧵 Processing: {p}")
    
    # Base report directory for the participant
    subj_report_dir = EEG_DIR / p / 'reports'

    for job in pipeline_jobs:
        nb_file = job['notebook']
        label = job['label']
        custom_params = job['params']
            
        print(f"  → Running {nb_file} [{label.upper()}]...")
            
        input_path = notebook_dir / nb_file

        # Create a specific sub-folder based on the job label (e.g., reports/baseline)
        if label == 'standard':
            job_report_dir = EEG_DIR / p / 'reports'
        elif label == 'baseline':
            job_report_dir = EEG_DIR / p / 'baseline_check' / 'baseline_reports'
        elif label == 'alt_pipeline':
            job_report_dir = EEG_DIR / p / 'alternative_pipeline' / 'alt_reports'
            
        job_report_dir.mkdir(parents=True, exist_ok=True)

        nb_base_name = nb_file.replace('.ipynb', '')
        output_name = f"{p}_{nb_base_name}_{label}_report.ipynb"
        
        # Save the report into its dedicated sub-folder
        output_path = job_report_dir / output_name

        # Merge parameters
        run_params = dict(p_id=p, PAPERMILL_RUN=True)
        run_params.update(custom_params)

        try:
            pm.execute_notebook(
                input_path,
                output_path,
                parameters=run_params, 
                log_output=False 
            )
        except Exception as e:
            print(f"  ❌ FAILED at {nb_file} [{label.upper()}]: {e}")
            failures.append((p, f"{nb_file} [{label}]", str(e)))
            break
            
    elapsed = (time.time() - iter_start) / 60
    print(f"✅ Finished {p} in {elapsed:.1f} minutes.")

    # Clear memory
    gc.collect() 
    print(f"🧹 Memory cleared for {p}")

if failures:
    print("\n⚠️ INCOMPLETE PARTICIPANTS:")
    for p, step, err in failures:
        print(f"  {p} failed at {step}: {err[:80]}")
else:
    print("\n✅ All participants completed every step.")

print(f"\nTotal time: {(time.time()-start_time)/60:.1f} min")


# Windows command to shut down the computer (/s = shutdown, /t 60 = 60 second delay)
# Uncomment to shut the computer down after preprocessing

#os.system("shutdown /s /t 60")
#print("Shutting down in 60 seconds...")

# stop by typing shutdown /a in the terminal