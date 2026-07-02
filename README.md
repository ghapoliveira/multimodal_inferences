# Emojis and inferential processes: the neurophysiology of bridging in multimodal settings

Research compendium for a master's dissertation (Postgraduate Program in Linguistics, UFRJ) on **bridging inferences between text and emojis**. The project runs in two stages: a behavioral norming study that builds and screens the materials, and an EEG study that measures the neural response to the selected items.

The procedures were duly approved by the Committee of Ethics in Research of the Federal University of Rio de Janeiro (CEP - UFRJ) and are registered under the Certificate of Presentation for Ethical Evaluation (CAAE) number 93482425.1.0000.5286. In compliance with the Committee's ethical recommendations, no participant received monetary compensation, but only complementary hours certificates (when requested).

## The two experiments

```         
exp1_norming  ──(120 selected target items)────►  exp2_eeg
Likert norming (three scales)                 EEG (RSVP) study
of 150 sentence–emoji triads            cluster permutation test + rERP
        (PCIbex)                               (MNE, Julia, R)
```

- **`exp1_norming/`** — a PCIbex norming study of 150 sentence–emoji triads (450 items). The selection, `results/analysis_ph/targets_selected_ph.csv`, is the bridge to Experiment 2. See [`exp1_norming/README.md`](exp1_norming/README.md).

- **`exp2_eeg/`** — the EEG study. The 120 selected targets are interleaved with 120 fillers and presented in E-Prime during EEG recording, then analyzed with a spatio-temporal cluster permutation test and a regression-ERP (rERP). See [`exp2_eeg/README.md`](exp2_eeg/README.md).

The two are coupled: `exp2_eeg/create_lists_eeg.py` and `exp2_eeg/analysis/` read the targets **live** from `../exp1_norming/`, so the folders must stay side by side — the EEG targets can never drift from the norming selection.

## Layout

```         
.
├── README.md            ← you are here
├── exp1_norming/        norming study (materials, PCIbex, analysis)  + its own README
└── exp2_eeg/                EEG study (experiment, preprocessing, analysis) + its own README
```

Each experiment is self-documenting: its README has the full pipeline, the reproduction commands, and its dependency files. For python: `requirements.txt` / `environment.yml`; for R (`requirements_R.txt`); for Julia, see the toml files inside `exp2_eeg/analysis/statistics/rERP/`

## Reproducing

Clone the whole repository so both folders sit together, then follow each experiment's README in order — exp1 first (it produces the target selection exp2 depends on), exp2 second. Source materials and all non-identifiable result tables are included; only group data is made available.

## Data and archival

This repository includes the group-level results and materials for both experiments. Individual-level data are not included.

## Citation

> Pimentel de Oliveira, G. H. A. (2026). *Emojis and inferential processes: the neurophysiology of bridging in multimodal settings* \[Master's dissertation, Universidade Federal do Rio de Janeiro\]. Advisor: Profa. Dra. Marije Soto.

## Credits

The EEG preprocessing and cluster analysis follow Aaron Newman's procedure (https://neuraldatascience.io/) and uses his environment. The rERP analysis adapts Christoph Aurnhammer's code (https://github.com/caurnhammer/psyp23rerps).

>Note: Script development was AI-assisted (Google Gemini and Claude).