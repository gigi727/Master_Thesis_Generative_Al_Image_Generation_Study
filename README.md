# How Generative AI Image Generation Aligns with and Shapes Human Mental Images

This repository contains the public analysis pipeline for a master thesis on how generative AI image generation aligns with and shapes human mental images. The project studies GenAI image generators as tools for externalizing internal mental representations and as feedback systems that may slightly reshape what users imagine during iterative interaction.

The study uses an exploratory mixed-methods design. It combines a pre-survey on everyday GenAI image-generation practices with a three-round image-generation task in which participants repeatedly generated AI images from a target word, compared the outputs with their own mental images, and reported whether their mental image changed across iterations.

## Project focus

The central research interest is the interaction between three elements:

1. **mental images**: the internal visual representations participants form before image generation;
2. **text prompts**: the linguistic interface through which participants communicate their mental image to a GenAI image generator;
3. **AI-generated outputs**: external visual representations that participants evaluate, refine, and may partially integrate into their own imagination.

The thesis asks whether GenAI image generation merely helps users make inner images visible, or whether the generated images also become part of a feedback loop that influences the user's mental representation.

## Study design

### Pre-survey

The pre-survey captures general experience with GenAI image generation, including usage contexts, tools, goals, prompting practices, perceived benefits, failures, and concerns. After data cleaning, the valid pre-survey sample contains **N = 189** responses. Of these, **N = 156** participants report prior experience with GenAI image-generation tools.

### Main study

The main study uses a within-subject, three-round image-generation task. Participants first complete the 16-item Vividness of Visual Imagery Questionnaire (VVIQ). They then receive a randomly assigned target word and generate an AI image across three iterations. After each generated image, they rate agreement between the AI-generated output and their mental image and report whether their mental image changed.

After cleaning and matching, the final valid main-study sample contains **N = 46** matched participants. The final anonymized analysis dataset combines pre-survey variables, main-study variables, VVIQ scores, agreement ratings, prompt coding variables, and derived analysis variables.

## Main analytical components

The repository supports the following analysis steps:

- data cleaning, exclusion documentation, and matching of pre-survey and main-study responses;
- creation of a final anonymized analysis dataset;
- VVIQ scoring and imagery-vividness summaries;
- prompt-coding integration and sequence-level prompt analysis;
- descriptive analyses of GenAI usage, prompting practices, image agreement, mental-image change, authorship, and preference;
- stratified plots and tables by VVIQ level, GenAI usage duration, and target-word category;
- final image-agreement analysis using linear mixed-effects models, controlled models, ordinal robustness checks, and change-score summaries;
- generation of a project-wide HTML master index and a curated `Output for Research` folder.

## Prompt-coding framework

The prompt-coding scheme treats each participant's three-round task as one ordered prompting trajectory. Each round is coded separately, and the complete sequence is then classified as an additional analytical unit.

Round-level coding includes:

- `F1`: generation prompt;
- `F2`: edit prompt;
- `F3`: re-generation / complete reformulation;
- `F4`: meta- / repair prompt;
- `L1`–`L3`: prompt-engineering level;
- vibe prompting: whether the prompt specifies mood, atmosphere, or subjective impression.

Sequence-level coding groups the ordered function sequence into five trajectory types:

- `S1`: iterative refinement (`F1-F2-F2`);
- `S2`: double re-specification (`F1-F3-F3`);
- `S3`: re-specification with final refinement (`F1-F3-F2`);
- `S4`: refinement followed by re-specification (`F1-F2-F3`);
- `S5`: refinement with reversion (`F1-F2-F4`).

The full codebook is available in [`docs/codebook/prompt_coding_codebook.md`](docs/codebook/prompt_coding_codebook.md).

## Key findings reflected in the analysis pipeline

The thesis findings suggest that GenAI image generators can help users externalize mental images, but the interaction is not a simple linear optimization process. Participants generally report relatively high agreement between their mental images and the generated outputs. However, repeated iterations do not automatically produce higher alignment for everyone. Instead, the process is adaptive: users refine prompts in response to generated images, sometimes accept small AI-suggested changes, and sometimes experience divergence.

Abstract target words leave more room for AI influence and are more often associated with vibe prompting. Concrete words remain more closely tied to participants' original mental images. Overall, the reported influence of AI-generated images is mostly incremental, such as refined details or added elements, rather than a full replacement of the original imagination.

## Repository structure

```text
.
├── scripts/              # R scripts for the full analysis pipeline
├── data_raw/             # Private only: Qualtrics/raw data must not be committed
├── data_processed/       # Private only: intermediate datasets must not be committed
├── data_final/           # Public only: final anonymized analysis dataset
├── data_output/          # Public only: curated master index and Output for Research
├── docs/                 # Codebook and project documentation
├── analysis_notes/       # Public-release checklist and release notes
├── install_packages.R    # Package installation helper
├── run_all.R             # Pipeline runner
└── README.md             # Repository overview
```

## Public data and output policy

This repository is designed for public release. It intentionally excludes raw and directly identifying material.

Publicly allowed:

- all R scripts in `scripts/`;
- `data_final/final_analysis_dataset_anonymized.csv`;
- `data_final/final_analysis_dataset_anonymized.rds`;
- optional anonymization summary: `data_final/final_analysis_dataset_anonymized_export_summary.csv`;
- `data_output/project_master_index/00_master_export_index.html`;
- selected HTML tables, RTF tables, and PNG plots under `data_output/project_master_index/Output for Research/`;
- documentation files such as this README, the codebook, and release notes.

Not public:

- Qualtrics raw exports;
- e-mail addresses, IP addresses, Response IDs, Case IDs, coordinates, or other direct identifiers;
- non-anonymized datasets;
- intermediate cleaning, matching, review, or coding datasets;
- private prompt-coding input files;
- uploaded participant images;
- CSV, XLSX, TXT, or console-summary outputs from `data_output/`.

## Important privacy note

The private prompt-coding source file is intentionally excluded from the repository. In the uploaded working version, the `Case_ID` field contained IP-like values. For public release, only the final anonymized dataset should be committed. The anonymization script removes direct identifier columns and checks for remaining IP-like strings in character columns before export.

## Reproduction

### Public mode: reproduce outputs from the final anonymized dataset

Place the final anonymized dataset in `data_final/`:

```text
data_final/final_analysis_dataset_anonymized.csv
data_final/final_analysis_dataset_anonymized.rds
```

Then run in R:

```r
source("install_packages.R")
source("run_all.R")
```

This mode rebuilds the public analysis outputs from the final anonymized dataset and then generates the project-wide master index.

### Private mode: full local rebuild from raw data

This mode is only for local use and must not be used for a public GitHub release unless all private files are excluded afterward.

1. Place private Qualtrics raw files in `data_raw/`.
2. Place the private prompt-coding input file locally, without committing it.
3. Run:

```r
Sys.setenv(RUN_PRIVATE_REBUILD = "true")
source("run_all.R")
```

This executes the private cleaning, matching, prompt-coding join, and anonymized dataset creation steps.

## Master index and curated research outputs

After running the pipeline, open:

```text
data_output/project_master_index/00_master_export_index.html
```

The GitHub-ready research outputs are collected under:

```text
data_output/project_master_index/Output for Research/
```

This folder should contain only:

- `.html` tables;
- `.rtf` tables;
- `.png` plots.

CSV, XLSX, TXT, raw data, intermediate datasets, and console summaries are intentionally excluded from the public output folder.

## Current archive status

This prepared archive contains the public scripts, documentation, repository structure, and release rules. The final anonymized dataset and generated `Output for Research` files are not included unless they are added locally after the final pipeline run.
