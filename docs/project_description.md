# Project description

## Thesis title

**How Generative AI Image Generation Aligns with and Shapes Human Mental Images**

## Short description

This project analyzes how non-expert users use generative AI image generators to externalize personal mental images and how repeated interaction with generated outputs may influence what users imagine. The study treats image generation as an iterative human-AI feedback process: participants begin with an internal mental image, translate it into a prompt, receive an AI-generated image, evaluate its alignment, and refine or reformulate their prompt over three rounds.

## Research context

Prior research on text-to-image generation has often focused on professional, creative, or design-oriented contexts. This project instead examines everyday users and personal mental images. The central question is not only whether AI-generated images match what people had in mind, but also whether the AI output feeds back into the imaginative process.

## Empirical design

The study uses an exploratory mixed-methods design with two connected parts:

1. **Pre-survey**: captures general GenAI image-generation practices, prompting behavior, perceived benefits, failures, and concerns.
2. **Main study**: uses a three-round image-generation task in which participants repeatedly compare generated images with their own mental images.

The final public analysis is based on a final anonymized matched dataset derived from participants who completed both parts of the study.

## Data basis

- Valid pre-survey sample after cleaning: **N = 189**.
- Experienced GenAI image-generation users in the pre-survey: **N = 156**.
- Valid matched main-study sample: **N = 46**.
- Final dataset: matched pre-survey and main-study variables, VVIQ scores, prompt-coding variables, agreement ratings, mental-image-change variables, and derived analysis variables.

## Analysis logic

The repository contains scripts for:

- cleaning and matching pre-survey and main-study data;
- generating the final anonymized dataset;
- scoring the VVIQ;
- joining and checking prompt-coding data;
- producing descriptive tables and figures;
- analyzing image-agreement ratings across three iterations;
- comparing abstract and concrete target words;
- creating plots by VVIQ level, GenAI use duration, and target-word category;
- estimating linear mixed-effects models and robustness checks;
- exporting publication-oriented HTML/RTF tables and PNG plots.

## Prompt analysis

Prompts are analyzed as three-round sequences. Each round is coded by function, prompt-engineering level, and vibe prompting. The full sequence is then classified as an ordered prompting trajectory. This allows the analysis to distinguish between incremental refinement, repeated re-specification, mixed strategies, and repair or reversion behavior.

## Main empirical interpretation

The results indicate that GenAI image generators can support the externalization of mental images. Participants generally report relatively high agreement between generated images and their own mental images. However, improvement across iterations is not automatic or uniform. Instead, users adapt to the generated outputs by refining, reformulating, accepting small changes, or occasionally experiencing divergence.

Abstract target words appear to leave more room for AI influence and mood-based prompting. Concrete words remain more closely connected to participants' initial mental images. Overall, generated images tend to influence mental images through minor changes, such as added elements or refined details, rather than replacing the original imagination.

## Public-release boundary

The public repository includes only scripts, documentation, the final anonymized dataset, and selected publication-ready outputs. It excludes raw Qualtrics data, direct identifiers, intermediate datasets, private prompt-coding sources, uploaded participant images, and non-anonymized tabular exports.
