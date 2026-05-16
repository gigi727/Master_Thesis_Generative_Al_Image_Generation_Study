# Prompt-Coding Codebook

This codebook documents the manual coding of three-round prompt sequences.

## Unit of analysis

One observation corresponds to one case: one person, one data row, and one complete three-round sequence. Rounds 1, 2, and 3 are first coded individually. The entire sequence is then classified as an additional analytical unit.

## Round-level coding

Each round is coded in the same order.

### Function code

| Code | Meaning | Decision rule |
|---|---|---|
| F1 | Generation prompt | The prompt describes an image from scratch. |
| F2 | Edit prompt | The prompt refers to an existing image and modifies it locally. |
| F3 | Re-generation / complete reformulation | A later round describes the image again almost entirely from scratch. |
| F4 | Meta-/repair prompt | The prompt corrects the process, e.g., returning to a previous version. |

Core rule: If the prompt clearly refers to an existing image, it is coded as F2. If it formulates a complete image description again, it is coded as F3.

### Prompt-engineering level

The prompt-engineering level is coded for F1, F2, F3, and F4.

| Level | Meaning | Short description |
|---|---|---|
| L1 | Low | Basic motif, little control, hardly any composition, hardly any style, no clear constraints. |
| L2 | Medium | Controllable motif with several relevant specifications, e.g., setting, perspective, or style. |
| L3 | High | Strong control over image effect, composition, and style, with precise spatial cues, details, and constraints. |

Guiding questions: How precise is the subject? How clear are form, composition, or perspective? How elaborated are style, modifiers, and quality specifications? Are there constraints or targeted limitations?

### Vibe prompting

Vibe prompting is present when a prompt describes not only visible elements but also deliberately specifies mood, atmosphere, or subjective effect. Typical indicators include, for example, “warm,” “cozy,” “tense,” “nostalgic,” “mysterious,” “eerie,” “calm,” “dreamlike,” or “melancholic.”

Core rule: If the prompt primarily specifies visible entities, composition, or scene structure, no vibe code is assigned. If it explicitly evokes mood, atmosphere, or emotional tone, vibe prompting is present.

## Sequence-level coding

After coding the three rounds, the ordered function sequence is formed, e.g., `F1-F2-F2`. The order is analytically decisive: `F1-F2-F3` is not identical to `F1-F3-F2`.

| Sequence type | Function sequence | Meaning |
|---|---:|---|
| S1 – Iterative Refinement | F1-F2-F2 | Initial specification followed by two local refinements. |
| S2 – Double Re-specification | F1-F3-F3 | Repeated complete reformulation instead of local editing. |
| S3 – Re-specification with Final Refinement | F1-F3-F2 | First a global restart, followed by a local final correction. |
| S4 – Refinement Followed by Re-specification | F1-F2-F3 | First a local correction, followed by a shift to complete reformulation. |
| S5 – Refinement with Reversion | F1-F2-F4 | Initial specification, edit, and final process correction/reversion. |

The sequence classification groups cases exactly according to their ordered function sequence. It is not distance-based clustering, but a typological grouping of nominal codes while taking order into account.

## Literature base

- Liu, V., & Chilton, L. B. (2022). *Design Guidelines for Prompt Engineering Text-to-Image Generative Models*. CHI. DOI: 10.1145/3491102.3501825.
- Xie, Y., Pan, Z., Ma, J., Jie, L., & Mei, Q. (2023). *A Prompt Log Analysis of Text-to-Image Generation Systems*. WWW ’23. DOI: 10.1145/3543507.3587430.
- Oppenlaender, J. (2024). *A taxonomy of prompt modifiers for text-to-image generation*. Behaviour & Information Technology. DOI: 10.1080/0144929X.2023.2286532.
- Madaan, A., et al. (2023). *Self-Refine: Iterative Refinement with Self-Feedback*. NeurIPS. arXiv:2303.17651.
