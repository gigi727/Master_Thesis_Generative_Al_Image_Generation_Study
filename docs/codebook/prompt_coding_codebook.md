# Prompt-Coding-Codebook

Dieses Codebook dokumentiert die manuelle Codierung der Drei-Runden-Prompt-Sequenzen.

## Einheit der Analyse

Eine Beobachtung entspricht einem Fall: eine Person, eine Datenzeile und eine vollständige Drei-Runden-Sequenz. Die Runden 1, 2 und 3 werden zunächst einzeln codiert. Danach wird die gesamte Sequenz als zusätzliche analytische Einheit klassifiziert.

## Round-level coding

Jede Runde wird in derselben Reihenfolge codiert.

### Function code

| Code | Bedeutung | Entscheidungsregel |
|---|---|---|
| F1 | Generation prompt | Der Prompt beschreibt ein Bild von Grund auf. |
| F2 | Edit prompt | Der Prompt bezieht sich auf ein bestehendes Bild und verändert es lokal. |
| F3 | Re-generation / complete reformulation | Eine spätere Runde beschreibt das Bild erneut fast vollständig von Grund auf. |
| F4 | Meta-/repair prompt | Der Prompt korrigiert den Prozess, z. B. Rückkehr zu einer vorherigen Version. |

Kernregel: Wenn der Prompt klar auf ein bestehendes Bild verweist, wird er als F2 codiert. Wenn er erneut eine vollständige Bildbeschreibung formuliert, wird er als F3 codiert.

### Prompt-engineering level

Der Prompt-engineering level wird für F1, F2, F3 und F4 codiert.

| Level | Bedeutung | Kurzbeschreibung |
|---|---|---|
| L1 | Low | Basismotiv, wenig Kontrolle, kaum Komposition, kaum Stil, keine klaren Constraints. |
| L2 | Medium | Kontrollierbares Motiv mit mehreren relevanten Spezifikationen, z. B. Setting, Perspektive oder Stil. |
| L3 | High | Starke Kontrolle über Bildwirkung, Komposition und Stil, mit präzisen räumlichen Hinweisen, Details und Constraints. |

Leitfragen: Wie präzise ist das Subjekt? Wie klar sind Form, Komposition oder Perspektive? Wie ausgearbeitet sind Stil, Modifier und Qualitätsangaben? Gibt es Constraints oder gezielte Begrenzungen?

### Vibe prompting

Vibe prompting ist vorhanden, wenn ein Prompt nicht nur sichtbare Elemente, sondern gezielt Stimmung, Atmosphäre oder subjektive Wirkung beschreibt. Typische Indikatoren sind z. B. „warm“, „cozy“, „tense“, „nostalgic“, „mysterious“, „eerie“, „calm“, „dreamlike“ oder „melancholic“.

Kernregel: Wenn der Prompt primär sichtbare Entitäten, Komposition oder Szenenstruktur spezifiziert, wird kein Vibe-Code vergeben. Wenn er ausdrücklich Stimmung, Atmosphäre oder emotionalen Ton aufruft, ist Vibe prompting vorhanden.

## Sequence-level coding

Nach der Codierung der drei Runden wird die geordnete Funktionssequenz gebildet, z. B. `F1-F2-F2`. Die Reihenfolge ist analytisch entscheidend: `F1-F2-F3` ist nicht identisch mit `F1-F3-F2`.

| Sequence type | Funktionssequenz | Bedeutung |
|---|---:|---|
| S1 – Iterative Refinement | F1-F2-F2 | Initiale Spezifikation mit zwei anschließenden lokalen Verfeinerungen. |
| S2 – Double Re-specification | F1-F3-F3 | Wiederholte vollständige Reformulierung statt lokaler Bearbeitung. |
| S3 – Re-specification with Final Refinement | F1-F3-F2 | Erst globaler Neustart, danach lokale Schlusskorrektur. |
| S4 – Refinement Followed by Re-specification | F1-F2-F3 | Zunächst lokale Korrektur, danach Wechsel zu vollständiger Reformulierung. |
| S5 – Refinement with Reversion | F1-F2-F4 | Initiale Spezifikation, Edit und abschließende Prozesskorrektur/Rückkehr. |

Die Sequenzklassifikation gruppiert Fälle exakt nach ihrer geordneten Funktionssequenz. Es handelt sich nicht um ein distanzbasiertes Clustering, sondern um eine typologische Gruppierung nominaler Codes unter Berücksichtigung der Reihenfolge.

## Literaturbasis

- Liu, V., & Chilton, L. B. (2022). *Design Guidelines for Prompt Engineering Text-to-Image Generative Models*. CHI. DOI: 10.1145/3491102.3501825.
- Xie, Y., Pan, Z., Ma, J., Jie, L., & Mei, Q. (2023). *A Prompt Log Analysis of Text-to-Image Generation Systems*. WWW ’23. DOI: 10.1145/3543507.3587430.
- Oppenlaender, J. (2024). *A taxonomy of prompt modifiers for text-to-image generation*. Behaviour & Information Technology. DOI: 10.1080/0144929X.2023.2286532.
- Madaan, A., et al. (2023). *Self-Refine: Iterative Refinement with Self-Feedback*. NeurIPS. arXiv:2303.17651.
