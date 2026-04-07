# CLAUDE.md

## Project Overview

This is a political science research project on multi-party vote buying and clientelism in Mexico. The project produces 2–3 academic papers from the CIDE-CSES 2015 National Electoral Study, merged with precinct-level electoral data.

## Principal Investigator

Sergio Béjar, political science professor affiliated with CIDE. Researches clientelism, vote buying, and electoral politics in Latin America. Values direct, honest feedback including critical assessments of findings, methodological limitations, and journal fit. Prefers Spanish and English interchangeably.

## Papers

### Paper 1 (primary): "Competitive Clientelism in Mexico's Electoral Precincts"
- **Target:** Electoral Studies
- **Status:** Manuscript drafted (paper1/manuscript/paper1.tex), all figures and tables complete
- **Core finding:** All parties buy votes in the same communities (cross-party correlations r = 0.34–0.56). Sequential elimination shows this is not supply-side herding (survives controls + state FE), is partially explained by community-level norms (acceptability vignette r = 0.143), and is not fully explained by shared contact infrastructure
- **Key decision:** Framed as demand-side theory of clientelistic competition, NOT as asymmetric information paper (earlier framing was abandoned)
- **Electoral data:** Larreguy municipal elections are the PRIMARY specification. Federal deputy data (Magar) is ROBUSTNESS ONLY in appendix. Do not substitute federal for municipal data.

### Paper 2: "From Contact to Gift" (contact networks)
- **Target:** Party Politics (but standalone viability under evaluation)
- **Status:** Analysis complete, replication script done, NO manuscript yet
- **Core finding:** PRI contact → PRI VB at r = 0.29 within precincts (strongest individual predictor). Phone > face-to-face. 35% of PRI recipients had no prior contact. PRI converts 50% of contacts, MORENA 23%.
- **Open question:** May be folded into Paper 1 unless phone finding survives multivariate logit with sección FE and all modes entered simultaneously. Run this test before writing a standalone manuscript.

### Paper 3: Social desirability / measurement (pending collaboration)
- **Target:** Political Analysis (with replication from Castro Cornejo's 2017 data)
- **Status:** Analysis complete, formal interaction test marginal (p = 0.086). Waiting on collaboration with Rodrigo Castro Cornejo (UMass Lowell) who has a between-subjects list experiment from a 2017 gubernatorial survey
- **Core finding:** PROSPERA–VB correlation vanishes in list experiment (direct r = +0.231, list r = +0.021). Non-beneficiaries under-report by 25pp, beneficiaries by 11pp.
- **Key decision:** Separated from Paper 1. If collaboration doesn't materialize, fold back into Paper 1 as supplementary section.

## Data Files

All data lives in `data/`. Key files:

| File | Description | N | Used in |
|------|-------------|---|---------|
| `BD.sav` | CIDE-CSES 2015 post-electoral survey (SPSS) | 1,200 obs, 446 vars | All papers |
| `larreguy_matched_precincts.csv` | Larreguy municipal elections, 200 matched precincts | 200 rows | Paper 1 |
| `dip2015.csv` | Magar federal deputy 2015, casilla-level | 148,833 rows | Paper 1 (robustness) |
| `dip2012.csv` | Magar federal deputy 2012, casilla-level | 143,132 rows | Paper 1 (robustness) |

**BD.sav is not public.** Do not commit to public repos. The `.gitignore` should exclude it.

### Survey Variable Reference (BD.sav)

**Vote buying (party-specific):** pcyc1=PAN, pcyc2=PRI, pcyc3=PRD, pcyc4=PVEM, pcyc5=MORENA, pcyc6=MC, pcyc7=other (1=yes, 2=no)

**Observed neighborhood VB:** pcyc8a–pcyc8g (same party order)

**Programmatic offers:** pcyc13, pcyc19. **Coercion:** pcyc14, pcyc20

**Program beneficiaries:** pcyc12=LICONSA, pcyc12a=PROCAMPO, pcyc12b=PROSPERA, pcyc16=scholarship, pcyc17=pension, pcyc18=food aid

**List experiment:** p10v2 (treatment, 5 items), p10v2a (control, 4 items). Within-subjects design (both lists to all respondents). Recode 7→0, drop 8/9.

**Acceptability vignette:** pcyc9a (1=totally acceptable to 5=totally unacceptable). Reverse-code for analysis (higher = more acceptable).

**Party contact:** p26_1 through p26_7 (1=contacted, 2=not). Modes: a=face-to-face, b=postal, c=phone, d=text, e=email, f=social media. Mode questions only asked if p26_X=1.

**Rally:** p50_5 (attended), p51_1–p51_8 (which party), p51b (recruitment: 1=neighbor, 2=leader, 3=party member), p51c (transport help)

**Demographics:** ps1=age (99=missing), ps2=sex (2=female), ps3=education (96=none, 98/99=missing), p9=party ID (3,4=PRI; 1,2=PAN; 97=none)

**Ethnicity/skin color:** ps23=self skin chromatic (1–10, 99=missing), ps24=self face match, ps26=ethnic ID (1=indigenous, 2=mestizo, 3=white), ps27=indigenous language (1=yes). NOTE: Interviewer-assessed variables pce and pfe are ALL ZEROS (not collected).

**Geocoding:** estado=state code, secc=sección electoral. Merge with Larreguy on state_code + precinct.

### Larreguy Variable Reference (larreguy_matched_precincts.csv)

Key variables: pri_mean, pan_mean, prd_mean (mean vote shares across elections), pri_sd/pan_sd (volatility), turnout_mean, n_elections, n_distinct_incumbents (alternations), share_PRI/PAN/PRD_valid_vote_pre2015 (most recent pre-2015 election), incumbent_party_pre2015, mun_winning_margin_pre2015, max_registered, turnout_pre2015.

## Analysis Conventions

- **Language:** R is the primary analysis language. Replication scripts are in R.
- **Figures:** 300 DPI PNG, serif font, minimal formatting, publication-ready
- **Tables:** CSV format for portability; formatted in LaTeX for manuscript
- **Within-sección correlations:** Demean both IV and DV at the sección level, then compute Pearson r. This is the standard approach throughout both papers.
- **ICC calculation:** One-way random effects ANOVA method (manual, not lme4), consistent across all DVs
- **Significance notation:** *** p<0.01, ** p<0.05, * p<0.10

## Key Empirical Facts (do not re-derive)

These have been verified across multiple specifications and should be treated as settled:

- ICC for any VB = 0.225
- Cross-party mean off-diagonal r = 0.41 (raw), 0.40 (controls), 0.35 (controls + state FE)
- PRI mean vote share → any VB: r = −0.202*** (Larreguy municipal data)
- PRI mean vote share → any VB: r = −0.092 n.s. (federal deputy data — expected attenuation)
- All volatility measures → VB: null (every specification, both electoral datasets)
- Acceptability → VB (sección level): r = +0.143, p = 0.043
- Within-sección: PRI contact → PRI VB: r = +0.292***
- Within-sección: indigenous language → PRI VB: r = +0.095***
- Within-sección: PROSPERA → any VB: r = +0.118***
- Within-sección: female, age, education, skin color → VB: all near zero
- PROSPERA → VB (direct question): r = +0.231***; PROSPERA → VB (list experiment): r = +0.021 n.s.
- Interaction test (PROSPERA × method): p = 0.086 (marginal)
- Larreguy merge: 1,010 obs, 200 precincts (missing DF + Veracruz)
- Federal deputy merge: 1,175 obs, 234 precincts (98% match)

## Decisions Made (do not revisit)

1. **Larreguy municipal data is primary, federal deputy data is robustness.** Rationale: brokers are municipal actors; municipal elections capture local organizational landscape; Larreguy has 3–4 elections per sección for reliable volatility; federal results attenuate findings due to nationalized trends.

2. **Volatility hypothesis is dead.** Tested with municipal volatility (3–4 elections), federal volatility (2 elections), PRI volatility, PAN volatility, margin volatility, total Pedersen index, alternation count. All null. Do not revisit.

3. **PROSPERA-as-mechanism story is dead for Paper 1.** List experiment shows the correlation is inflated by differential reporting. Separated into Paper 3 (collaboration with Castro Cornejo). Paper 1 mentions PROSPERA only as a control variable, not as a mechanism.

4. **Paper 1 framing is demand-side (community norms), NOT asymmetric information.** Earlier framing was abandoned after the Stanford evaluation. The title is "Competitive Clientelism" not "Asymmetric Information."

5. **Indigenous language finding is a supplementary result in Paper 1, not a standalone paper.** Only 78 speakers in the sample. Within-sección effect (r = 0.095) is real but fragile. Skin color is null within secciones.

6. **List experiment finding is separated from Paper 1.** Pending collaboration with Castro Cornejo. If collaboration fails, fold back into Paper 1 as supplementary section.

7. **Crime/violence paper is dropped.** Survey has only perceptual measures (no objective violence data). Would need INEGI municipal homicide data to be viable. Not worth pursuing.

8. **Rally findings folded into Paper 2, not standalone.** Only 187 attendees — too few for a separate paper.


## Style Preferences

- Be direct and honest about methodological limitations
- Do not oversell findings or use causal language for correlational results
- Use "evidence consistent with" rather than "strong evidence for" when effects are modest
- Acknowledge power limitations (5 respondents per precinct) whenever presenting within-sección nulls
- When writing for Sergio, match his analytical intensity — he pushes back on premature conclusions and expects to be treated as an active intellectual partner
