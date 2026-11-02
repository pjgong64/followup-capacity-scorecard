# Whom to visit when officers are few — analysis code
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.23888415.svg)](https://doi.org/10.5281/zenodo.23888415)
Analysis code for a study of credit follow-up under a fixed field-staff capacity, on a nationwide panel of
1,496,721 Thai rice-cultivating borrowers of the Bank for Agriculture and Agricultural Cooperatives (BAAC),
4,490,163 borrower-years (2020–2022), with the 2022 season withheld from every fitted or selected quantity.

**Paper:** Jungsatidkul, P.; Innet, S. *Whom to Visit When Officers Are Few: Follow-Up Capacity, Exposure and the
Price of Transparency in Agricultural Credit.* Manuscript in preparation, 2026.

**Companion code (Study 1, the forecasters this study builds on):**
[pjgong64/stcraan-forecasting](https://github.com/pjgong64/stcraan-forecasting) ·
[10.5281/zenodo.22842038](https://doi.org/10.5281/zenodo.22842038) ·
paper [10.3390/forecast8060100](https://doi.org/10.3390/forecast8060100)

## What is here

The borrower records are restricted and are not in this repository.

```
R/            fi_00 ... fi_10, one script per analysis step (run order below)
bat/          the Windows launchers used to run each step
data/
  schema/     the ex-ante information set: every column, the date it is recorded, its fate in the scorecard
  synthetic/  make_synthetic.R -- a fully artificial panel that lets fi_02 to fi_10 run end to end
output/
  results/    every CSV / Markdown table the manuscript reads from (aggregates only, no borrower rows)
  logs/       the run logs of the published run
  figures/    Figure 1
```

## Run order

| Step | Script | Does | Uses 2022 outcomes? |
|---|---|---|---|
| 00 | `fi_00_inventory.R` | read-only checks: score vectors reproduce, money join, tiers, March-due proxy | describes only |
| 01 | `fi_01_run_exante.R`, `fi_01_run_exante_rolling.R` | refit LR and XGB without the 16 year-end covariates (rolling 2020→2021, main 2020–21→2022) | no |
| 02 | `fi_02_pd_correction.R` | two-stage probability correction (held-out intercept shift, then EM prior on unlabelled scores) | no |
| 03 | `fi_03_capacity_frontier.R` | ranking rules × capacity; rule selected on 2021; 2022 frontier; ADM3 Poisson bootstrap | evaluation only |
| 04 | `fi_04_sensitivity.R` | loan-size groups, largest loans, LGD × MRR, cost of a visit, feasible capacity | evaluation only |
| 05 | `fi_05_mixed_policy.R` | mixed policy (money track + small-loan PD track); share chosen on 2021 | evaluation only |
| 06 | `fi_06_scorecard.R` | WOE points scorecard, reason codes; built on development years | no |
| 08 | `fi_08_transparency.R` | XGB vs LR vs scorecard vs bank grade, in baht, with paired bootstrap | evaluation only |
| 09 | `fi_09_psi.R` | score PSI, characteristic CSI, reason drift — no outcomes | no |
| 10 | `fi_10_deployment.R`, `fi_10_figure1.R` | gains table, odds alignment, label-free realignment, strategy table set on 2021, Figure 1 | evaluation only |

Step 01 is a copy of the Study 1 factorial runner with one addition (`EXANTE_EXCLUDE`); it sources
`12_Factorial7_config.R`, `12_Factorial7_partition.R` and `12_Factorial7_report.R` from the companion repository and
reads the restricted modelling file, so it cannot be run on the synthetic panel.

## Quick start on synthetic data

```
git clone https://github.com/<owner>/<this-repo> && cd <this-repo>
cd data/synthetic && Rscript make_synthetic.R && cd ../..      # writes work/ (~10 s)
cd work
for s in fi_02_pd_correction fi_03_capacity_frontier fi_04_sensitivity fi_05_mixed_policy \
         fi_06_scorecard fi_08_transparency fi_09_psi fi_10_deployment fi_10_figure1; do
  Rscript ../R/$s.R
done
```

Each script reads and writes relative to the working directory (`1_data_before_out.txt`, `factorial7_out/`,
`fi_out/`), exactly as in the published run. On the synthetic panel every number is meaningless — the default
process is invented — and `fi_10_deployment.R` still uses the two priors of the published run as constants
(`DEV_PRIOR`, `EM_PRIOR_2022`, from `fi06_log.txt`). Requirements: R ≥ 4.3 with `data.table`
(steps 01–02 also need the packages of the companion repository).

## Traceability

Every number in the manuscript comes from a file in `output/`.

| Claim | Value | Produced by | Published in |
|---|---|---|---|
| Cost of deciding on 1 May (XGB / LR) | AUC 0.9077 → 0.8970 / 0.8943 → 0.8885 | `fi_01_*` | `logs/` of the companion run; `fi02_summary.csv` |
| Corrected PD on 2022 | XGB 12.21%, LR 13.80% (realised 12.06%) | `fi_02` | `fi02_summary.csv` |
| Money-weighted rule selected on 2021 | XGB R3_EL, LR R2_EXP | `fi_03` | `fi03_selection.csv` |
| Model list vs bank grade at K = 19% | EAR 11.34 vs 32.54 bn; Δ [−22.35, −20.15] | `fi_03` | `fi03_frontier.csv`, `fi03_contrasts.csv` |
| Money-weighted vs PD-ranked at K = 19% | Δ EAR [−5.13, −4.45] bn; Δ recall [−0.186, −0.176] | `fi_03` | `fi03_contrasts.csv` |
| Marginal exposure per follow-up | ~48,000 / 24,000 / 13,000 THB at 20 / 30 / 40% | `fi_03` | `fi03_frontier.csv` (`marg_exp`) |
| Robustness to the largest loans | gap 17.8–20.8 bn | `fi_04` | `fi04_large_loans.csv` |
| Break-even save rate at 500 THB | 0.4 / 1.0 / 2.1% at 10 / 20 / 30% | `fi_04` | `fi04_breakeven.csv` |
| τ* volumes by LGD | 29–41% / 17–25% / 12–17% | `fi_04` | `fi04_lgd_mrr.csv` |
| Loan-size quintiles at 20% | Q1 recall 1.0% (money) vs 84.7% (bank) | `fi_04` | `fi04_size_groups.csv` |
| Mixed policy, s = 0.5 chosen on 2021, vs bank grade | EAR [−13.68, −11.95] bn; small reached [+7,641, +8,503] | `fi_05` | `fi05_recommended.csv`, `fi05_mixed.csv` |
| Scorecard: 11 characteristics, AUC | 0.8788 (LR 0.8885, XGB 0.8970) | `fi_06` | `fi06_log.txt`, `fi06_scorecard_exante_main.csv` |
| Price of transparency at 19% | SC − XGB [+0.80, +1.11] bn; SC − LR [−0.12, +0.16] | `fi_08` | `fi08_contrasts.csv` |
| Score PSI 2021 → 2022 | 0.0022 | `fi_09` | `fi09_psi.csv` |
| Gains table, realigned odds (Δ = 20.9 points) | 7.34% → 11.10% (realised 12.06%) | `fi_10_deployment` | `fi10_gains.csv`, `fi10_realign.csv` |
| Strategy table set on 2021, evaluated on 2022 | K = 19%: recall 0.691, small 0.827, EAR 20.97 bn | `fi_10_deployment` | `fi10_strategy_table.csv`, `fi10_strategy_eval.csv` |
| Figure 1 | capacity frontier | `fi_10_figure1` | `figures/fi10_figure1.*` |

## What was withheld

Row-level files — score vectors, corrected PDs, per-borrower reasons (`*.rds`) — and the borrower-year input files.
`output/` contains aggregates only: rates, sums and counts over bins, bands, quintiles, capacities and bootstrap
replicates.

## Licence, funding and competing interests

MIT — see `LICENSE`. The licence covers the code only and confers no rights over any BAAC data.
P.J. is a BAAC employee supported by a BAAC doctoral scholarship; BAAC owns the de-identified dataset. S.I. has no
BAAC affiliation. Generative AI (Claude, Anthropic) was used to draft parts of the code; all code was reviewed,
modified and validated by the authors before execution.

## Citation

See `CITATION.cff`.
