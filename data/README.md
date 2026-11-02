# data/

- `schema/exante_columns.csv` — the ex-ante information set (Supplementary Table S1): each column, the date it is
  recorded, and whether it entered the points scorecard (or why it was dropped). The sixteen year-end columns are
  listed with status "excluded".
- `synthetic/make_synthetic.R` — writes `work/` with an artificial `1_data_before_out.txt` and score vectors in
  the layout of `fi_01`, so that `fi_02` to `fi_10` run end to end (`SYNTH_N` sets the number of borrowers per
  year; default 20,000). The default process is invented; numbers printed on it mean nothing.
