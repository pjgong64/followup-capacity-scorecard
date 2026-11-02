# Publishing this repository and minting a Zenodo DOI

1. **Zenodo first, once:** sign in at zenodo.org with GitHub → *Account → GitHub* → switch the toggle **on** for this
   repository (it appears after step 2; refresh the page).
2. **GitHub:** create an empty public repository `followup-capacity-scorecard` under `pjgong64` (no README, no
   licence — both are here). Then, from this folder:
   ```
   git init && git add . && git commit -m "Analysis code, v1.0.0"
   git branch -M main
   git remote add origin https://github.com/pjgong64/followup-capacity-scorecard.git
   git push -u origin main
   ```
   (GitHub Desktop works too: *File → Add local repository* → *Publish repository*.)
3. **Release:** on GitHub, *Releases → Draft a new release* → tag `v1.0.0` → *Publish release*. Zenodo archives
   the release within a few minutes and uses `.zenodo.json` for the record.
4. **DOI:** the Zenodo record shows two DOIs. Cite the **concept DOI** ("Cite all versions") in the paper; it
   always resolves to the newest version. Paste it into the manuscript's *Availability of data and materials* and
   into `CITATION.cff` (`doi:`), then push again (no new release needed).
5. **Before submitting to a double-blind venue** (e.g. IC2IT): do not cite this repository; write "code will be
   released upon acceptance" instead, because the owner name identifies the authors.
