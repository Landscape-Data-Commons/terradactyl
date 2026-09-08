# terradactyl — CRAN Readiness TO-DO

Shared checklist to track CRAN submission prep for `terradactyl` (and later 
`aim.analysis`). Check items off as they're done, and add notes/comments 
inline so we don't lose context between people.

Status legend: `[ ]` not started · `[~]` in progress · `[x]` done

---

## Testing

- [x] Set up a working `testthat` scaffold (previous `tests/testthat/`
      contents were broken: stub files calling `test_that(desc, code)` with
      undefined `desc`/`code`, an empty test file, and a `helper.R`
      referencing objects that didn't exist anywhere in the package).
      *(Geovany, `testthat` branch)*
- [x] Add initial tests for a few core functions as a pattern to build from:
      `pct_cover()`, `gap_cover()`, `mean_height()`, `soil_stability()`,
      `demo_data()`. *(Geovany, `testthat` branch)*
- [ ] Brandi to share existing validation scripts used to check terradactyl
      updates, so we can convert them into formal `testthat` tests.
- [ ] Identify additional checks that aren't currently automated anywhere
      (Brandi to brainstorm from QA experience; anyone can add ideas below).
- [ ] Expand coverage to the remaining ~65 exported functions, notably:
  - [ ] The `gather_*` family (read GDBs/CSVs/text files) — will need small
        synthetic fixtures (e.g. under `inst/extdata`) or mocking, since they
        depend on file I/O.
  - [ ] Species utilities (`species_join()`, `species_count()`, etc. in
        `R/species.R`) — need to confirm exact exported function
        names/signatures before writing targeted tests.
  - [ ] `R/aim_gdb.R` functions — need bundled sample `.gdb` fixtures or
        mocking.
- [ ] Decide on a coverage target / policy (e.g. all exported functions get
      at least a smoke test) and document it somewhere (README or CONTRIBUTING).
- [ ] Set up CI to run `R CMD check` and `testthat` on push/PR (GitHub
      Actions config already exists under `.github/` — confirm it's current
      and includes test running).

## Known function bugs (found while writing tests)

- [ ] `mean_height(method = "max", tall = FALSE)` in `R/height.R` (~line 200):
      calls `tidyr::pivot_longer()` with `pivot_wider()`-style arguments
      (`names_from`/`values_from`/`values_fill`) and assumes a `Species`
      column is always present in the grouping. Currently errors for any
      call with `tall = FALSE`. Needs a real fix, not just a test.
- [x] `demo_data()` in `R/data.R` used `load(file.path("data", ...))`, a path
      that only resolves in the package source tree, not once installed.
      Fixed to use `system.file("data", ..., package = "terradactyl")`.
      *(Geovany, `testthat` branch)*
- [ ] (Add any other function bugs discovered here as we go.)

## DESCRIPTION fields

- [ ] Convert `Author:`/`Maintainer:` fields to a single `Authors@R:` field
      (CRAN policy prefers/requires this format; the list of `person()`
      calls is already there, just needs restructuring).
- [ ] Add `URL` field (GitHub repo: `Landscape-Data-Commons/terradactyl`,
      currently only referenced in the README).
- [ ] Add `BugReports` field (GitHub issues URL).
- [ ] Resolve vignette mismatch: DESCRIPTION declares
      `VignetteBuilder: knitr` and `Suggests: knitr, rmarkdown`, but there is
      no `vignettes/` directory in the package. Either add a real vignette
      (e.g. adapt `README.Rmd`) or drop the vignette-related fields/deps.
- [ ] Reconsider `raster` in `Suggests` — it's soft-deprecated in favor of
      `terra`. Confirm it's still actually needed or migrate.
- [ ] Double-check `Title` and `Description` fields read clearly to an
      outside CRAN reviewer (e.g. "TerrADat", "BLM AIM", "BLM LMF", "NRCS
      NRI" are internal acronyms that may need spelling out on first use).
      Consider adding a `<doi:...>` reference for the McCord et al. 2022
      paper cited in the README.

## Licensing

- [ ] Add a proper `LICENSE` file, or clean up the current orphaned
      `LICENSE.R` file (it's a `usethis`-style stub with placeholder `YEAR`/
      `COPYRIGHT HOLDER` text, misnamed with an `.R` extension — not a valid
      LICENSE file as-is).
- [ ] If keeping a LICENSE file, update DESCRIPTION to
      `License: GPL-3 + file LICENSE` (currently just `License: GPL-3`,
      which is valid on its own but doesn't match having a LICENSE file
      present).

## Package hygiene / size

- [ ] Remove `man/figures/GitHub - Shortcut.lnk` (a stray Windows shortcut
      file that shouldn't be tracked in the package).
- [ ] Add `.Rbuildignore` entries for: `README.Rmd`, `README.html` (~728 KB
      build artifact currently shipped in the source tree), `install.R`,
      `runtime.txt`, `terradactyl.Rproj`.
- [ ] Review overall package/data size — `data/species_list_sample.rda`
      (~765 KB) and `data/indicators.rda` (~220 KB), plus `README.html`, add
      up to a fairly large tarball for a first CRAN submission. Consider
      trimming sample datasets further if possible.

## Submission process

- [ ] Write `cran-comments.md` ahead of submission (standard CRAN practice —
      explains any NOTEs, describes what changed, etc.).
- [ ] Run `R CMD check --as-cran` locally (and/or via `rhub`/win-builder) once
      the above items are addressed, and track any remaining NOTEs/WARNINGs
      here.
- [ ] Decide on rough timeline/division of labor between Nelson, Brandi, and
      Geovany (per Nelson's email — this is a medium-long term effort, no
      fixed deadline yet).
- [ ] Apply the same process to `aim.analysis` once renamed (separate
      checklist may be worth spinning up once terradactyl is closer to done).

## Notes / open questions

- *(Add running notes, decisions, or questions here as the team works
  through the list.)*
