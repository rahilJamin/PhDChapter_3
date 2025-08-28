# AGENTS.md — Project guidance for coding agents

## Purpose
Assist with an R project that behaves like a well-organised, reproducible **package-style** repo (without needing CRAN), while keeping the human in full control of Git operations.

---

## Style & Naming (must follow)
- **File/folder names**: lower case, words separated by underscores (e.g., `data_cleaning.R`, `plot_outputs/`).
- **R objects**: snake_case for functions/objects (e.g., `fit_model()`); S3/S4 classes may use standard R conventions if present.
- **Language**: use **British English** in comments, documentation, messages, and README (e.g., *colour*, *organise*).
- **Formatting**: prefer tidyverse style; keep lines readable; factor out duplicated code into functions.

---

## Repository shape (package-like)
Recommended layout (create if missing; do not break existing workflows):
- `R/`               — reusable functions (document with **roxygen2**)
- `scripts/`         — entry-point scripts (ETL, analysis, figures)
- `tests/`           — unit tests (**testthat**)
- `data/`            — small example data safe to commit
- `data-raw/`        — large/intermediate data (**gitignored**)
- `man/`             — generated documentation from roxygen2 (if using pkg structure)
- `inst/`            — auxiliary files (e.g., templates), if helpful
- `README.md`        — how to run/install
- `renv/` + `renv.lock` — dependency lock for reproducibility
- `.github/workflows/` — CI (tests/lint) if enabled later

If a minimal package skeleton improves reproducibility, propose adding `DESCRIPTION` and `NAMESPACE`, but **ask before doing it**.

---

## R toolchain (must ensure, prefer `{pak}`)
**Always verify these are installed/available:**
- `{pak}` (preferred for installs)
- `{renv}` (project environment)
- `{roxygen2}` (docs)  *[“oxygen” in conversation refers to **roxygen2**]*
- `{import}` (lightweight import helper)
- `{devtools}` (build/test helpers)

**Bootstrap script behaviour (idempotent):**
1. If `{pak}` missing, install via base R:  
   `install.packages("pak", repos = "https://cloud.r-project.org")`
2. Use `{pak}` to ensure the rest exist:  
   `pak::pkg_install(c("renv","roxygen2","import","devtools","testthat","lintr","styler","here"))`
3. If `renv` is initialised, respect `renv::status()` and **do not** alter `renv.lock` without explicit instruction.  
4. When adding packages to the project, use `{pak}` or `renv::install()` and propose updating `renv.lock` — **ask first**.

Provide or update:
- `scripts/install_deps.R`: installs required packages (using `{pak}`) and initialises `renv` if requested.
- `scripts/dev_docs.R`: runs `roxygen2::roxygenise()` to (re)generate docs.
- `scripts/run_all.R`: orchestrates typical workflow (import, clean, model, plot).

---

## Documentation & comments
- Use **roxygen2** for functions in `R/`.  
- Comments and README in **British English**; be concise and helpful.  
- Prefer `here::here()` for paths; avoid absolute Windows paths.

---

## Git rules (safety first)
- **Do not commit** changes yourself. Stage/commit is a **human-only** action.
- **Always ask for explicit approval** before any operation that would push to a remote (e.g., open a message: “Ready to push branch `feature/x`? [y/N]”).
- Work on a feature branch by default (e.g., `codex/<short-purpose>`), but **create only after user approval**.
- Provide a clear **diff summary** and commit message suggestion; leave the actual commit/push to the user (or request confirmation explicitly).

---

## Data handling
- Never commit large or sensitive data. Use `data-raw/` (gitignored) for intermediates.
- If a dataset download is needed, write a script in `scripts/` and document provenance in README.

---

## Tasks you may do (with approval points)
1. **Scaffold** missing structure (folders, `.gitignore`, `README.md`, `scripts/install_deps.R`) — *ask first*.  
2. **Refactor** a script into functions under `R/` with roxygen2 docs — *show diff, ask to proceed*.  
3. **Add tests** in `tests/` using **testthat** for key functions — *ask first*.  
4. **Set up `renv`** (if not present): propose initialisation and lockfile creation — *ask first*.  
5. **Create VS Code tasks** (optional) to run common R commands with `Rscript` — *ask first*.  
6. **Propose CI** via GitHub Actions to run tests/lintr — *ask first*; do **not** push workflows without approval.

---

## Non-goals / boundaries
- Do **not** change remote settings, tokens, or branch protection.
- Do **not** run system-level commands that could affect the OS without explicit consent.
- Do **not** rename files without explaining the change; when renaming, use lower case with underscores as per policy.

---

## Quick checks the agent should run before any work
- In repo root? (contains `.git`)
- Print planned actions and ask: “Proceed? (y/N)”
- Confirm `{pak}`, `{renv}`, `{roxygen2}`, `{import}`, `{devtools}` presence; if missing, propose to run `scripts/install_deps.R`
- If `renv` present, honour it (`renv::activate()` within scripts as needed).

---

## Handy commands (for humans)
- Install deps: `Rscript scripts/install_deps.R`
- Generate docs: `Rscript -e "roxygen2::roxygenise()"`
- Run tests: `Rscript -e "testthat::test_dir('tests')"`
- Style code: `Rscript -e "styler::style_dir()"`
- Lint: `Rscript -e "lintr::lint_dir()"`

