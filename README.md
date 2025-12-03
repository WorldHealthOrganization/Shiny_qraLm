
# Overview

`Shiny_qraLm` is a **Shiny web application** designed for the
**quantitative risk assessment** of *Listeria monocytogenes* in various
food categories.

The application is available:
<https://worldhealthorg.shinyapps.io/Shiny_qraLm/>

The tool supports risk modeling for: - **Frozen Vegetables** -
**Cold-Smoked Fish** - **RTE Cantaloupe**

The application is built using the
[`qraLm`](https://worldhealthorganization.github.io/qraLm/) R package
and was developed under the scope of the [Joint FAO/WHO Expert Meeting
on Microbiological Risk
Assessment](https://www.who.int/publications/m/item/jemra-of-listeria-monocytogenes-in-foods).

📄 **Reference Document:** [FAO/WHO Summary
Report](https://www.fao.org/3/cc6993en/cc6993en.pdf)  
🔗 **GitHub Repository:**
[Shiny_qraLm](https://github.com/WorldHealthOrganization/Shiny_qraLm)

## Key Features

**Interactive Modeling** – Run risk assessment models in real-time  
**Modular Architecture** – Scalable and extensible codebase  
**Data Visualization** – Advanced `ggplot2` and `plotly` charts  
**Live Updates** – Dynamic risk level calculations  
**Downloadable Reports** – Export results in various formats  
**Centralized Metadata** – Dose-response models/populations pulled
directly from the `qraLm` package for consistency

## System Requirements

- **R 4.2 or newer** with the full `shiny` stack; macOS/Linux builds
  work out of the box, while Windows users should install Rtools for
  source packages.
- **Pandoc** is required for rendering this README and the pkgdown site
  (bundled with RStudio).
- Core dependencies: `doseresponsemodels`, `qraLm`, `ggplot2`,
  `pkgdown`, `devtools`, and `rsconnect` (optional for deployments).
  Install them via `install.packages()`/`remotes::install_github()` as
  shown below.
- When building locally, make sure you can reach the WHO GitHub
  organization because the installer pulls private registries through
  `options(myRepos = "https://worldhealthorganization.github.io/")`.
- The hosted shinyapps.io deployment caps simulation size (e.g., ≤500
  lots with 100 fish per lot, ≤500 units for vegetables/cantaloupe) to
  stay within RAM; run locally to explore larger scenarios.

## Quick Start

1.  **Install the WHO modeling dependencies** (not yet on CRAN).

``` r
# install.packages("remotes")
remotes::install_github("WorldHealthOrganization/doseresponsemodels")
remotes::install_github("WorldHealthOrganization/qraLm")
```

2.  **Install ShinyqraLm and launch the packaged app.** The installer
    pulls the rest of the CRAN dependencies automatically.

``` r
remotes::install_github("WorldHealthOrganization/Shiny_qraLm")
ShinyqraLm::run_Shiny_qraLm()
```

3.  **Explore in the browser.** The helper is just a thin wrapper around
    `shiny::runApp()` so any additional arguments (e.g., `host`, `port`,
    `launch.browser = FALSE`) can be forwarded through
    `run_Shiny_qraLm(...)`.

If you work from a git checkout, use the bundled directory directly:

``` r
# inside the repository root
shiny::runApp("inst/app")
```

That keeps your editing workflow identical to a regular Shiny project
while ensuring the same files are shipped inside the package.

## Development Workflow

1.  **Clone and install dev dependencies**

``` sh
git clone https://github.com/WorldHealthOrganization/Shiny_qraLm.git
cd Shiny_qraLm
```

``` r
# Pick your preferred tooling; devtools is used here.
install.packages(c("devtools", "pkgdown"))
devtools::install_dev_deps()  # optional but convenient
```

2.  **Iterate on the app**

``` r
devtools::load_all()
shiny::runApp("inst/app")
```

3.  **Regenerate metadata as needed**

``` r
devtools::document()
pkgdown::build_site()
```

4.  **Check before shipping**

``` r
devtools::check()
```

The `inst/app` pattern means deployments to
[shinyapps.io](https://worldhealthorg.shinyapps.io/Shiny_qraLm/) and
package installations share the exact same UI, server, data, and static
assets. Simply re-installing the package or redeploying from this
repository updates both delivery paths.

## Data & Metadata Sources

All modeling datasets live under `inst/app/data/`. The Excel workbooks
(`FV_parametersbaseline.xlsx`, `RTEFish_parametersbaseline.xlsx`,
`Cantaloupe_parametersbaseline.xlsx`) contain parameter tables that are
ingested through `data/data.R` and exposed via
`load_dose_response_metadata()`. The `sampledata.csv` file is a
lightweight example for manual uploads, while `sysdata.rda` holds
pre-processed objects for offline fallbacks.

When adding new commodities or scenarios:

- Place spreadsheets or CSVs inside `inst/app/data/` and document
  provenance in the file header.
- Update `data/data.R` so the loader keeps returning a complete metadata
  list; this powers dropdowns across modules.
- Regenerate `sysdata.rda` (if needed) with the same structure so
  packaged deployments remain self-contained.

Large or sensitive datasets should remain external—consider building
lightweight summary tables to preserve privacy.

# Architecture Overview

The application follows a modular architecture, bundled under `inst/app`
so it is shipped with the package.

``` bash
Shiny_qraLm/
├── inst/
│   └── app/
│       ├── app.R                 # Main application entry point
│       ├── data/                 # Datasets & metadata helpers
│       │   ├── Cantaloupe_parametersbaseline.xlsx
│       │   ├── data.R            # load_dose_response_metadata()
│       │   ├── FV_parametersbaseline.xlsx
│       │   ├── RTEFish_parametersbaseline.xlsx
│       │   ├── sampledata.csv
│       │   └── sysdata.rda       # Preprocessed datasets (fallback)
│       ├── modules/              # Reusable UI/server components
│       ├── pages/                # Individual app pages
│       └── www/                  # Static files (CSS, images, JS)
├── R/
│   └── run_Shiny_qraLm.R         # Helper that launches inst/app
├── LICENSE               # License information
├── README.Rmd            # README file (this file)
├── Shiny_qraLm.Rproj      # RStudio project file
└── DESCRIPTION           # Package metadata
```

## Key Components

- **`inst/app/app.R`**: Main entry point that initializes the UI and
  server logic, sources all modules/pages, and registers dose-response
  metadata.
- **`inst/app/data/`**: Contains datasets and the
  `load_dose_response_metadata()` helper that synchronizes the UI with
  the `qraLm` package.
- **`inst/app/modules/`**: Stores reusable Shiny UI/server components
  with shared authorship by Vasco Cadavez and Ursula Gonzales-Barron.
- **`inst/app/pages/`**: Organizes the UI and server logic for each
  section of the app.
- **`inst/app/www/`**: Holds static assets like CSS, JavaScript, and
  images.
- **`R/run_Shiny_qraLm.R`**: Public function exposed by the package to
  launch the bundled app.

## Deployment

Use `rsconnect` for shinyapps.io/Posit Connect or `shiny::runApp()` for
on-prem hosting. Typical workflow:

``` r
install.packages("rsconnect")
rsconnect::setAccountInfo(name = "worldhealthorg", token = "<token>", secret = "<secret>")
rsconnect::deployApp("inst/app", appName = "Shiny_qraLm")
```

Deployment tips: - Keep `inst/app` free of user-specific files; static
assets go under `inst/app/www`. - Ensure the WHO GitHub mirror remains
accessible during deployment because `app.R` sets a custom repo via
`options(myRepos = "https://worldhealthorganization.github.io/")`. -
Always run `devtools::check()` plus an end-to-end smoke test
(`shiny::runApp("inst/app")`) before publishing so shinyapps.io picks up
the latest metadata.

## FAQ

### How do I run the app locally?

Clone the repository:

``` sh
git clone https://github.com/WorldHealthOrganization/Shiny_qraLm.git
```

Install required packages in R (see [Quick Start](#quick-start)). Start
the app:

``` r
ShinyqraLm::run_Shiny_qraLm()
```

Open your browser and go to `http://localhost:8000/`.

### What kind of models are available?

- [Frozen Vegetables](https://doi.org/10.3390/foods13223610)
- [Cold-Smoked Fish](https://doi.org/10.3390/foods13233831)
- [RTE Cantaloupe](https://doi.org/10.3390/foods14132212)

## Contributing

Contributions are welcome! Please open an issue first for substantial
changes so we can discuss scope and data implications. Typical workflow:

1.  Fork and clone the repository.
2.  Work off a feature branch.
3.  Run `devtools::check()` and `pkgdown::build_site()` (if you touched
    docs) before submitting a pull request.

For detailed coding conventions, release expectations, and pull-request
templates, read [AGENTS.md](AGENTS.md). The guide covers module
structure, naming, testing, and review checklists tailored to this
repository.

Community members are especially encouraged to propose new risk
scenarios, UI improvements, and documentation clarifications. When
contributing large datasets, include their provenance and licensing
details.

## License

This project is licensed under the MIT License. See the `LICENSE.md`
file for full details.

## Acknowledgments

Special thanks to the FAO/WHO Expert Group for their valuable
contributions to this project.
