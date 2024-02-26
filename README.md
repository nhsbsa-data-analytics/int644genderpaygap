# NHSBSA Gender Pay Gap report

This R package has been developed by NHS Business Services Authority Data Analytics Learning Lab to use as a template for building NHSBSA branded R `{shiny}` dashboards. 

## Features

We have used the `{golem}` framework to develop this taking inspiration from:

* [Example `golem` apps](https://github.com/ThinkR-open/golem)
* [Clinical Development Unit Data Science Team dashboards](https://github.com/CDU-data-science-team)

There are also github actions to check that code conforms to `lintr` (https://lintr.r-lib.org/), passes `R CMD check` and to run a `gitleaks` check.

## Structure

The package is structured as below. See the "Using this template" section for further details of the files.

```
nhsbsaGPGR
├── .git                                    # Git folder, should never need to even look in here!
├── .github                                 # Workflows for github actions - lintr and R CMD check
├── .gitignore                              # Ensure that you ignore the data folder once template is reused
├── .Rbuildignore                           # Like a .gitignore, add any files to ignore when building the package
├── app.R                                   # Golem file
├── data                                    # Package data (accessible via nhsbsaShinyR::{name})
├── data-raw                                # Example script to produce a package data file
├── DESCRIPTION                             # Metadata of package
├── dev                                     # Golem files
│   ├── 01_start.R                          # Golem file (use to set up golem framework)
│   ├── 02_dev.R                            # Golem file (use to develop package)
│   ├── 03_deploy.R                         # Golem file (use to deploy package)
│   └── run_dev.R                           # Golem file (use to test development of package)
├── gitleaks.toml                           # Config for gitleaks, only needed in public repos (see DALL wiki)
├── inst                                    # Installed files...
│   ├── app                                 # For the app...
│   │   └── www                             # Made available at runtime
│   │       └── assets                      # Static files
|   |           ├── favicons                # NHS favicons
|   |           ├── icons                   # NHS front-end toolkit icons
|   |           ├── logos                   # NHS logos
|   |           └── markdown                # Markdown documents
│   │       ├── css                         # NHS front-end toolkit and custom CSS
│   │       └── js                          # NHS front-end toolkit and custom JavaScript
│   └── golem-config.yml                    # Golem file
├── LICENSE.md                              # Apache
├── man                                     # Package documentation, created automatically by roxygen2 from your initial comment blocks
├── NAMESPACE                               # Automatically generated documentation by roxygen2
├── nhsbsaGPG.Rproj                      # R Project file
├── R                                       # R code for the dashboard
│   ├── _disable_autoload.R                 # Golem file (no modification)
│   ├── app_config.R                        # Golem file
│   ├── app_server.R                        # Server component
│   ├── app_ui.R                            # UI component
│   ├── golem_utils_ui.R                    # Useful utility functions for UI
│   ├── mod_*.R                             # Example Shiny modules
|   ├── nhs_*.R                             # NHS styled UI components
|   ├── nhsbsaGPG.R                      # Package documentation file
│   ├── run_app.R                           # Golem file (no modification)
│   ├── utils_accessibility.R               # Custom NHSBSA highcharter theme
│   └── utils-pipe.R                        # Magrittr %>% operator
└── README.md                               # Brief overview of the package

```

