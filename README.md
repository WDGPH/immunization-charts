# Immunization Charts

## Introduction
This project provides an approach to creation of custom immunization history charts. These charts can be generated as part of notice letters for overdue Immunization of School Pupils Act (ISPA)-mandated or Child Care and Early Years Act (CCEYA)-mandated immunizations.

## Usage
### Environment
[R](https://www.r-project.org/) is used with [LaTeX](https://www.latex-project.org/) (via [rmarkdown](https://pkgs.rstudio.com/rmarkdown/index.html)) for PDF generation. [renv](https://rstudio.github.io/renv/index.html) is used in this repository to assist with accurately reproducing the R project environment.

### Data
This project is intended to be used with data extracts from [Panorama PEAR](https://accessonehealth.ca/).

Input files, in `xlsx` format should be organized in a subfolder `input`. Each `xlsx` file should have a shared format. It's suggested that `xlsx` exports from Panorama PEAR are batched by client birth year. The report must at minimum include "Client ID", "Date of Birth", and a string representation of the immunization history, in a column "Received Agents". To create this immunization history string, a "Repeater" Data Container must be used in the Panorama PEAR report builder. The repeater will be formatted as:
1. `[PresentationView].[Immunization Received].[Date Administered]`
2. Text box with space, dash, and a space (` - `)
3. `[PresentationView].[Immunization Received].[Immunizing Agent]`

### Functionality
`make_charts.R` contains data processing steps, with some functions relating specifically to formatting information for use in LaTeX code separated out into `latex_utilities.R`. Based on your particular report, adjust the `col_types` and `select`ed columns for your particular data file(s) in `make_charts.R`.

`chart_template.Rmd` allows for generation of PDF files using LaTeX, by inserting processed data elements into LaTeX code. This LaTeX code can be customized and expanded upon such that the immunization chart is an element in a larger letter with:
- Addressee information that can be shown in the window of an envelope
- Public Health Unit branding
- List of overdue diseases
- Public Health Unit-specific instructions for updating vaccination records or consultation on vaccination
- Any other customizations that can enhance client experience or streamline vaccine record management operations

This project currently supports charts that include any combination of CCEYA and ISPA-mandated vaccinations, in addition to HPV and Hepatitis B recommended vaccines.

These diseases can be re-ordered to suit your application, through modifications in the parameters section in `make_charts.R`. Any diseases left off generated charts will be collapsed into the 'Other' diseases column.

An `output` subfolder should also be created for generated PDFs, and a leger of vaccines detected in the `Received Agents` in your data file(s).

## Contributing
Fixes or additions to the `vaccine_references.xlsx`, dependency updates, documentation improvements, and additions of tests will enhance the usability and reliability of this project and are welcome contributions.