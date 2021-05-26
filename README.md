# Introduction

The goal is to make a shiny app which contains my workflow for lipidomics profiling. 
It should contain having a look at the QC and an easier way to check the identification 
of the lipids. I also hope to add some simple analysis.

In the end it should be possible do download a report and lipid list (e.g. as Excel file).

**This is still a lot of work in progress!!**

# Installation

This shiny app is build inside an R package and can be installed with `devtools::install_github("ricoderks/lipidomics")`.

# Usage

## Locally

You can use it locally from RStudio by:

    library(lipidomics)
    launchApp()

## Shiny server

To run it from a shiny server you have to make sure the package is installed and then 
you only have to create a file called `app.R` in the folder where you want to run it from.
The file `app.R` should only contain:

    lipidomics::launchApp()

