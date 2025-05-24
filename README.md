# FIMS case studies

This is a [website](https://noaa-fims.github.io/case-studies/) (`type: website`) showcasing test cases of the [Fisheries Integrated Modeling System](https://NOAA-FIMS/FIMS/).

Case studies included so far:
Stock | Status
-- | --
NEFSC yellowtail flounder | working
AFSC GOA pollock | working
AFSC BSAI Atka Mackerel | working
SWFSC sardine | working
NWFSC petrale | working
PIFSC opakapaka | working
SEFSC scamp | working

## How to add a case study

* Create a new branch to work on a case study.
* Edit the qmd or md files in the `content` folder. qmd files can include code (R) and lots of Quarto markdown bells and whistles (like call-outs, cross-references, auto-citations and much more).
* Add the files to `_quarto.yml`.
* Submit a pull request when finished working on a case study. If the case study renders successfully, the rendered pages will be uploaded to the artifacts section of the GitHub Actions page. If the case study fails to render, developers can review the GitHub Actions log to debug.

<hr>

## Main and dev branches

- The `main` branch of case-studies should always work with the `main` branch of FIMS. It should always be passing GitHub Actions.
- The `dev` branch of case-studies should work with the `dev` branch of FIMS. It may sometimes be broken.

## Working off of main

It may be necessary to work off of main in order to apply a "hot fix" to case-studies between FIMS releases. Create a new branch off of main that includes the word "main" somewhere in its name - the Quarto setup file will automatically install the main version of FIMS as long as the word main is somewhere in the branch name. Otherwise, the dev version of FIMS will be installed.

## Working off of dev

Create a branch off of dev that does NOT include the word "main" somwhere in its name.

## How to use `renv`  

[renv](https://rstudio.github.io/renv/) helps manage R packages for a project. We have set it up for this repository so that everyone can ensure they have all of the necessary packages installed on their computer to reproduce the case studies. When starting a working session run `renv::restore()` to automatically download and install all necessary packages. As you make changes periodically check `renv::status()` to see if any new packages need to be added to the lock file (`renv.lock`). If your project is out of sync, run `renv::snapshot()` to update the lock file. Once finished working for the day, be sure to push your changes to the lock file to the repository. 

## How to use codespaces 

See the [GitHub codespaces documentation](https://docs.github.com/en/codespaces) for general codespaces help.

After launching a codespace, to code in Rstudio rather than VSCode, type `rserver` into the command line.

If needed for use, reshape2 will need to be install on the R terminal using `install.packages("reshape2")`. It was failing to install as part of the codespaces devcontainer.json file, and so was not included for now.

gdb is installed in the codespace.

### Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project content is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

### License

This content was created by U.S. Government employees as part of their official duties. This content is not subject to copyright in the United States (17 U.S.C. §105) and is in the public domain within the United States of America. Additionally, copyright is waived worldwide through the CC0 1.0 Universal public domain dedication.

