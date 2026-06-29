# FIMS case studies

## Goal
If you want to run a FIMS model and don't know where to start, you can submit your model to this test-bed 🧪🛏️ repository and we can work with you on getting your model running in [Fisheries Integrated Modeling System](https://NOAA-FIMS/FIMS/)! The associated [GitHub pages site](https://noaa-fims.github.io/case-studies/) with this repo "shows the work" of getting each model running in FIMS. Once the model is completed and running, it will graduate :mortar_board: to it's own repository. In the future, we will have a way to track assessments that use FIMS, so be on the lookout for that!

## Case studies included so far

<!-- TABLE_START -->

Stock | Previous Model | Status | Notable Features |
-- | -- | -- | --
NEFSC yellowtail flounder | ASAP | working |
AFSC GOA pollock | Cole's TMB model | non-updated | Bayesian |
SWFSC sardine | SS3 | not updated |
NWFSC petrale | SS3 | not updated |
PIFSC opakapaka | SS3 | not updated | Age-to-length conversion matrix |
SEFSC scamp | BAM | working |
NWFSC Pacific hake | SS3 | failing | Bayesian |

<!-- TABLE_END -->

## How to add a case study

* Create a new branch to work on a case study.
* Edit the `.qmd` or `.md` files in the `content` folder. `.qmd` files can include code (R) and lots of Quarto markdown bells and whistles (like call-outs, cross-references, auto-citations and much more).
* Add the files to `_quarto.yml`.
* Submit a pull request when finished working on a case study. If the case study renders successfully, the rendered pages will be uploaded to the artifacts section of the GitHub Actions page. If the case study fails to render, you can review the GitHub Actions log to debug.
* FIMS team members will review the pull request and make comments, suggestions, etc. about how to get your model running.

## Using codespaces to develop case studies

See the [GitHub codespaces documentation](https://docs.github.com/en/codespaces) for general codespaces help.

After launching a codespace, to code in Rstudio rather than VSCode, type `rserver` into the command line.

If needed for use, `reshape2` will need to be install on the R terminal using `install.packages("reshape2")`. It was failing to install as part of the codespaces `devcontainer.json` file, and so was not included for now.

`gdb` is installed in the codespace.

<hr>

### Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project content is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

### License

This content was created by U.S. Government employees as part of their official duties. This content is not subject to copyright in the United States (17 U.S.C. §105) and is in the public domain within the United States of America. Additionally, copyright is waived worldwide through the CC0 1.0 Universal public domain dedication.

