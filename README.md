
<!-- README.md is generated from README.Rmd. Please edit that file -->

# waterlogged

<!-- badges: start -->
<!-- badges: end -->

## WELCOME TO waterlogged!

We are embarking on an exciting journey of collaborative coding in
github and this will be fun and also terrifying and probably also a bit
kooky so lets just try it and see what happens, shall we?

Right now, lets use this README to keep everyone on track with getting
started here and assigning tasks. Eventually this README will be
replaced by a (functioning) vignette to guide users on how to use the
package.

## STEP 1: Get all collaborators integrated with git, RStudio, and this project

- Make sure you have git installed on your local computer and you have
  integrated it in R Studio. Follow these instructions:
  <https://happygitwithr.com/>
- Next, clone this project onto your local computers for collaboration.
  Follow these instructions, starting at Chapter 4 Using GitHub via
  RStudio, specifically 4.3 Cloning a Repository
  <https://pages.github.nceas.ucsb.edu/lter/github-workshop/using-github-via-rstudio.html>
- Test it! Edit README.Rmd by replacing text indicated below. In git
  window, stage README.Rmd file, commit, and then push to github.
- After this, whenever you start working on this project, make sure you
  press PULL in the git window first to pull any changes from
  collaborators (us) from github before you start working on scripts.

JULIAN GITHUB-R TEST: REPLACE TEXT HERE WITH WELCOME OF CHOICE

AMANDA GITHUB-R TEST: Here we gooooo! \[Nice work! - DL\]

## Step 2: Add branches to update functions

- I’ve uploaded my most recent versions of functions into the R folder
  of this package. In doing so, I fixed a few errors in the
  documentation (header) section of the functions, as well as separated
  the scripts with multiple functions into separate scripts for each
  functions. Take a look at these, and see if you have any other
  additions to the scripts that you need to add.
- Read through Ch 7 Appendix B Branches:
  <https://pages.github.nceas.ucsb.edu/lter/github-workshop/appendix-b-branches.html>
- Before working on anything, make sure you have pulled any changed from
  github onto your local computer to be up to date.
- in git window, create new branch and name it as “updates to \_\_\_\_
  function” or another informative name
- Edit the functions as needed. Then commit and push branch to github.
- go to the github repo page (in your browser), and click “compare and
  pull request”, enter info, and then hit “create pull request”. This
  notifies me that you’ve made changes and are ready for them to be
  integrated.
- if everything looks good, I will integrate your edits (your branch)
  ie. “merge pull request”. Then I will close your branch (delete it).
- Pull from RStudio to get the latest repo changes from github

## Step 3: update vignette

Daniella:

- make dummy data for: metadata, dissox, waterlevel, baro
- update vignette with updated functions using dummy data

Julian:

- upload dummy conductivity data
- in vignette, start a new BRANCH for conductivity data processing: add
  section to process conductivity data using dummy data

Amanda:

- upload dummy manual discharge data
- in vignette, start a new BRANCH for discharge data processing: add
  section in vignette to process dishcarge data using dummy data

## IGNORE (Placeholder): Installation

You can install the development version of waterlogged from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("daniella-lo/waterlogged")
```
