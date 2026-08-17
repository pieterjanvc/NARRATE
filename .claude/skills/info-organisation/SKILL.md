---
name: info-organisation
description: Overview of what this project is about, organisation of the repo, and best practices. To be used whenever a new feature is being added, or changes are made to existing core files in the R/ or inst/ folders.
---

# Important terminology

Always notify the user in case the terminology is not used consistently or there
is ambiguity.

- Evaluation: Relates to the original data of student evaluations by faculty
  during clerkship rotations. Any variable containing the word `eval` refers to
  this as well.
- Review: Related to the AI or human review and scoring of the provided
  evaluations to assess their quality. Any variable containing the word `review`
  refers to this as well.
- Competency: Outcomes for medical students that are evaluated during their
  clerkship rotations. They form the basic of the rubric scoring system.

# Overarching aims

This project is hosting R Shiny apps and custom functions that will help with
reviewing the quality of narrative evaluations of medical student clerkships by
faculty (i.e. the reviewers). The main aim it to score the quality of each
evaluation based on a rubric. The review process is done by both humans (via the
apps) and AI.

# Repo organisation

The repo is organised as an R package.

## Folders

Use this to know where to look for files, and avoid loading unnecessary ones

- `./deploy`: Auto generated files to create standalone apps for publication to
  a Posit connect server. Can be ignored unless there is a specific question
  about a file in here.
- `./dev`: Not part of the R package, contains files used for local development
  and testing
- `./inst`: Files and R scripts that are installed alongside the R package and
  are needed by its functions. The most important ones are listed below:
  - `./inst/narrate.sql`: The schema of the SQLite database where all app data
    is saved to
  - `./inst/review_app.R`: The main R Shiny app that will be published and used
    to review evaluations.
  - `./inst/prompt_comp_extract` and `./inst/prompt_comp_score`: AI review
    prompt templates (contains `{}` placeholders for the `glue` library to fill
    in during runtime)
- `./local`: ConContains files not tracked by github like SQLite databases used
  for dev / testing
- `./man`: R manuals for the package functions. Most functions have roxygen2
  docstrings so it might be better to use those as a reference instead.
- `./R`: The main folder of this project. It contains all R functions needed to
  run the apps and process the data. All functions should have roxygen2
  docstrings
- `./renv`: R environment folder with libraries

# Review process overview

## Setup

Before getting started a one-time setup of the database and loading in data is
needed.

- A new SQLite database is created using `./inst/narrate.sql`
- The raw evaluation data is parsed from an .xlsx file (no need to look at this
  file) and inserted into the database
- Other data like the initial prompts and competencies might also be inserted
  into the database
- Relevant environment variables are set (mostly API keys or other passwords)

NOTE: the `sqlife` package is a custom library to work with SQLite in R, and if
there are issues with any of its functions, look for details in the GitHub repo
at https://github.com/pieterjanvc/sqlife

## Human review

Human reviewers use the `./inst/review_app.R` to review evaluations. They have
been assigned one or more evaluations and go through the following process for
each:

1. Detect and score competencies: Using the evaluation text, reviewers look for
   any of the possible competencies and if present, score them using the
   specificity score. They also highlight and save any pieces of text that serve
   as supporting evidence. There usually are multiple competencies present, so
   each gets scored separately.
2. Utility scoring: This is a global score across the whole evaluation to see
   how useful the evaluation might be for a student
3. Sentiment scoring: Mostly for comparison with AI, a score to see what the
   general sentiment of the evaluation is.
4. Submission: Once finished, the review can be submitted and the status in the
   database changes.

All relevant functions that are needed for this process are found in the `./R`
folder. They are mostly for getting data from the database into the UI and
inserting / updating when needed.

## AI review

AI uses the exact same rubric and scoring system as the humans, but does not
need to use the app. There functions for both single and bulk evaluation review.

Single, live AI review can be part of the app experience so humans can run one
on demand, but bulk AI review is an asynchronous process that's run outside of
the app, but still all results are put into the shared database.

All relevant functions that are needed for this process are found in the `./R`
folder.

# Deployment

This repo is for development, production apps that are hosted on Posit connect
are generated from this repo and stored in `./deploy`. There should almost never
be a reason to edit any of the files in this folder by hand, and thus can mostly
be ignored.

The `deployShinyApp` function in `R/helperFunctions.R` is the main function
here.
