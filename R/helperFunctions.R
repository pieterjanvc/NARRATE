#' Initialize a new NARRATE database, load evaluation data, and seed reviews
#'
#' Creates a new NARRATE SQLite database from the package schema, imports
#' evaluation data from an .xlsx file, registers the default AI and human
#' reviewers, links the rubric prompts, and assigns the same random sample
#' of evaluations to every reviewer so review can start immediately.
#'
#' @param path Path to the (new) NARRATE SQLite database
#' @param dataset Path to the .xlsx file with the combined evaluation data
#' @param default_ai (Default = "gpt-5.1") Model name used to create the
#' default AI reviewer
#' @param default_reviewers (Default = c("TK", "AW", "KM", "test")) Usernames
#' used to create the default human reviewers
#' @param n_assigned (Default = 3) Number of evaluations, per
#' summary_flg / complete group, randomly assigned to every reviewer.
#' Ignored if `id_assigned` is set
#' @param id_assigned (Default = NULL) Vector of evaluation ids to assign to
#' every reviewer instead of a random sample. When set, `n_assigned` is
#' ignored and no random sampling takes place
#' @param seed (Default = 1) Random seed used when sampling evaluations
#' @param redactedOnly (Default = TRUE) If TRUE, only redacted evaluations
#' are inserted into the database; the version with identifiers is omitted
#' @param force_overwrite (Default = FALSE) If a database already exists at
#' `path`, stop unless this is TRUE. If TRUE, the existing database (and any
#' `-wal` / `-shm` / `-journal` sidecar files) is moved to the system temp
#' folder with a timestamp appended before a new one is created
#' @param verbose (Default = TRUE) Show messages with the steps in the process
#'
#' @import dplyr
#' @importFrom readxl read_xlsx
#'
#' @returns A list with the AI and human reviewer records, the rubric id,
#' the sampled evaluation ids and the created review assignments. The
#' database connection opened internally is closed before returning; call
#' `dbGetConn(path)` to continue working with the database
#' @export
#'
narrate_init <- function(
  path,
  dataset,
  default_ai = "gpt-5.1",
  default_reviewers = c("TK", "AW", "KM", "test"),
  n_assigned = 3,
  id_assigned = NULL,
  seed = 1,
  redactedOnly = TRUE,
  force_overwrite = FALSE,
  verbose = TRUE
) {
  if (pkgload::is_dev_package("NARRATE")) {
    schema <- "inst/narrate.sql"
  } else {
    schema <- system.file("narrate.sql", package = "NARRATE")
  }

  if (file.exists(path)) {
    if (!force_overwrite) {
      stop(
        "A database already exists at ",
        path,
        ". Set force_overwrite = TRUE to replace it."
      )
    } else if (verbose) {
      print("Move old database to temp folder")
    }

    timestamp <- as.integer(Sys.time())
    for (suffix in c("", "-wal", "-shm", "-journal")) {
      sidecar <- paste0(path, suffix)
      if (file.exists(sidecar)) {
        backup_path <- file.path(
          tempdir(),
          paste0(basename(sidecar), "_", timestamp)
        )
        file.copy(sidecar, backup_path)
        file.remove(sidecar)
        message(
          "Existing database file ",
          sidecar,
          " moved to ",
          backup_path,
          ". This is a system temp folder so the file may (but is not ",
          "guaranteed to) be removed automatically on next system startup."
        )
      }
    }
  }

  if (verbose) {
    print("Creating new database with default values")
  }
  dbSetup(path, schema)

  # Add all evaluation data (manages its own connection internally)
  if (verbose) {
    print("Adding data")
  }
  combined_data <- readxl::read_xlsx(dataset)
  dbAddEvaluations(combined_data, path, redactedOnly = redactedOnly)

  if (verbose) {
    print("Random initial review assignments")
  }
  conn <- dbGetConn(path)

  # Add default AI and human reviewers
  ai_reviewer <- dbReviewerAI(conn, model = default_ai)
  human_reviewers <- lapply(default_reviewers, function(username) {
    dbReviewerHuman(conn, username = username)
  }) |>
    bind_rows()

  # The initial rubric (competencies, disambiguation, scores, rules) is
  # seeded directly by the schema; generate and link its prompts here
  rubric_id <- tbl(conn, "rubric") |>
    summarise(id = max(id, na.rm = TRUE)) |>
    pull(id)
  rubric_link_prompts(conn, rubric_id)

  # Assign the same set of evaluations to every reviewer: either the
  # provided ids, or a random sample if none were given
  if (!is.null(id_assigned)) {
    eval_sample <- id_assigned
  } else {
    set.seed(seed)
    eval_sample <- tbl(conn, "evaluation") |>
      group_by(summary_flg, complete) |>
      slice_sample(n = n_assigned) |>
      pull(id)
  }

  reviewer_ids <- c(ai_reviewer$id, human_reviewers$id)

  assignments <- lapply(reviewer_ids, function(reviewer_id) {
    dbReviewAssignment(
      conn,
      reviewer_id = reviewer_id,
      evaluation_id = eval_sample,
      rubric_id = rubric_id,
      redacted = TRUE,
      include_questions = TRUE
    )
  }) |>
    bind_rows()

  dbFinish(conn)

  return(invisible(TRUE))
}

#' Check if a prompt is structured correctly and returned a parsed version
#'
#' @param prompt String of text to check
#'
#' @returns list with
#' - success: TRUE / FALSE
#' - msg: message
#' - content: list containing parsed prompt if successful
#'
#' @import stringr
#'
#' @export
#'
parsePrompt <- function(prompt) {
  sections <- str_split(prompt, "(?m)^#[^#]")[[1]][-1]
  #Check if there are 3 major sections (Task, rubric, to return)
  if (length(sections) != 3) {
    return(list(
      success = F,
      msg = paste(
        "The prompt does not have the 3 expected sections (#):",
        " task, rubric, to return"
      ),
      content = NULL
    ))
  }

  task <- str_split(sections[1], "\\n", n = 2)[[1]][-1] |> str_trim()

  # Competencies
  rubric <- str_split(sections[2], "(?m)^##[^#]")[[1]][-1]

  competencies <- str_split(rubric[1], "(?m)^###\\s\\d.\\s")[[1]][-1] |>
    str_split("\n", n = 2)

  if (length(competencies) == 0) {
    return(list(
      success = F,
      msg = "Cannot find any competencies in the prompt",
      content = NULL
    ))
  }

  competencies <- lapply(competencies, function(competency) {
    list(
      name = competency[1] |> str_trim(),
      description = competency[2] |> str_trim()
    )
  }) |>
    setNames(1:length(competencies))

  # Competency Scoring
  compScore <- str_split(rubric[2], "(?m)^###[^#]")[[1]][-1] |>
    str_split("\\:\\s?", n = 2)

  compScore <- setNames(compScore, sapply(compScore, "[[", 1))

  compScore <- lapply(compScore, function(x) {
    x <- str_split(x[[2]], "\n\\-\\s?")[[1]] |> str_trim()
    list(desciption = x[1], options = x[-1])
  })

  # Overall Scoring
  overallScore <- str_split(rubric[3], "(?m)^###[^#]")[[1]][-1] |>
    str_split("\\:\\s?", n = 2)

  overallScore <- setNames(overallScore, sapply(overallScore, "[[", 1))

  overallScore <- lapply(overallScore, function(x) {
    x <- str_split(x[[2]], "\n\\-\\s?")[[1]] |> str_trim()
    list(desciption = x[1], options = x[-1])
  })

  retrunMsg <- str_split(sections[3], "\\n", n = 2)[[1]][-1] |> str_trim()

  return(list(
    success = T,
    msg = "Prompt data successfully parsed",
    content = list(
      task = task,
      competencies = competencies,
      compScore = compScore,
      overallScore = overallScore,
      retrunMsg = retrunMsg
    )
  ))
}

#' Provide missing values if variable does not exist
#'
#' @param var Variabe to check
#' @param useNull (Default = F). Return NA if FALSE else NULL
#' @param n (Default = 1) How may times to repeat NA
#'
#' @returns A vector of values, NAs or NULL depending on settings
#'
missingVal <- function(var, useNull = F, n = 1) {
  if (!missing(var)) {
    var
  } else if (useNull) {
    NULL
  } else {
    rep(NA, n)
  }
}


#' Get the set (function) arguments of the current environment
#'
#' This is useful at the start of a function to capture all passed arguments
#'
#' @returns A list with the set function arguments and their values
#'
getFunArgs <- function(exclude) {
  x <- as.list(parent.frame())
  x <- x[!names(x) %in% exclude]
  x <- x[sapply(x, function(x) typeof(x) != "symbol")]
  if (length(x) == 0) {
    NULL
  } else {
    x
  }
}


#' Delop Shiny App
#'
#' @param db Database to use
#' @param gitHubBranch NARRATE branch
#' @param dev Deploy to dev app
#'
#' @import shiny bslib
#' @importFrom DT DTOutput renderDT datatable
#' @importFrom tidyr pivot_wider
#'
#' @returns Nothing
#'
#' @export
#'
deployShinyApp <- function(
  db,
  gitHubBranch,
  dev = F,
  app_file = "inst/review_app.R"
) {
  root <- ifelse(dev, "deploy/NARRATE-dev", "deploy/NARRATE")
  # Copy files
  dir.create(root, showWarnings = F)
  file.copy(app_file, file.path(root, "app.R"), overwrite = T)
  file.copy("renv.lock", file.path(root, "renv.lock"), overwrite = T)
  file.copy(db, file.path(root, "narrate.db"), overwrite = T)
  pak::pak(paste0("pieterjanvc/NARRATE@", gitHubBranch))
  # Add NARRATE to lock file
  lockfile <- file.path(root, "renv.lock")
  renv::record(
    paste0("pieterjanvc/NARRATE@", gitHubBranch),
    lockfile = lockfile
  )
  # renv::record() omits Imports, so packrat on Connect can't determine install
  # order and fails. Patch the entry from the installed package's DESCRIPTION.
  desc <- packageDescription("NARRATE")
  imports <- trimws(strsplit(gsub("\n\\s*", " ", desc$Imports), ",")[[1]])
  imports <- sub("\\s*\\(.*?\\)\\s*$", "", imports)
  lock <- jsonlite::read_json(lockfile)
  lock$Packages$NARRATE$Imports <- as.list(imports)
  jsonlite::write_json(lock, lockfile, pretty = 2, auto_unbox = TRUE)
}

#' Backup and replace the DB using pins
#'
#' @param password Admin password, set `adminPass` as an environment variable
#' @param dbPath Path to the DB
#' @param action Any of the following: "import", "export". Can be both as vector
#' @param exportPin (Default = "narrate_db_export") Pin name for the export / backup DB
#' @param importPin (Default = "narrate_db_import") Pin name for the import DB
#' @param nBackups (Default = 3) N most recent exports to keep
#'
#' @import pins
#' @importFrom sqlife dbIsSQLite
#'
#' @returns list with success an msg
#' @export
#'
pinDB <- function(
  dbPath,
  action,
  exportPin = "narrate_db_export",
  importPin = "narrate_db_import",
  nBackups = 3
) {
  if (!dbIsSQLite(dbPath)) {
    return(list(success = F, msg = "Database file not found"))
  }

  if (
    missing(action) ||
      is.null(action) ||
      !all(action %in% c("import", "export"))
  ) {
    return(list(success = F, msg = "Action must be: import, export or both"))
  }

  result <- tryCatch(
    {
      # Always back up the current DB before it can be overwritten by an import
      if ("export" %in% action || "import" %in% action) {
        pin_dev_set(exportPin, dbPath, nBackups = nBackups)
      }

      if ("import" %in% action) {
        # Import the latest upload and replace it locally
        imported <- pin_dev_get(importPin, dbPath, tempBackup = F)

        if (!dbIsSQLite(dbPath)) {
          file.remove(dbPath)
          file.copy(imported$tempBackup, dbPath)
          file.remove(imported$tempBackup)
          stop("Import file not a valid database")
        }

        file.remove(imported$tempBackup)
      }

      list(
        success = T,
        msg = sprintf("Database %s completed", paste(action, collapse = " and "))
      )
    },
    error = function(e) {
      list(success = F, msg = conditionMessage(e))
    }
  )
  return(result)
}

#' Get a pin
#'
#' @param path Path to save the file to
#' @param pinName name of the pin to access
#'
#' @import pins
#' @importFrom stringr str_extract
#'
#' @returns list with new file and temp backup if set
#' @export
#'
pin_dev_get <- function(
  pinName,
  path,
  tempBackup = T
) {
  board <- board_connect()
  fullPin <- paste0(board$account, "/", pinName[1])

  if (!fullPin %in% pin_list(board)) {
    stop(pinName[1], " pin not found for ", board$account)
  }

  # Backup to temp if needed
  if (tempBackup && file.exists(path)) {
    ext <- str_extract(path, "\\.[^.]+$")
    tFile <- tempfile(fileext = ifelse(is.na(ext), "", ext))
    file.copy(path, tFile, overwrite = T)
    print(paste("Temp backup created at", tFile))
  } else {
    tempBackup <- F
  }

  # Copy and change permissions
  new <- pin_download(board, fullPin)
  file.remove(path)
  file.copy(new, path, overwrite = T)
  Sys.chmod(path, file.info(dirname(path))$mode)
  file.remove(new)

  return(list(
    new = path,
    tempBackup = ifelse(tempBackup, tFile, NA_character_)
  ))
}

#' Set a pin
#'
#' @param pinName Name of the pin
#' @param path Path to the file to pin
#' @param nBackups (Default = 3) N most recent pins to keep online
#'
#' @import pins
#'
#' @returns The name of the new pin
#' @export
#'
pin_dev_set <- function(
  pinName,
  path,
  nBackups = 3
) {
  board <- board_connect()
  newPin <- pin_upload(board, path, pinName)
  # Only keep n backups
  pin_versions_prune(board, newPin, n = nBackups)
  return(newPin)
}

#' Backup the local DB and replace it with the latest online export
#'
#' Copies the current local database to
#' \code{<backupDir>/narrate-<today>.db} (e.g. \code{local/backup/narrate-2026-09-07.db}),
#' then downloads the latest exported database from the pin and writes it to
#' \code{dbPath}.
#'
#' @param dbPath (Default = "local/narrate.db") Path to the local database
#' @param exportPin (Default = "narrate_db_export") Pin holding the exported DB
#' @param backupDir (Default = "local/backup") Directory for the dated backup
#'
#' @import pins
#'
#' @returns (invisibly) list with the backup path and the refreshed db path
#' @export
#'
fetch_online_db <- function(
  dbPath = "local/narrate.db",
  exportPin = "narrate_db_export",
  backupDir = "local/backup"
) {
  if (!file.exists(dbPath)) {
    stop("Local database not found at ", dbPath)
  }

  # Back up the current local DB before it gets overwritten
  dir.create(backupDir, showWarnings = F, recursive = T)
  backupPath <- file.path(
    backupDir,
    sprintf("narrate-%s.db", format(Sys.Date(), "%Y-%m-%d"))
  )
  file.copy(dbPath, backupPath, overwrite = T)
  print(paste("Local database backed up to", backupPath))

  # Pull the latest export down and set it as the new local DB
  pin_dev_get(exportPin, dbPath, tempBackup = F)
  print(paste("Latest online export written to", dbPath))

  invisible(list(backup = backupPath, db = dbPath))
}

#' Poll a batch to a terminal state and send one PushOver notification
#'
#' The monitoring loop behind batch_status_notify(). Kept as its own function
#' so the detached process launched by batch_status_notify() can reach it with
#' a one-line generated script (pkgload::load_all() + this call) instead of
#' carrying the whole loop inline.
#'
#' @param batch_id ID of the batch to monitor
#' @param db_path Path to the SQLite database
#' @param auth Named list with url/key/user for the PushOver API. Defaults to
#'   reading the `PUSHOVER_URL` / `PUSHOVER_KEY` / `PUSHOVER_USER` env vars,
#'   which is how batch_status_notify() hands the credentials to its detached
#'   monitor (keeps them out of the on-disk script)
#' @param feq_sec Polling interval in seconds
#' @param max_wait Maximum time in seconds before giving up and notifying
#'
#' @returns Invisibly, the message that was sent
#' @keywords internal
batch_notify_poll <- function(
  batch_id,
  db_path,
  auth = list(
    url = Sys.getenv("PUSHOVER_URL"),
    key = Sys.getenv("PUSHOVER_KEY"),
    user = Sys.getenv("PUSHOVER_USER")
  ),
  feq_sec = 60,
  max_wait = 2 * 3600
) {
  push <- function(message) {
    httr2::request(auth$url) |>
      httr2::req_body_form(token = auth$key, user = auth$user, message = message) |>
      httr2::req_perform()
    message
  }

  conn <- sqlife::dbGetConn(db_path)
  start_time <- Sys.time()

  # sqlife::dbGetConn() registers a deferred check that errors if the
  # connection isn't closed with dbFinish() before its frame exits, so every
  # exit path has to close it before notifying / returning.
  settle <- function(message) {
    try(dbFinish(conn), silent = TRUE)
    invisible(push(message))
  }

  tryCatch(
    repeat {
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      if (elapsed >= max_wait) {
        return(settle(paste("LLM batch", batch_id, "timed out")))
      }

      batch_info <- llm_batch_status(batch_id, conn)

      if (batch_info$statusCode == 3) {
        return(settle(paste("LLM batch", batch_id, "finished")))
      }

      # Failed / expired / cancelled - notify now rather than waiting out max_wait
      if (batch_info$statusCode < 0) {
        return(settle(paste(
          "LLM batch", batch_id,
          "did not complete (statusCode", batch_info$statusCode, ")"
        )))
      }

      Sys.sleep(feq_sec)
    },
    error = function(e) {
      settle(paste("LLM batch", batch_id, "error:", conditionMessage(e)))
    }
  )
}

#' Monitor a batch job and send a PushOver notification when it settles
#'
#' Polls llm_batch_status() in a detached R process and notifies once the
#' batch completes (statusCode 3), fails/expires/cancels (statusCode < 0), or
#' the max_wait is reached. Content-agnostic - works for any batch step
#' (extract, resolve, score).
#'
#' @param batch_id ID of the batch to monitor
#' @param db_path Path to the SQLite database
#' @param feq_sec (Default = 60) Polling interval in seconds
#' @param max_wait (Default = 2 hours) Maximum time in seconds before giving up
#' @param pkg_path (Default = here::here()) Path to the package source, loaded
#'   with pkgload::load_all() in the monitor process so that dev-only functions
#'   (e.g. llm_batch_status) are available there too
#' @param work_dir (Default = a "batch_notify" folder next to db_path) Where the
#'   generated monitor script and its log are written
#'
#' @details The monitor is launched as a fully detached `Rscript` process
#'   (`cleanup = FALSE`, `supervise = FALSE`) running a small script written to
#'   `work_dir` - not a `callr::r_bg()` closure. `callr::r_bg()` keeps its
#'   bootstrap script in the *caller's* session tempdir, which R deletes on
#'   exit, so an r_bg monitor dies the moment a submit-and-return script
#'   finishes. Writing a standalone script to a persistent folder and inheriting
#'   the caller's library paths via `R_LIBS` lets the monitor outlive the
#'   calling session. The PushOver credentials are read from the keyring once
#'   here and passed to the detached process as environment variables (not
#'   written into the on-disk script), so it never touches the keyring itself.
#'   The generated script deletes itself on start.
#'
#' @import keyring
#'
#' @returns Invisibly, a list with the monitor's `process` (processx handle),
#'   `script` path, and `log` path
#'
batch_status_notify <- function(
  batch_id,
  db_path,
  feq_sec = 60,
  max_wait = 2 * 3600,
  pkg_path = here::here(),
  work_dir = file.path(dirname(normalizePath(db_path, mustWork = FALSE)), "batch_notify")
) {
  auth <- keyring::key_get("PUSHOVER_API", "default") |> jsonlite::fromJSON()

  dir.create(work_dir, showWarnings = FALSE, recursive = TRUE)
  stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  script_path <- file.path(work_dir, sprintf("notify_%s_%s.R", batch_id, stamp))
  log_path <- file.path(work_dir, sprintf("notify_%s_%s.log", batch_id, stamp))

  writeLines(
    c(
      sprintf("invisible(file.remove(%s))", encodeString(script_path, quote = '"')),
      sprintf("pkgload::load_all(%s, quiet = TRUE)", encodeString(normalizePath(pkg_path), quote = '"')),
      "NARRATE:::batch_notify_poll(",
      sprintf("  batch_id = %s,", batch_id),
      sprintf("  db_path = %s,", encodeString(normalizePath(db_path, mustWork = FALSE), quote = '"')),
      sprintf("  feq_sec = %s,", feq_sec),
      sprintf("  max_wait = %s", max_wait),
      ")"
    ),
    script_path
  )

  libs <- paste(.libPaths(), collapse = .Platform$path.sep)
  px <- processx::process$new(
    file.path(R.home("bin"), "Rscript"),
    c("--no-save", "--no-restore", script_path),
    env = c(
      "current",
      R_LIBS = libs, R_LIBS_USER = libs, R_LIBS_SITE = libs,
      HMS_AZURE_API = Sys.getenv("HMS_AZURE_API"),
      PUSHOVER_URL = auth$url, PUSHOVER_KEY = auth$key, PUSHOVER_USER = auth$user
    ),
    stdout = log_path,
    stderr = "2>&1",
    cleanup = FALSE,
    supervise = FALSE
  )

  invisible(list(process = px, script = script_path, log = log_path))
}

#' Look up status codes for a database table or function
#'
#' @param conn NARRATE database connection
#' @param table Name of a database table or function to filter by (optional)
#'
#' @import dplyr
#' @returns Data frame of matching status codes
#' @export
status_codes <- function(conn, table = NULL) {
  q <- tbl(conn, "status_codes")
  if (!is.null(table)) {
    tbl_filter <- table # avoid name collision with the "table" column in dplyr mask
    q <- filter(
      q,
      .data[["table"]] == tbl_filter | .data[["function"]] == tbl_filter
    )
  }
  collect(q) |>
    select(-id) |>
    arrange(.data[["table"]], .data[["function"]], code)
}
