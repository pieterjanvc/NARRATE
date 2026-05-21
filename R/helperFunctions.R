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
#' @param gitHubBranch CFME branch
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
deployShinyApp <- function(db, gitHubBranch, dev = F) {
  root <- ifelse(dev, "deploy/CFME-dev", "deploy/CFME")
  # Copy files
  dir.create(root, showWarnings = F)
  file.copy("inst/review_app.R", file.path(root, "app.R"), overwrite = T)
  file.copy("renv.lock", file.path(root, "renv.lock"), overwrite = T)
  file.copy(db, file.path(root, "cfme.db"), overwrite = T)
  devtools::install_github(paste0("pieterjanvc/CFME@", gitHubBranch))
  # Add CFME to lock file
  lockfile <- file.path(root, "renv.lock")
  renv::record(paste0("pieterjanvc/CFME@", gitHubBranch), lockfile = lockfile)
  # renv::record() omits Imports, so packrat on Connect can't determine install
  # order and fails. Patch the entry from the installed package's DESCRIPTION.
  desc <- packageDescription("CFME")
  imports <- trimws(strsplit(gsub("\n\\s*", " ", desc$Imports), ",")[[1]])
  imports <- sub("\\s*\\(.*?\\)\\s*$", "", imports)
  lock <- jsonlite::read_json(lockfile)
  lock$Packages$CFME$Imports <- as.list(imports)
  jsonlite::write_json(lock, lockfile, pretty = 2, auto_unbox = TRUE)
}

#' Backup and replace the DB using pins
#'
#' @param password Admin password, set `adminPass` as an environment variable
#' @param dbPath Path to the DB
#' @param action Any of the following: "import", "export". Can be both as vector
#' @param exportPin (Default = "cfme_db_export") Pin name for the export / backup DB
#' @param importPin (Default = "cfme_db_import") Pin name for the import DB
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
  exportPin = "cfme_db_export",
  importPin = "cfme_db_import",
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

  tryCatch(
    {
      if ("export" %in% action) {
        backup <- pin_dev_set(exportPin, dbPath)
      }

      if ("import" %in% action) {
        # Import the latest upload and replace it locally
        result <- pin_dev_get(importPin, dbPath, tempBackup = F)

        if (!dbIsSQLite(dbPath)) {
          file.remove(dbPath)
          file.copy(result$tempBackup, dbPath)
          file.remove(result$tempBackup)
          stop("Import file not a valid database")
        }

        file.remove(result$tempBackup)
      }
    },
    error = function(e) {
      return(list(success = F, msg = e))
    }
  )
  return(list(
    success = T,
    msg = sprintf("Database %s completed", paste(action, collapse = " and "))
  ))
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

#' Monitor a batch job and send a PushOver notification when complete
#'
#' @param batch_id ID of the batch to monitor
#' @param db_path Path to the SQLite database
#' @param feq_sec (Default = 60) Polling interval in seconds
#' @param max_wait (Default = 2 hours) Maximum time in seconds before killing the process
#'
#' @import callr keyring
#'
#' @returns Invisibly returns the background process handle (callr r_bg object)
#'
batch_status_notify <- function(
  batch_id,
  db_path,
  feq_sec = 60,
  max_wait = 2 * 3600
) {
  auth <- keyring::key_get("PUSHOVER_API", "default") |> jsonlite::fromJSON()

  bg <- callr::r_bg(
    func = function(batch_id, db_path, feq_sec, max_wait, auth) {
      conn <- sqlife::dbGetConn(db_path)

      start_time <- Sys.time()

      tryCatch(
        {
          repeat {
            elapsed <- as.numeric(difftime(
              Sys.time(),
              start_time,
              units = "secs"
            ))
            if (elapsed >= max_wait) {
              dbFinish(conn)
              httr2::request(auth$url) |>
                httr2::req_body_form(
                  token = auth$key,
                  user = auth$user,
                  message = paste("LLM batch", batch_id, "timed out")
                ) |>
                httr2::req_perform()
              break
            }

            batch_info <- llm_batch_status(batch_id, conn)

            if (batch_info$statusCode == 3) {
              httr2::request(auth$url) |>
                httr2::req_body_form(
                  token = auth$key,
                  user = auth$user,
                  message = paste("LLM batch", batch_id, "finished")
                ) |>
                httr2::req_perform()

              break
            }

            Sys.sleep(feq_sec)
          }
        },
        error = function(e) {
          httr2::request(auth$url) |>
            httr2::req_body_form(
              token = auth$key,
              user = auth$user,
              message = paste(
                "LLM batch",
                batch_id,
                "error:",
                conditionMessage(e)
              )
            ) |>
            httr2::req_perform()
        }
      )
    },
    args = list(
      batch_id = batch_id,
      db_path = db_path,
      feq_sec = feq_sec,
      max_wait = max_wait,
      auth = auth
    )
  )

  invisible(bg)
}

#' Look up status codes for a database table or function
#'
#' @param conn CFME database connection
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
