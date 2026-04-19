library(httr2)
library(jsonlite)

`%||%` <- function(a, b) if (!is.null(a)) a else b

call_responses <- function(body) {
  resp <- request("https://azure-ai.hms.edu/openai/v1/responses") |>
    req_headers(
      "Content-Type" = "application/json",
      "api-key" = info$key
    ) |>
    req_body_json(body, auto_unbox = TRUE) |>
    req_perform()

  resp_body_json(resp, simplifyVector = FALSE)
}

# Skill path should be the FOLDER that contains SKILL.md
skill_local <- list(
  name = "hopper_method",
  description = "Hopper method to extract structured data from text",
  path = normalizePath("inst/skills/hopper", mustWork = TRUE)
)

tools_local <- list(list(
  type = "shell",
  environment = list(
    type = "local",
    skills = list(skill_local)
  )
))

body1 <- list(
  model = "gpt-5.4",
  instructions = paste(
    "You are a helpful assistant. Use the hopper_method skill.",
    "Shell commands run in bash. Use `cat` to read files (do not use Windows `type`)."
  ),
  input = readLines("dev/poem.txt") |> paste(collapse = "\n"),
  tools = tools_local,
  max_output_tokens = 512
)

r <- call_responses(body1)

repeat {
  # Find shell_call items
  shell_calls <- Filter(function(it) identical(it$type, "shell_call"), r$output)

  if (length(shell_calls) == 0) {
    break
  } # model is done (should have output_text)

  call <- shell_calls[[1]]
  cmds <- call$action$commands

  # Execute all requested commands locally
  outputs <- lapply(cmds, function(cmd) {
    out <- system(paste(cmd, "2>&1"), intern = TRUE)
    status <- attr(out, "status")
    if (is.null(status)) {
      status <- 0
    }

    list(
      stdout = paste(out, collapse = "\n"),
      stderr = "",
      outcome = list(type = "exit", exit_code = status)
    )
  })

  tool_output_item <- list(
    type = "shell_call_output",
    call_id = call$call_id,
    max_output_length = call$action$max_output_length %||% 4096,
    output = outputs
  )

  # Continue the SAME run
  r <- call_responses(list(
    model = "gpt-5.4",
    previous_response_id = r$id,
    tools = tools_local, # pass tools again
    input = list(tool_output_item),
    max_output_tokens = 512
  ))
}

r$output[[1]]$content[[1]]$text
