library(httr2)
library(jsonlite)

# ── Config ────────────────────────────────────────────────────────────────────

# ── Option A: Local skill (files live on your machine) ────────────────────────
# Point `path` at the folder that contains SKILL.md plus any helper files.
# Azure's Responses API never needs to upload anything — the model just reads
# the manifest from the JSON and executes locally.

skill_local <- list(
  name = "hopper_method",
  description = "Hopper method to extract structured data from text",
  path = "inst/skills/hopper/SKILL.md"
)

tools_local <- list(
  list(
    type = "shell",
    environment = list(
      type = "local",
      skills = list(skill_local)
    )
  )
)

body <- list(
  model = "gpt-5.4",
  input = "Use the hopper skill on the poem Hope by Emily Dickinson",
  tools = tools_local
)

info <- keyring::key_get("edAI_creds") |> fromJSON()

resp <- request(
  "https://azure-ai.hms.edu/openai/v1/responses"
) |>
  req_headers(
    "Content-Type" = "application/json",
    "api-key" = info$key #keyring::key_get("HMS_AZURE_API")
  ) |>
  req_body_json(body, auto_unbox = TRUE) |>
  req_error(is_error = \(r) FALSE) |> # handle errors manually below
  req_perform()

cat(resp_body_string(resp))
