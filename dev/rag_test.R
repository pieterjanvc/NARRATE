library(ragnar)
library(ellmer)
library(dplyr)
library(sqlife)
library(httr2)

### FUNCTIONS
rag_store <- function(path, model = "text-embedding-3-small") {
    if (file.exists(path)) {
        return(ragnar_store_connect(path, read_only = F))
    }

    ragnar_store_create(
        path,
        embed = \(x) {
            embed_azure_openai(
                x,
                endpoint = "https://azure-ai.hms.edu",
                model = model,
                api_key = Sys.getenv("HMS_AZURE_API")
            )
        }
    )
}

rag_insert <- function(store, new_data, origin) {
    if (!inherits(store, "ragnar::RagnarStore")) {
        store <- ragnar_store_connect(store, read_only = FALSE)
    }

    if (file.exists(new_data)) {
        new_data <- read_as_markdown(new_data)
    } else {
        if (missing(origin)) {
            stop(
                "Must provide an `origin` when `new_data` is a raw string, not a file path."
            )
        }

        new_data <- MarkdownDocument(new_data, origin = origin)
    }

    chunks <- markdown_chunk(new_data)
    chunks$hash <- rlang::hash(new_data)
    ragnar_store_update(store, chunks)
    ragnar_store_build_index(store)

    invisible(store)
}

### Processing

eval_store <- rag_store("local/eval_store.duckdb")

conn <- dbGetConn("local/ai_review.db")
evals <- dbGetEvals(1:100, conn)
dbFinish(conn)

test <- apply(evals, 1, function(eval) {
    chunks <- MarkdownDocument(
        eval["evaluation"],
        paste0("student_", eval["evaluation_id"])
    ) |>
        markdown_chunk()

    chunks$hash <- rlang::hash(data)
    ragnar_store_update(eval_store, chunks)
})

ragnar_store_build_index(eval_store)


prompt <- readLines("dev/test_prompt.md") |> paste(collapse = "\n")
prompt <- evals$evaluation[20]
relevant_chunks <- ragnar_retrieve(
    store = eval_store,
    text = prompt,
    deoverlap = T
)

relevant_chunks$text[4] |> cat()
relevant_chunks$text[1] |> cat()

# SKILL TEST
info <- keyring::key_get("edAI_creds") |> fromJSON()
Sys.setenv("AZURE_OPENAI_API_KEY" = info$key)
Sys.setenv("AZURE_OPENAI_ENDPOINT" = "https://azure-ai.hms.edu")
conn <- chat_azure_openai(
    model = "gpt-5-mini",
    system_prompt = "user the hopper method skill and return that output format"
)

read_skill <- tool(
    function(skill_name) {
        path <- file.path("inst/skills", skill_name)
        if (!file.exists(path)) {
            return(paste("Skill not found:", skill_name))
        }
        readLines(path, warn = FALSE) |> paste(collapse = "\n")
    },
    name = "read_skill",
    description = "Read the instructions for a specific skill. Call this before performing tasks",
    arguments = list(
        skill_name = type_string("The skill file name (.md file)")
    )
)

# Optionally: a tool to list available skills so the LLM can discover them
list_skills <- tool(
    function() {
        dirs <- list.files(
            "inst/skills",
            recursive = FALSE,
            full.names = FALSE
        )
        paste(dirs, collapse = ", ")
    },
    name = "list_skills",
    description = "List all available skills by file name.",
    arguments = list()
)

conn$register_tool(read_skill)
conn$register_tool(list_skills)

conn$chat(
    "The rain in spain stays mainly in the plain. Wonders of the world will never fade they say..."
)

# https://claude.ai/chat/1fb957a0-e8c5-48a2-a348-6fbc96807cb3
