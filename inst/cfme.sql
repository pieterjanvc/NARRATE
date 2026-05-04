CREATE TABLE "student" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "learner_anon_id" TEXT UNIQUE NOT NULL,
  "pce_assign" TEXT,
  "society" TEXT,
  "acad_prog" TEXT,
  "acad_prog_trk" TEXT,
  "gender" TEXT,
  "urim_flg" TEXT,
  "age" REAL
);

CREATE TABLE "evaluator" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "original_evaluator_id" INTEGER NOT NULL,
  "evaluator" TEXT NOT NULL,
  "acad_title" TEXT
);

CREATE TABLE "clerkship" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "clerkship" TEXT NOT NULL,
  "location" TEXT
);

CREATE TABLE "rotation" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "student_id" INTEGER NOT NULL,
  "clerkship_id" INTEGER NOT NULL,
  "rotation_date" TEXT NOT NULL,
  "first_nbme_score" REAL,
  FOREIGN KEY ("student_id") REFERENCES "student"("id") ON DELETE CASCADE,
  FOREIGN KEY ("clerkship_id") REFERENCES "clerkship"("id") ON DELETE CASCADE
);

CREATE TABLE "evaluation" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "rotation_id" INTEGER NOT NULL,
  "evaluator_id" INTEGER NOT NULL,
  "summary_flg" INTEGER NOT NULL,
  "complete" INTEGER,
  "acad_yr" TEXT,
  FOREIGN KEY ("rotation_id") REFERENCES "rotation"("id") ON DELETE CASCADE,
  FOREIGN KEY ("evaluator_id") REFERENCES "evaluator"("id") ON DELETE CASCADE
);

CREATE TABLE "question" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "question" TEXT NOT NULL
);

CREATE TABLE "answer" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "question_id" INTEGER NOT NULL,
  "evaluation_id" INTEGER NOT NULL,
  "submission_date" TEXT NOT NULL,
  "answer_txt" TEXT,
  "answer_txt_redacted" TEXT,
  "rowid" INTEGER,
  FOREIGN KEY ("evaluation_id") REFERENCES "evaluation"("id") ON DELETE CASCADE,
  FOREIGN KEY ("question_id") REFERENCES "question"("id") ON DELETE CASCADE
);

CREATE TABLE "reviewer" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "timestamp" TEXT DEFAULT (datetime('now', 'localtime')),
  "human" INTEGER NOT NULL,
  "model" TEXT,
  "username" TEXT,
  "first_name" TEXT,
  "last_name" TEXT,
  "note" TEXT
);

CREATE TABLE "prompt" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "task" TEXT,
  "timestamp" TEXT DEFAULT (datetime('now', 'localtime')),
  "hash" TEXT UNIQUE NOT NULL,
  "prompt" TEXT,
  "note" TEXT
);

CREATE TABLE "batch" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "file_input_id" TEXT,
  "prompt_id" INTEGER,
  "batch_id" TEXT,
  "file_output_id" TEXT,  
  "created" TEXT DEFAULT (datetime('now', 'localtime')),
  "checked" TEXT,
  "finished" TEXT,
  "statusCode" INTEGER NOT NULL,
  "n_requests" INTEGER,
  "tokens_in" INTEGER,
  "tokens_out" INTEGER,
  "note" TEXT,
  FOREIGN KEY ("prompt_id") REFERENCES "prompt"("id") ON DELETE CASCADE
);

CREATE TABLE "review_assignment" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "created" TEXT DEFAULT (datetime('now', 'localtime')),
  "modified" TEXT DEFAULT (datetime('now', 'localtime')),
  "evaluation_id" INTEGER NOT NULL,  
  "reviewer_id" INTEGER NOT NULL,
  "statusCode" INTEGER NOT NULL,
  "include_questions" INTEGER,
  "redacted" INTEGER,
  "utility" INTEGER,
  "sentiment" INTEGER,
  "prompt_id" INTEGER,
  "prompt_extract_id" INTEGER,
  "prompt_score_id" INTEGER,
  "tokens_in" INTEGER,
  "tokens_out" INTEGER,
  "duration" REAL,
  "note" TEXT,
  FOREIGN KEY ("evaluation_id") REFERENCES "evaluation"("id") ON DELETE CASCADE,
  FOREIGN KEY ("prompt_id") REFERENCES "prompt"("id") ON DELETE CASCADE,
  FOREIGN KEY ("reviewer_id") REFERENCES "reviewer"("id") ON DELETE CASCADE,
  FOREIGN KEY ("prompt_extract_id") REFERENCES "prompt"("id") ON DELETE CASCADE,
  FOREIGN KEY ("prompt_score_id") REFERENCES "prompt"("id") ON DELETE CASCADE
);

CREATE TABLE "batch_review" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "batch_id" INTEGER NOT NULL,
  "review_assignment_id" INTEGER NOT NULL,  
  FOREIGN KEY ("batch_id") REFERENCES "batch"("id") ON DELETE CASCADE,
  FOREIGN KEY ("review_assignment_id") REFERENCES "review_assignment"("id") ON DELETE CASCADE
);

CREATE TABLE "competency" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "cID" INTEGER NOT NULL,
  "name" TEXT NOT NULL,
  "description" TEXT NOT NULL,
  "timestamp" TEXT DEFAULT (datetime('now', 'localtime')),
  "note" TEXT
);

CREATE TABLE "competency_diff" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "competency_id1" INTEGER NOT NULL,
  "competency_id2" INTEGER,
  "description" TEXT NOT NULL,
  "timestamp" TEXT DEFAULT (datetime('now', 'localtime')),
  "note" TEXT,
  FOREIGN KEY ("competency_id1") REFERENCES "competency"("id") ON DELETE CASCADE,
  FOREIGN KEY ("competency_id2") REFERENCES "competency"("id") ON DELETE CASCADE
);

CREATE TABLE "score" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "category" TEXT NOT NULL,
  "value" TEXT NOT NULL,
  "description" TEXT NOT NULL,
  "example" TEXT,
  "timestamp" TEXT DEFAULT (datetime('now', 'localtime')),
  "note" TEXT
);

CREATE TABLE "competency_score" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "review_assignment_id" INTEGER NOT NULL,
  "competency_id" INTEGER NOT NULL,
  "specificity" INTEGER,
  "note" TEXT,
  FOREIGN KEY ("review_assignment_id") REFERENCES "review_assignment"("id") ON DELETE CASCADE,
  FOREIGN KEY ("competency_id") REFERENCES "competency"("id") ON DELETE CASCADE
);

CREATE TABLE "competency_text" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "competency_score_id" INTEGER NOT NULL,
  "text_match" TEXT NOT NULL,
  FOREIGN KEY ("competency_score_id") REFERENCES "competency_score"("id") ON DELETE CASCADE
);

CREATE TABLE "status_codes" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "table" TEXT,
  "function" TEXT,
  "code" INTEGER NOT NULL,
  "description" TEXT NOT NULL,
  "note" TEXT
);

INSERT INTO "status_codes" ("table", "code", "description", "note") VALUES
  -- review_assignment: shared (human + AI)
  ('review_assignment', -1, 'Completed with flag',        'Human reviewer flagged'),
  ('review_assignment',  0, 'New',                        NULL),
  ('review_assignment',  1, 'In progress',                'Human: reviewer started; AI: extraction submitted to batch'),
  ('review_assignment',  2, 'Completed',                  'Human completion or legacy single-call AI'),
  -- review_assignment: AI batch only
  ('review_assignment', -2, 'AI extraction failed',       NULL),
  ('review_assignment', -3, 'AI scoring failed',          NULL),
  ('review_assignment',  3, 'Batch extraction complete',  NULL),
  ('review_assignment',  4, 'Batch scoring submitted',    NULL),
  ('review_assignment',  5, 'Batch scoring complete',     NULL),
  -- batch
  ('batch', -3, 'Cancelled',   NULL),
  ('batch', -2, 'Expired',     NULL),
  ('batch', -1, 'Failed',      NULL),
  ('batch',  1, 'Submitted',   NULL),
  ('batch',  2, 'In progress', NULL),
  ('batch',  3, 'Completed',   NULL),
  ('batch',  4, 'Processed',   NULL);

INSERT INTO "status_codes" ("function", "code", "description") VALUES
  -- llm_comp_extract return codes (not stored in DB)
  ('llm_comp_extract',         0, 'API error'),
  ('llm_comp_extract',         1, 'Parse error'),
  ('llm_comp_extract',         2, 'Success'),
  -- llm_comp_score return codes (not stored in DB)
  ('llm_comp_score',           0, 'API error'),
  ('llm_comp_score',           1, 'Parse error'),
  ('llm_comp_score',           2, 'Success'),
  -- batch_results_preprocess return codes (not stored in DB)
  ('batch_results_preprocess', -1, 'Missing result'),
  ('batch_results_preprocess', -2, 'Parse error'),
  ('batch_results_preprocess',  2, 'Success');
