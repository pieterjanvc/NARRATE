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
  "acad_title" TEXT,
  "core_faculty_start" TEXT
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
  "core_faculty" INTEGER,
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
  "rubric_id" INTEGER NOT NULL,
  "statusCode" INTEGER NOT NULL,
  "include_questions" INTEGER,
  "redacted" INTEGER,
  "utility_score_id" INTEGER,
  "utility_score_value" INTEGER,
  "sentiment_score_id" INTEGER,
  "sentiment_score_value" INTEGER,
  "tokens_in" INTEGER,
  "tokens_out" INTEGER,
  "duration" REAL,
  "note" TEXT,
  FOREIGN KEY ("evaluation_id") REFERENCES "evaluation"("id") ON DELETE CASCADE,
  FOREIGN KEY ("rubric_id") REFERENCES "rubric"("id") ON DELETE CASCADE,
  FOREIGN KEY ("reviewer_id") REFERENCES "reviewer"("id") ON DELETE CASCADE,
  FOREIGN KEY ("utility_score_id") REFERENCES "utility"("id") ON DELETE CASCADE,
  FOREIGN KEY ("sentiment_score_id") REFERENCES "sentiment"("id") ON DELETE CASCADE
);

CREATE TABLE "batch_review" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "batch_id" INTEGER NOT NULL,
  "review_assignment_id" INTEGER NOT NULL,  
  FOREIGN KEY ("batch_id") REFERENCES "batch"("id") ON DELETE CASCADE,
  FOREIGN KEY ("review_assignment_id") REFERENCES "review_assignment"("id") ON DELETE CASCADE
);

CREATE TABLE "rubric" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "prompt_extract_id" INTEGER,
  "prompt_score_id" INTEGER,
  "prev_id" INTEGER,
  "info" TEXT,
  "timestamp" TEXT DEFAULT (datetime('now', 'localtime')),
  FOREIGN KEY ("prompt_extract_id") REFERENCES "prompt"("id") ON DELETE CASCADE,
  FOREIGN KEY ("prompt_score_id") REFERENCES "prompt"("id") ON DELETE CASCADE
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

CREATE TABLE "rubric_competency" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "rubric_id" INTEGER NOT NULL,
  "competency_id" INTEGER NOT NULL,
  "order" INTEGER NOT NULL,
  "timestamp" TEXT DEFAULT (datetime('now', 'localtime')),
  FOREIGN KEY ("rubric_id") REFERENCES "rubric"("id") ON DELETE CASCADE,
  FOREIGN KEY ("competency_id") REFERENCES "competency"("id") ON DELETE CASCADE
);

CREATE TABLE "specificity" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "value" INTEGER NOT NULL,
  "description" TEXT NOT NULL,
  "example" TEXT,
  "timestamp" TEXT DEFAULT (datetime('now', 'localtime')),
  "note" TEXT
);

CREATE TABLE "rubric_specificity" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "rubric_id" INTEGER NOT NULL,
  "specificity_id" INTEGER NOT NULL,
  "timestamp" TEXT DEFAULT (datetime('now', 'localtime')),
  FOREIGN KEY ("rubric_id") REFERENCES "rubric"("id") ON DELETE CASCADE,
  FOREIGN KEY ("specificity_id") REFERENCES "specificity"("id") ON DELETE CASCADE
);

CREATE TABLE "utility" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "value" INTEGER NOT NULL,
  "description" TEXT NOT NULL,
  "example" TEXT,
  "timestamp" TEXT DEFAULT (datetime('now', 'localtime')),
  "note" TEXT
);

CREATE TABLE "rubric_utility" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "rubric_id" INTEGER NOT NULL,
  "utility_id" INTEGER NOT NULL,
  "timestamp" TEXT DEFAULT (datetime('now', 'localtime')),
  FOREIGN KEY ("rubric_id") REFERENCES "rubric"("id") ON DELETE CASCADE,
  FOREIGN KEY ("utility_id") REFERENCES "utility"("id") ON DELETE CASCADE
);

CREATE TABLE "sentiment" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "value" INTEGER NOT NULL,
  "description" TEXT NOT NULL,
  "example" TEXT,
  "timestamp" TEXT DEFAULT (datetime('now', 'localtime')),
  "note" TEXT
);

CREATE TABLE "rubric_sentiment" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "rubric_id" INTEGER NOT NULL,
  "sentiment_id" INTEGER NOT NULL,
  "timestamp" TEXT DEFAULT (datetime('now', 'localtime')),
  FOREIGN KEY ("rubric_id") REFERENCES "rubric"("id") ON DELETE CASCADE,
  FOREIGN KEY ("sentiment_id") REFERENCES "sentiment"("id") ON DELETE CASCADE
);

CREATE TABLE "rule" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "title" TEXT NOT NULL,
  "description" TEXT NOT NULL,
  "timestamp" TEXT DEFAULT (datetime('now', 'localtime')),
  "note" TEXT
);

CREATE TABLE "rubric_rule" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "rubric_id" INTEGER NOT NULL,
  "rule_id" INTEGER NOT NULL,
  "order" INTEGER NOT NULL,
  "timestamp" TEXT DEFAULT (datetime('now', 'localtime')),
  FOREIGN KEY ("rubric_id") REFERENCES "rubric"("id") ON DELETE CASCADE,
  FOREIGN KEY ("rule_id") REFERENCES "rule"("id") ON DELETE CASCADE
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
  "start" INTEGER,
  "end" INTEGER,
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

-- Default rubric seed data (competencies, disambiguation, scores, rules).
-- Replaces the old inst/rubric.md parsing step; update values here directly
-- if the rubric needs to change. IDs are hardcoded, relying on this running
-- against a blank database.

INSERT INTO "competency" ("id", "cID", "name", "description") VALUES
  (1, 1, 'Medical Knowledge',
    'Demonstrate understanding of foundational principles that underlie the medical
sciences and apply this knowledge in care of individuals and populations.
'),
  (2, 2, 'Medical History Taking and Physical Examination',
    'Elicit and synthesize a complete and accurate medical history and perform a
focused or comprehensive physical examination, using information from the
patient and other relevant sources.
'),
  (3, 3, 'Provide Effective Oral and Written Professional Communication',
    'Communicate clinical information effectively, efficiently, and professionally in
oral and written formats, including concise patient presentations on rounds and
well-organized clinical documentation such as initial histories and physicals
and daily progress notes to support patient care.
'),
  (4, 4, 'Clinical Reasoning and Decision Making',
    'Efficiently evaluate patient data and use clinical problem solving to generate and prioritize a differential diagnosis, establish an assessment, and propose a diagnostic and/or therapeutic plan.
'),
  (5, 5, 'Interpersonal and Communication Skills',
    'Form collaborative and trusting relationships with patients, caregivers, staff
and all. Effectively communicate with patients and caregivers to promote shared
decision making.
'),
  (6, 6, 'Scholarly Inquiry and Evidence-Based Medicine Integration',
    'Evaluate, analyze, and apply new and existing knowledge across biomedical,
clinical, population, and data sciences through continuous self-directed
learning and scholarly activity to advance patient care. 
'),
  (7, 7, 'Professionalism',
    'Exemplify compassion, integrity, social responsibility and respect for all
persons and identities. Demonstrate responsible behaviors including
accountability, patient confidentiality and safety, punctuality and the
prioritizing the needs of others while maintaining appropriate self-care.
Demonstrate and embody ethical standards, principles and moral reasoning in all
professional interactions with patients, caregivers, colleagues and society at
large.
'),
  (8, 8, 'Interprofessional and Team-Based Care',
    'Collaborate effectively within interprofessional healthcare teams by
communicating clearly and respectfully with physicians, nurses, staff and other
health professionals to provide coordinated, patient-centered care.
'),
  (9, 9, 'Workplace Skills',
    'Independently manage the patients they are caring for by organizing, 
    prioritizing, and completing clinical tasks such as consults, updates to 
    patients/caregivers, discharge planning, etc.');

INSERT INTO "specificity" ("id", "value", "description", "example") VALUES
  (1, 1,
    'Competency is mentioned only with general or broad qualifier without example to support it',
    'Impressive medical knowledge!'),
  (2, 2,
    'Detail about competency is mentioned without example to support it',
    'Demonstrated a deep understanding of infectious disease'),
  (3, 3,
    'At least one specific example supports the competency',
    'Was able to suggest empirical antibiotic regiments based on the ' ||
    'initial infection and relevant patient characteristics'),
  (4, 4,
    'At least one very detailed example of a specific encounter providing ' ||
    'exceptional support',
    'Identified an opportunity to practice good antibiotic stewardship by ' ||
    'suggesting to switch from Meropenem to Temocillin for a confirmed ' ||
    'Proteus mirabilis catheter infection');

INSERT INTO "utility" ("id", "value", "description", "example") VALUES
  (1, 1,
    'low/not useful: too vague or general to act upon',
    'keep reading about important topics'),
  (2, 2,
    'moderately useful: includes student-specific recommendations for ' ||
    'maintaining or improving performance but recommendations are hard to ' ||
    'act upon',
    'has strong theoretical knowledge, but should practice clinical reasoning'),
  (3, 3,
    'highly useful: student-specific, actionable recommendations for ' ||
    'maintaining or improving performance',
    'review disease management guidelines to help improve clinical ' ||
    'decision making');

INSERT INTO "sentiment" ("id", "value", "description", "example") VALUES
  (1, 1,
    'strongly negative (e.g. red flags)',
    'did not have adequate fund of knowledge'),
  (2, 2,
    'negative (including coded language indicating potential criticism)',
    'I would recommend that she continue practicing how to identify and ' ||
    'prioritize clinically relevant information when documenting (such as ' ||
    'which symptoms patients report that are actually clinically relevant ' ||
    'to the current presentation or not).'),
  (3, 3,
    'Neutral or not enough information to indicate sentiment',
    'Her solid foundation in medical knowledge is evident - Continue reading'),
  (4, 4,
    'positive',
    'Was a pleasure to work with! Strong performance.'),
  (5, 5,
    'strongly positive (e.g. signaling exceptional student)',
    'The level of care taken to review each patient case set her apart ' ||
    'from most other interns I have worked with over the years. ' ||
    'She''s working at the level of an PGY1');

INSERT INTO "rule" ("id", "title", "description") VALUES
  (1, 'Minimum bar',
    'Only include a competency if the evaluation makes a clear, specific ' ||
    'reference to it. Generic phrases that could apply to any competency ' ||
    '— "great attitude", "hard worker", "pleasure to work with" — do not ' ||
    'constitute evidence for any specific competency. When in doubt, leave ' ||
    'it out.'),
  (2, 'One competency per quote',
    'Assign each piece of text to maximum one competency — i.e. the most ' ||
    'specific match. Do not repeat the same text under two competencies. ' ||
    'Sometimes content is repeated (slightly differently worded but same ' ||
    'meaning), in which case you treat this as a single quote. See the ' ||
    'disambiguation section below for details.'),
  (3, 'Verbatim only',
    'Copy the exact text from the evaluation. Do not paraphrase, ' ||
    'summarize, or combine sentences. Multiple spans across the text can ' ||
    'be used if relevant. Include all illustrating examples (as long as ' ||
    'they don''t belong to another competency)'),
  (4, 'Missing is normal',
    'Most clerkship evaluations might only explicitly address 2–4 ' ||
    'competencies. Do not force a match to reach a higher number.'),
  (5, 'Include both positive and negative',
    'The aim is to extract all pieces of text related to a competency ' ||
    'regardless of sentiment (i.e. positive and negative)');

INSERT INTO "rubric" ("id") VALUES (1);

INSERT INTO "rubric_competency" ("rubric_id", "competency_id", "order") VALUES
  (1, 1, 1), (1, 2, 2), (1, 3, 3), (1, 4, 4),
  (1, 5, 5), (1, 6, 6), (1, 7, 7), (1, 8, 8), (1, 9, 9);

INSERT INTO "rubric_specificity" ("rubric_id", "specificity_id") VALUES
  (1, 1), (1, 2), (1, 3), (1, 4);

INSERT INTO "rubric_utility" ("rubric_id", "utility_id") VALUES
  (1, 1), (1, 2), (1, 3);

INSERT INTO "rubric_sentiment" ("rubric_id", "sentiment_id") VALUES
  (1, 1), (1, 2), (1, 3), (1, 4), (1, 5);

INSERT INTO "rubric_rule" ("rubric_id", "rule_id", "order") VALUES
  (1, 1, 1), (1, 2, 2), (1, 3, 3), (1, 4, 4), (1, 5, 5);
