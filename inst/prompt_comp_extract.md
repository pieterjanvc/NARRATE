# TASK

Analyze the quality of a clinical clerkship evaluation of a medical student
below and identify which of the listed competencies are explicitly addressed by
the evaluator and how how good the quality of each description is.

# RULES

1. **Minimum bar**: Only include a competency if the evaluation makes a clear,
   specific reference to it. Generic phrases that could apply to any competency
   — "great attitude", "hard worker", "pleasure to work with" — do not
   constitute evidence for any specific competency. When in doubt, leave it out.

2. **One competency per quote**: Assign each piece of text to maximum one
   competency — i.e. the most specific match. Do not repeat the same text under
   two competencies. Sometimes content is repeated (slightly differently worded
   but same meaning), in which case you treat this as a single quote. See the
   disambiguation section below for details.

3. **Verbatim only**: Copy the exact text from the evaluation. Do not
   paraphrase, summarize, or combine sentences. Multiple spans across the text
   can be used if relevant. Include all illustrating examples (as long as they
   don't belong to another competency)

4. **Missing is normal**: Most clerkship evaluations might only explicitly
   address 2–4 competencies. Do not force a match to reach a higher number.

5. **Include both positive and negative**: The aim is to extract all pieces of
   text related to a competency regardless of sentiment (i.e. positive and
   negative)

# DISAMBIGUATION

When text could fit more than one competency, apply these rules to assign it to
a single one:

{disambiguation}

# COMPETENCIES

{competencies}

# OUTPUT

Return valid JSON that can be parsed directly, so no markdown, no explanation.
Use this exact structure:

{"extractions": [{"cIndex": 1, "text": ["verbatim span 1", "verbatim span 2"]}]}

Only include competencies that were found. An empty extractions array is valid.
cIndex needs to be unique (i.e. no multiple entries for the same competency)
