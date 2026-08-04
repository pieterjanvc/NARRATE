# TASK

Analyze the quality of a clinical clerkship evaluation of a medical student
below and identify which of the listed competencies are explicitly addressed by
the evaluator and how how good the quality of each description is.

# RULES

{rules}

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
