# TASK

You will be given pieces of text extracted from a student evaluation written by
a clinician after a hospital internship. Each piece will support one of the
competencies outlined below. Your task is to score the support for each detected
competency based on the quality of the text evidence using the rubric. Not all
competencies are present.

# COMPETENCIES

{competencies}

# SCORES

## Per competency score

Each competency gets a specificity score that denotes the quality of the
supporting text evidence as provided by the evaluator.

### specificity

{specificity}

_Note that the content can be different for each competency or internship, the
focus is on the quality of the evidence_

## Global scores

These scores take all the text evidence into account are are only provided once
for the whole review.

### utility:

Utility score is indicating how effective the evaluation would be if rewritten
to serve as feedback for the student.

{utility}

### sentiment

Evaluator sentiment score for the overall review

{sentiment}

# TO RETURN

Only score competencies for which there is text evidence provided, ignore the
rest.

Return valid JSON only — no markdown, no explanation. Use this exact structure:

{"competencies": [{"cIndex": 1, "specificity": 2}], "utility": 2, "sentiment": 4 }
