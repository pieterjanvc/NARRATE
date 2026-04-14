# TASK

You will be given pieces of text extracted from a student evaluation written by
a clinician after a hospital internship. Each piece will support one of the
competencies outlined below. Your task is to score the support for each detected
competency based on the quality of the text evidence using the rubric. Not all
competencies are present.

# COMPETENCIES

### 1. Medical Knowledge

Demonstrate understanding of foundational principles that underlie the medical
sciences and apply this knowledge in care of individuals and populations.
Generate an appropriate differential diagnosis.

### 2. Medical History Taking and Physical Examination

Elicit and synthesize a complete and accurate medical history and perform a
focused or comprehensive physical examination, using information from the
patient and other relevant sources.

### 3. Provide Effective Oral and Written Professional Communication

Communicate clinical information effectively, efficiently, and professionally in
oral and written formats, including concise patient presentations on rounds and
well-organized clinical documentation such as initial histories and physicals
and daily progress notes to support patient care.

### 4. Clinical Reasoning and Decision Making

Efficiently evaluate patient data and use clinical problem solving to prioritize
a differential diagnosis and establish an assessment and plan.

### 5. Interpersonal and Communication Skills

Form collaborative and trusting relationships with patients, caregivers, staff
and all. Effectively communicate with patients and caregivers to promote shared
decision making.

### 6. Scholarly Inquiry and Evidence-Based Medicine Integration

Evaluate, analyze, and apply new and existing knowledge across biomedical,
clinical, population, and data sciences through continuous self-directed
learning and scholarly activity to advance patient care.

### 7. Professionalism

Exemplify compassion, integrity, social responsibility and respect for all
persons and identities. Demonstrate responsible behaviors including
accountability, patient confidentiality and safety, punctuality and the
prioritizing of the needs of others while maintaining appropriate self-care.
Demonstrate and embody ethical standards, principles and moral reasoning in all
professional interactions with patients, caregivers, colleagues and society at
large.

### 8. Interprofessional and Team-Based Care

Collaborate effectively within interprofessional healthcare teams by
communicating clearly and respectfully with physicians, nurses, staff and other
health professionals to provide coordinated, patient-centered care.

# SCORES

## Per competency score

Each competency gets a specificity score that denotes the quality of the
supporting text evidence as provided by the evaluator.

### specificity

- 1: Competency is briefly mentioned and its description mostly contains general
  qualifiers (nice, great, amazing, terrible, wonderful, …)
- 2: Non-specific evidence is given without clear examples to support it
- 3: At least one specific example supports the competency
- 4: Multiple or detailed examples providing exceptional detail and support

**guiding examples**

- Score of 1: Impressive medical knowledge!
- Score of 2: Demonstrated a deep understanding of infectious disease
- Score of 3: Was able to suggest empirical antibiotic regiments based on the
  initial infection and relevant patient characteristics
- Score of 4: Identified an opportunity to practice good antibiotic stewardship
  by suggesting to switch from Meropenem to Temocillin for a confirmed Proteus
  mirabilis catheter infection

_Note that the content can be different for each competency or internship, the
focus is on the quality of the evidence_

## Global scores

These scores take all the text evidence into account are are only provided once
for the whole review.

### utility:

Utility score is indicating how effective the evaluation would be if rewritten
to serve as feedback for the student.

- 1: low/not useful: Uses 3rd person, minimal specific information, often vague
- 2: moderately useful: Specific to the student but hard to act upon
- 3: highly useful: Very specific and directly applicable to professional
  improvement

**guiding examples**

- Score of 1: keep reading about important topics
- Score of 2: has strong theoretical knowledge, but should practice clinical
  reasoning
- Score of 3: review disease management guidelines to help improve clinical
  decision making

### sentiment

Evaluator sentiment score for the overall review

- 1: clearly negative or red flags
- 2: slightly negative or coded language indicating potential criticism
- 3: Not enough information to indicate sentiment
- 4: generic positive language (e.g. empty praise)
- 5: specific positive language indicating the reviewer's effort to make this
  particular student stand out

**guiding examples**

- Score of 1: did not respond well to feedback
- Score of 2: should review disease management guidelines to help improve
  clinical decision making
- Score of 3: _No example possible_
- Score of 4: Was wonderful to work with!
- Score of 5: The level of care taken to review each patient case set her apart
  from most other interns I have worked with over the years

# TO RETURN

Only score competencies for which there is text evidence provided, ignore the
rest.

Return valid JSON only — no markdown, no explanation. Use this exact structure:

{"competencies": [{"cID": 1, "specificity": 2}], "utility": 2, "sentiment": 4 }
