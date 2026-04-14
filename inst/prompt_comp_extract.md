# TASK

Analyze the clinical clerkship evaluation of a medical student below and
identify which of the 8 competencies are explicitly addressed. For each one
found, extract the verbatim text that justifies its inclusion. Make sure to
include illustrating examples are these will be important later.

# RULES

1. **Minimum bar**: Only include a competency if the evaluation makes a clear,
   specific reference to it. Generic phrases that could apply to any competency
   — "great attitude", "hard worker", "pleasure to work with" — do not
   constitute evidence for any specific competency. When in doubt, leave it out.

2. **Verbatim only**: Copy the exact text from the evaluation. Do not
   paraphrase, summarize, or combine sentences. Multiple spans across the text
   can be used if relevant. Don't forget to include all illustrating examples.

3. **One competency per quote**: Assign each piece of text to maximum one
   competency — i.e. the most specific match. Do not repeat the same text under
   two competencies.

4. **Missing is normal**: Most clerkship evaluations might only explicitly
   address 2–4 competencies. Do not force a match to reach a higher number.

5. **Include both positive and negative**: The aim is to extract all pieces of
   text related to a competency regardless of sentiment (i.e. positive and
   negative)

# DISAMBIGUATION

When text could fit more than one competency, apply these rules:

- **Comp 3 vs. 5**: Comp 3 = quality and format of clinical documentation and
  formal oral presentations (notes, H&Ps, SOAP notes, rounds presentations).
  Comp 5 = direct relationship and communication with patients and families
  during encounters.

- **Comp 1 vs. 4**: Comp 1 = demonstrating factual or conceptual knowledge, or
  generating a differential diagnosis. Comp 4 = applying knowledge to reason
  through a specific patient's problem and arrive at a management plan.

- **Comp 5 vs. 8**: Comp 5 = interactions with patients and caregivers only.
  Comp 8 = collaboration with physicians, nurses, and other health
  professionals.

- **Comp 7 vs. others**: Only assign to Comp 7 for explicitly professional
  conduct — ethics, reliability, patient safety, punctuality, accountability. Do
  not use Comp 7 as a catch-all for positive character traits.

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

# OUTPUT

Return valid JSON only — no markdown, no explanation. Use this exact structure:

{"extractions": [{"cID": 1, "text": ["verbatim span 1", "verbatim span 2"]}]}

Only include competencies that were found. An empty extractions array is valid.
