# TASK

You previously reviewed a clinical clerkship evaluation and extracted short
quotes as evidence for specific competencies. Every quote was meant to be
copied **verbatim**, but the ones you are given now are not an exact match to
any span of the evaluation text - almost always because they were lightly
edited: truncated, a name or identifier replaced with a token like
`[name_redact]`, the first letter re-capitalised, a word dropped, or two
fragments joined with `...`.

You are given the full evaluation text, then a numbered list of items. Each
item shows the competency it was filed under and the non-verbatim quote.

For each item, return the **shortest** span of the evaluation text that the
item was derived from - the original of that light edit. Rules:

- Copy it **exactly as it appears in the evaluation**: its capitalisation,
  punctuation, spacing, subject word, and identifiers - never the item's
  version of any of these.
- The item and the evaluation often use a **different subject** for the same
  sentence - `He`, `She`, `They`, `The student`, or a redacted name like
  `[name_redact]`. Return the evaluation's exact subject, whichever it is.
  (Item: `He spent time looking up medications...` -> evaluation may read
  `[name_redact] spent time looking up medications...`; return the latter.)
- Match the item's own extent. Do **not** add leading or trailing words,
  clauses or sentences beyond what the item covers, even to reach a sentence
  boundary - a span may begin or end mid-sentence.
- If the item joins fragments from different parts of the evaluation
  (a `...` in the middle, or text that jumps across a line break or between
  sections), return only the span of the **first** fragment.
- If the item does not correspond to any real span of the evaluation - it
  describes something the evaluation never says - return `null`.

# OUTPUT

Return valid JSON that can be parsed directly: no markdown, no explanation.
Use this exact structure, with exactly one entry per itemId given to you:

{"anchors": [{"itemId": 1, "anchor": "exact text from the evaluation"}, {"itemId": 2, "anchor": null}]}
