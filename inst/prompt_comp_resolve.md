# TASK

You previously extracted competency evidence from a clinical clerkship
evaluation, but some of the quotes you returned overlap: the same span of text
(or a longer quote and a shorter quote nested inside it) was assigned to more
than one competency. Each span of evidence must belong to exactly one
competency - the single most specific match.

You are given, for context only, every quote currently extracted for this
review (grouped by competency), followed by a numbered list of conflicts. For
each conflict, decide which single competency the overlapping evidence belongs
to, or discard it entirely (cIndex 0) if it does not clearly belong to any of
the listed options. Only the overlapping span is reassigned - the non-
overlapping remainder of a longer quote stays with its competency, so prefer
the option whose competency the shared text is genuinely about, and use the
context to avoid stripping a competency of its only evidence when it isn't
necessary.

Judge the shared span on its own, not the sentence around it: if a broad quote
merely contains a shorter clause that more specifically supports another
competency, assign that clause to the more specific competency and let the
broad quote keep the rest. Only give the whole shared span to the broad quote
when the shorter clause genuinely is not distinct evidence for its competency.

{disambiguation_section}

# COMPETENCIES

{competencies}

# OUTPUT

Return valid JSON that can be parsed directly, so no markdown, no explanation.
Use this exact structure:

{"resolutions": [{"conflictId": 1, "cIndex": 2}]}

Return exactly one resolution per conflictId listed below. Use the cIndex of
the option you are keeping, or cIndex 0 to discard the overlapping evidence
entirely (assign it to none of the listed options).
