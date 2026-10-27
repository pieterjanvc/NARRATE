# Analysis notes

## To do: AI repeatability (test–retest)

The ICC non-inferiority test in `ai-eval.qmd` shows how well the AI agrees with
humans, but not how consistent the AI is with itself. Since the plan is to let
the AI do all the scoring at scale, this matters: LLM output varies between runs.

- Re-score some or all of the 60 shared formative evaluations with the AI
  (same model, prompt and rubric version) one or more extra times.
- Compute the AI's ICC with itself (ICC(2,1), absolute agreement) on quality
  and sentiment scores.
- Compare it with the mean human–human ICC. If AI–AI is clearly higher, that
  directly supports the claim that the AI is more consistent than human raters.
