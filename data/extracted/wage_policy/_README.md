# `data/extracted/wage_policy/`

One CSV per jurisdiction, named with the lowercase code (e.g.
`nsw.csv`). The CSV schema is documented in
[`R/extract/extract_wage_policy.R`](../../R/extract/extract_wage_policy.R).

Wage-policy extraction is Phase 5. Each row captures one wage-policy
decision announced in a Budget Paper or MYEFO: the percentage rise,
who it covers, productivity offsets, sign-on bonuses, and verbatim
policy text for the methodology page.

LLM-assisted extraction is the practical approach (the policy
statements are free text); the dev-machine workflow is documented in
[`R/extract/extract_llm.R`](../../R/extract/extract_llm.R).
