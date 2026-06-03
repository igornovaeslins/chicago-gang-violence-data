# On the use of AI tools

The research design, hypotheses, and interpretation in this paper are my own. I
formulated the question, specified the empirical strategy, and made every
analytical and methodological decision reported here. Between January and June
2026 I worked through Claude Code, Anthropic's agentic coding tool, which I
directed to plan tasks and to write and refactor the R code, build the
replication package, organize the data, and cross-check the reported figures
against the underlying records. I moved to each new release of Anthropic's
Claude Opus as it shipped. Opus 4.5 at the start of 2026, Opus 4.6 from late
February, Opus 4.7 from mid-April, and Opus 4.8 from late May. I set every task,
reviewed the output, and take full responsibility for the code, the data, the
results, and the text.

## Reproducibility

The full pipeline is written in R and runs offline from public sources, the
Chicago Data Portal and the Census Bureau's American Community Survey. The
replication package ships with portable paths, a data dictionary, and a
verification script that recomputes every headline statistic from the data and
checks it against its reference value. All 32 checks pass. Victim-name fields
were removed from the incident records, and the analysis operates at the
community-area × year level.
