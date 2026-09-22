# Fixture notes

This fixture was generated from the legacy `data/vqa-demo1/main_001_current`
workflow using the procedure in `tests/refactor/README.md` and seed `20260810`.

## Reviewed adjustment: land-cover summaries

After generation, four rows with `all.n.OK == FALSE` were removed manually
from both `results/lc.summary.csv` and
`results/lc.summary.detailed.csv`:

- `BF/BS Forest, Mature [Disturbed/Logged]`
- `BF/BS Forest, Old [Disturbed/Logged]`
- `Scrub [Disturbed/Logged]`
- `Wetland [Disturbed/Logged]`

The legacy workflow creates these reports before selecting eligible records
for downstream analysis. The four rows describe data excluded by the
minimum-sample rule and are not present in
`data/vqa-demo1-min/main_001_current`. The adjusted fixture therefore defines
the report reproducible from the committed minimal test input: its five
remaining `all.n.OK == TRUE` land-cover classes.

The legacy detailed report ordered `Reclaimed` before `Established Rec` using
ordering inherited from raw import data. That ordering is not represented in
the minimal schema. This fixture instead uses the stable row order of the
minimal `inputs/landCover.csv`, in which `Established Rec` precedes
`Reclaimed`.
