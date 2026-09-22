# Fixture notes

This fixture is generated from the legacy
`data/vqa-demo1/offset_001_current` workflow using the procedure in
`tests/refactor/README.md` and seed `20260810`.

## Reviewed adjustment: land-cover summaries

After refreshing this fixture, remove these six `all.n.OK == FALSE` rows from
both `results/lc.summary.csv` and `results/lc.summary.detailed.csv`:

- `BF/BS Forest, Early-Mid`
- `BF/BS Forest, Mature [Disturbed/Logged]`
- `BF/BS Forest, Old [Disturbed/Logged]`
- `Floodplain - Active Channel`
- `Scrub [Disturbed/Logged]`
- `WB Forest, Early-Mid [Disturbed/Logged]`

The legacy import writes these reports before its minimum-sample selection.
Those excluded classes are not in the corresponding minimal test input, so
the adjusted fixture defines the summaries reproducible from that input.
