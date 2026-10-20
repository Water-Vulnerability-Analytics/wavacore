# wavacore

Shared building blocks for the Water Vulnerability Analytics (WaVA) water
supply vulnerability assessments of National Park Service (NPS) units. Both the
rapid (nationwide) and deep-dive assessments use these functions.

| Function | What it does |
| --- | --- |
| `get_wsd()` | Loads and cleans the NPS water supply database, with optional filters by park and geography |
| `getParkSystem()` | Downloads the NPS park boundary shapefile |
| `getParkBoundary()` | Gets the boundary of one or more park units |
| `getUnitVisitation()` | Gets monthly visitation statistics for park units |

## Install

```r
# install.packages("remotes")
remotes::install_github("Water-Vulnerability-Analytics/wavacore")
```

## Data

`get_wsd()` reads the water supply database from the shared WaVA data folder,
which is not part of this repository. For now it looks for
`data/Water_Supply_Systems/NPS_Water_Systems_Database_Joined.xlsx` relative to
your working directory, so run it from a project that has the `data/` link to
the shared folder.

## Related packages

- [`wavarapid`](https://github.com/Water-Vulnerability-Analytics/wavarapid):
  rapid assessment indicators (planned)
- [`wavadeepdive`](https://github.com/Water-Vulnerability-Analytics/wavadeepdive):
  deep-dive data retrieval and analysis (planned)

## AI Use Disclosure

AI-assisted coding tools (Claude Code, using Anthropic's Claude models) were used
during development of this repository, beginning in 2026, to help set up the
package, prepare it for release, and draft documentation. All AI-assisted code
and text was reviewed, tested, and validated by the authors, who made all design
and scientific decisions and remain responsible for the content.

## License

MIT. See [LICENSE.md](LICENSE.md).
