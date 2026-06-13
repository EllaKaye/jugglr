## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new release.

## Notes for the reviewers

* The `animate()` examples are wrapped in `\dontrun{}` because they contact an
  external server (the JugglingLab animation server at <https://jugglinglab.org/>)
  and open a GIF in the viewer or browser. They cannot be run non-interactively
  or without a network connection.

* There are no published references describing the methods in this package. It
  implements established, well-documented juggling siteswap notation and
  validity rules.
