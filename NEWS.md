# ctrecurrent 0.1.3

- Added `R-CMD-check` GitHub Actions workflow to run tests and `R CMD check` on push/PR across Ubuntu, Windows, and macOS.

# ctrecurrent 0.1.2

- Added `time_unit` argument to `ct_to_recurrent()` to support time scales other than days (e.g. `"hours"`, `"mins"`, `"weeks"`). `survey_duration` and the resulting `t.start`/`t.stop` columns are interpreted in the chosen unit (#8).
- Added input validation via `checkmate` assertions to `ct_to_recurrent()` (#6).
- Added tests for `time_unit` scaling consistency and input validation.
- Fixed clipped x-axis labels in vignette two-panel plot (increased `fig.width`).
- Suppressed package startup messages in vignette.
