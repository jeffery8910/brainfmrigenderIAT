"""
QC: Verify that the RT values used in current analyses match the RT values in right.csv.

Context
-------
right.csv is a "paired" wide-format file:
  - 60 rows per subject
  - each row contains 1 Congruent RT and 1 Incongruent RT (total 120 trials)
  - missing RTs are represented as NaN

This script compares (per subject × condition) the multiset of RT values in:
  (A) right.csv
  (B) Thesis_Analysis_Output/analysis_data.csv (trial rows only, event empty)

Outputs
-------
QC/right_rt_set_match_summary.csv
QC/right_rt_set_mismatches.csv
"""

from __future__ import annotations

from collections import Counter
from pathlib import Path
from typing import Iterable

import numpy as np
import pandas as pd

BASE = Path(__file__).parent
RIGHT_FILE = BASE / "right.csv"
PROC_FILE = BASE.parent.parent / "Thesis_Analysis_Output" / "analysis_data.csv"

OUT_DIR = BASE / "QC"
SUMMARY_FILE = OUT_DIR / "right_rt_set_match_summary.csv"
MISMATCH_FILE = OUT_DIR / "right_rt_set_mismatches.csv"

ROUND_DIGITS = 7
TOL = 1e-6


def _as_float_series(s: pd.Series) -> pd.Series:
    return pd.to_numeric(s, errors="coerce")


def _rounded_counter(values: Iterable[float], ndigits: int) -> Counter:
    return Counter([round(float(v), ndigits) for v in values if np.isfinite(v)])


def _diff_counter(a: Counter, b: Counter) -> list[tuple[float, int]]:
    """Return a-b as a list of (value, count)."""
    d = a - b
    return sorted(d.items(), key=lambda x: (-x[1], x[0]))


def main() -> None:
    if not RIGHT_FILE.exists():
        raise FileNotFoundError(f"Missing: {RIGHT_FILE}")
    if not PROC_FILE.exists():
        raise FileNotFoundError(f"Missing: {PROC_FILE}")

    right = pd.read_csv(RIGHT_FILE)

    required_cols = {"Subject", "Condition 1", "Reaction Time", "Condition 2", "Reaction Time.1"}
    missing_cols = required_cols - set(right.columns)
    if missing_cols:
        raise ValueError(f"right.csv missing columns: {sorted(missing_cols)}")

    # Sanity check: Conditions should be constant (typical format)
    cond1_set = set(right["Condition 1"].dropna().unique().tolist())
    cond2_set = set(right["Condition 2"].dropna().unique().tolist())
    if not (cond1_set == {"Congruent"} and cond2_set == {"Incongruent"}):
        # Still proceed but print a warning (do not fail hard)
        print("[WARN] right.csv condition labels are not the expected singletons:")
        print("  Condition 1 unique:", sorted(cond1_set))
        print("  Condition 2 unique:", sorted(cond2_set))

    right = right.assign(
        rt_cong=_as_float_series(right["Reaction Time"]),
        rt_incong=_as_float_series(right["Reaction Time.1"]),
    )

    proc = pd.read_csv(PROC_FILE)
    proc = proc.assign(
        rt=_as_float_series(proc.get("rt")),
        condition=proc.get("condition"),
    )

    # Keep only trial rows (event empty or NA)
    if "event" in proc.columns:
        proc = proc[(proc["event"].isna()) | (proc["event"] == "")]

    # Build per-subject per-condition RT lists from processed data
    proc_rt = (
        proc[["subject", "condition", "rt"]]
        .rename(columns={"subject": "Subject", "condition": "Condition"})
        .copy()
    )

    # Ensure we only consider Congruent/Incongruent
    proc_rt = proc_rt[proc_rt["Condition"].isin(["Congruent", "Incongruent"])]

    subjects = sorted(set(right["Subject"].unique()).union(set(proc_rt["Subject"].unique())))

    summary_rows: list[dict] = []
    mismatch_rows: list[dict] = []

    for subj in subjects:
        r_sub = right[right["Subject"] == subj]
        p_sub = proc_rt[proc_rt["Subject"] == subj]

        for cond, r_col in [("Congruent", "rt_cong"), ("Incongruent", "rt_incong")]:
            r_vals = _as_float_series(r_sub[r_col]) if (r_sub is not None and len(r_sub) > 0) else pd.Series([], dtype=float)
            p_vals = _as_float_series(p_sub[p_sub["Condition"] == cond]["rt"]) if (p_sub is not None and len(p_sub) > 0) else pd.Series([], dtype=float)

            r_n = int(r_vals.shape[0])
            p_n = int(p_vals.shape[0])
            r_miss = int(r_vals.isna().sum())
            p_miss = int(p_vals.isna().sum())

            r_non = r_vals.dropna().to_numpy(dtype=float)
            p_non = p_vals.dropna().to_numpy(dtype=float)

            max_abs_diff = np.nan
            status = "OK"

            if r_n == 0 or p_n == 0:
                status = "FAIL"
            else:
                # Compare multisets after rounding
                cnt_r = _rounded_counter(r_non, ROUND_DIGITS)
                cnt_p = _rounded_counter(p_non, ROUND_DIGITS)

                missing_in_proc = _diff_counter(cnt_r, cnt_p)
                extra_in_proc = _diff_counter(cnt_p, cnt_r)

                # Also compute max abs diff after sorting (same-length only)
                if len(r_non) == len(p_non) and len(r_non) > 0:
                    max_abs_diff = float(np.max(np.abs(np.sort(r_non) - np.sort(p_non))))
                elif len(r_non) == 0 and len(p_non) == 0:
                    max_abs_diff = 0.0

                if missing_in_proc or extra_in_proc or (np.isfinite(max_abs_diff) and max_abs_diff > TOL) or (r_miss != p_miss) or (r_n != p_n):
                    status = "FAIL"
                    for v, c in missing_in_proc[:50]:
                        mismatch_rows.append(
                            {
                                "Subject": subj,
                                "Condition": cond,
                                "Type": "missing_in_processed",
                                "Value": v,
                                "Count": c,
                            }
                        )
                    for v, c in extra_in_proc[:50]:
                        mismatch_rows.append(
                            {
                                "Subject": subj,
                                "Condition": cond,
                                "Type": "extra_in_processed",
                                "Value": v,
                                "Count": c,
                            }
                        )

            summary_rows.append(
                {
                    "Subject": subj,
                    "Condition": cond,
                    "n_right": r_n,
                    "n_processed": p_n,
                    "missing_right": r_miss,
                    "missing_processed": p_miss,
                    "n_nonmissing_right": int(len(r_non)),
                    "n_nonmissing_processed": int(len(p_non)),
                    "max_abs_diff": max_abs_diff,
                    "status": status,
                }
            )

    OUT_DIR.mkdir(exist_ok=True)
    summary_df = pd.DataFrame(summary_rows).sort_values(["status", "Subject", "Condition"], ascending=[True, True, True])
    if mismatch_rows:
        mismatch_df = pd.DataFrame(mismatch_rows).sort_values(["Subject", "Condition", "Type"])
    else:
        mismatch_df = pd.DataFrame(columns=["Subject", "Condition", "Type", "Value", "Count"])

    summary_df.to_csv(SUMMARY_FILE, index=False, encoding="utf-8")
    mismatch_df.to_csv(MISMATCH_FILE, index=False, encoding="utf-8")

    n_fail = int((summary_df["status"] == "FAIL").sum())
    print("Saved:", SUMMARY_FILE)
    print("Saved:", MISMATCH_FILE)
    print(f"Status: FAIL rows={n_fail} / {len(summary_df)} (subject×condition)")
    if n_fail == 0:
        print("[OK] right.csv RT sets match analysis_data.csv (trial RT).")


if __name__ == "__main__":
    main()
