"""
Compare raw RT and processed RT per trial, and flag mismatches.

Input:
- rawdata/S*_fMRI_*.csv   (subject ids S004/S006/S007/S008/S010)
- Thesis_Analysis_Output/analysis_data.csv  (subject ids remapped S001~S005)

Output:
- QC/rt_keypress_mismatch.csv
  Columns:
    subject, phase, trial_global, block_type_raw, block_type_proc,
    condition_raw, condition_proc, resp_raw, acc_raw, acc_proc,
    onset_abs_raw, keypress_abs_raw, rt_raw, rt_proc, diff_ms,
    mismatch_reason
"""
from __future__ import annotations

import pandas as pd
from pathlib import Path

BASE = Path(__file__).parent
RAW_DIR = BASE / "rawdata"
PROC_FILE = BASE.parent.parent / "Thesis_Analysis_Output" / "analysis_data.csv"
OUT_DIR = BASE / "QC"
OUT_FILE = OUT_DIR / "rt_keypress_mismatch.csv"

SUBJECT_MAP = {
    "S004": "S001",
    "S006": "S002",
    "S007": "S003",
    "S008": "S004",
    "S010": "S005",
}

def load_raw():
    rows = []
    for csv_path in RAW_DIR.glob("S*_fMRI_*.csv"):
        df = pd.read_csv(csv_path)
        subj_orig = csv_path.stem.split("_")[0]
        subj = SUBJECT_MAP.get(subj_orig, subj_orig)
        df["subject"] = subj
        df["phase"] = pd.to_numeric(df.get("phase"), errors="coerce")
        df["trial_global"] = pd.to_numeric(df.get("trial_global"), errors="coerce")
        # keep only trial rows (event empty)
        trials = df[df["event"].isna() | (df["event"] == "")]
        trials = trials.assign(
            rt_raw=pd.to_numeric(trials["rt"], errors="coerce"),
            acc_raw=pd.to_numeric(trials.get("acc"), errors="coerce"),
            onset_abs_raw=pd.to_numeric(trials.get("onset_abs"), errors="coerce"),
            resp_raw=trials.get("resp"),
            block_type_raw=trials.get("block_type"),
            condition_raw=trials.get("condition"),
        )
        trials["keypress_abs_raw"] = trials["onset_abs_raw"] + trials["rt_raw"]
        rows.append(trials[[
            "subject", "phase", "trial_global",
            "block_type_raw", "condition_raw",
            "resp_raw", "acc_raw",
            "onset_abs_raw", "keypress_abs_raw",
            "rt_raw",
        ]])
    return pd.concat(rows, ignore_index=True)

def load_proc():
    df = pd.read_csv(PROC_FILE)
    # keep only trial rows (event empty)
    df = df[df["event"].isna() | (df["event"] == "")]
    df = df.assign(
        rt_proc=pd.to_numeric(df["rt"], errors="coerce"),
        acc_proc=pd.to_numeric(df.get("acc"), errors="coerce"),
        block_type_proc=df.get("block_type"),
        condition_proc=df.get("condition"),
    )
    return df[[
        "subject", "phase", "trial_global",
        "block_type_proc", "condition_proc",
        "acc_proc", "rt_proc",
    ]]

def main():
    raw = load_raw()
    proc = load_proc()
    merged = raw.merge(proc, on=["subject", "phase", "trial_global"], how="outer", indicator=True)

    def reason(row):
        if row["_merge"] == "left_only":
            return "missing_in_processed"
        if row["_merge"] == "right_only":
            return "missing_in_raw"
        # both
        diffs = []
        if row.get("condition_raw") != row.get("condition_proc"):
            diffs.append("condition_mismatch")

        rt_raw = row.get("rt_raw")
        rt_proc = row.get("rt_proc")
        if pd.isna(rt_raw) and not pd.isna(rt_proc):
            diffs.append("rt_missing_raw")
        elif not pd.isna(rt_raw) and pd.isna(rt_proc):
            diffs.append("rt_missing_proc")
        elif not pd.isna(rt_raw) and not pd.isna(rt_proc) and abs(rt_raw - rt_proc) * 1000 > 5:
            diffs.append("rt_diff>5ms")

        acc_raw = row.get("acc_raw")
        acc_proc = row.get("acc_proc")
        if pd.isna(acc_raw) and not pd.isna(acc_proc):
            diffs.append("acc_missing_raw")
        elif not pd.isna(acc_raw) and pd.isna(acc_proc):
            diffs.append("acc_missing_proc")
        elif not pd.isna(acc_raw) and not pd.isna(acc_proc) and int(acc_raw) != int(acc_proc):
            diffs.append("acc_mismatch")

        return ";".join(diffs) if diffs else ""

    merged["diff_ms"] = (merged["rt_raw"] - merged["rt_proc"]) * 1000
    merged["mismatch_reason"] = merged.apply(reason, axis=1)
    merged.sort_values(["subject", "phase", "trial_global"], inplace=True)

    OUT_DIR.mkdir(exist_ok=True)
    merged.to_csv(OUT_FILE, index=False, encoding="utf-8")

    summary = (
        merged.assign(flag=merged["mismatch_reason"] != "")
        .groupby("subject")["flag"]
        .agg(total="size", mismatch="sum")
    )
    print("Saved:", OUT_FILE)
    print(summary)

if __name__ == "__main__":
    main()
