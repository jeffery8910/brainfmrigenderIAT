"""
Review rules for behavioral logs (按鍵/RT 品質檢查).

輸入:
  Thesis_Analysis_Output/analysis_data.csv  (含練習、正式、程序 event)

輸出:
  Statistical_Analysis_Implementation/review_issues.csv  (逐列問題)
  在終端列印摘要 (每位受試者)

檢查規則 (trial 列 = event 為 NaN):
  - condition 缺失或不在 {Congruent, Incongruent}
  - acc 缺失或不在 {0,1}
  - rt 缺失
  - rt < 0.05 或 rt > 5  (極端)
  - rt 不在分析窗 [0.2, 1.5] (警告)
  - (subject, phase, trial_global) 重複
  - 每人 Congruent / Incongruent 試次應各 60 (若非，列出)
  - 每人練習試次應 20、正式 100；程序 event 應 8 (若非，列出)

檢查規則 (event 列 = event 非 NaN):
  - 若 rt 或 acc 有值，視為異常
"""

from __future__ import annotations

from pathlib import Path
import pandas as pd

BASE = Path(__file__).parent  # .../最新的資料/Behavior
DATA_ROOT = BASE.parent.parent  # .../data
SRC = DATA_ROOT / "Thesis_Analysis_Output" / "analysis_data.csv"
OUT_DIR = BASE / "QC"
OUT = OUT_DIR / "review_issues.csv"

RT_SOFT_MIN = 0.2
RT_SOFT_MAX = 1.5
RT_HARD_MIN = 0.05
RT_HARD_MAX = 5.0


def main() -> None:
    df = pd.read_csv(SRC)
    # Normalise numeric types
    for c in ["phase", "trial_global", "trial_block", "rt", "acc"]:
        if c in df.columns:
            df[c] = pd.to_numeric(df[c], errors="coerce")

    issues = []

    # Trial rows: event is NA or empty string (CSV empty cells are often "")
    trial_mask = df["event"].isna() | (df["event"] == "")

    # Duplicate (subject, phase, trial_global)
    dup_mask = df[trial_mask].duplicated(subset=["subject", "phase", "trial_global"], keep=False)
    dup_rows = df[trial_mask & dup_mask]
    for _, r in dup_rows.iterrows():
        issues.append(_issue(r, "duplicate_trial_global", "同一 subject-phase trial_global 重複"))

    for idx, row in df.iterrows():
        is_event = (not trial_mask.iloc[idx])

        if is_event:
            if pd.notna(row.get("rt")):
                issues.append(_issue(row, "event_has_rt", f"event 行含 rt={row['rt']}"))
            if pd.notna(row.get("acc")):
                issues.append(_issue(row, "event_has_acc", f"event 行含 acc={row['acc']}"))
            continue

        # trial rows
        if pd.isna(row.get("condition")):
            issues.append(_issue(row, "missing_condition", "condition 缺失"))
        elif row["condition"] not in {"Congruent", "Incongruent"}:
            issues.append(_issue(row, "invalid_condition", f"condition={row['condition']}"))

        if pd.isna(row.get("acc")):
            issues.append(_issue(row, "missing_acc", "acc 缺失"))
        elif row["acc"] not in {0, 1}:
            issues.append(_issue(row, "invalid_acc", f"acc={row['acc']}"))

        if pd.isna(row.get("rt")):
            issues.append(_issue(row, "missing_rt", "rt 缺失"))
        else:
            if row["rt"] < RT_HARD_MIN or row["rt"] > RT_HARD_MAX:
                issues.append(_issue(row, "rt_extreme", f"rt={row['rt']:.3f}"))
            elif row["rt"] < RT_SOFT_MIN or row["rt"] > RT_SOFT_MAX:
                issues.append(_issue(row, "rt_out_of_window", f"rt={row['rt']:.3f}"))

    issues_df = pd.DataFrame(issues)
    OUT_DIR.mkdir(exist_ok=True)
    issues_df.to_csv(OUT, index=False, encoding="utf-8")

    print(f"Issues saved to: {OUT}")
    print(f"總列數: {len(df)}, 事件列: {(~trial_mask).sum()}, 試驗列: {trial_mask.sum()}")
    print(f"發現問題列數: {len(issues_df)}")

    # Per-subject summaries
    subj_summary(df, trial_mask)


def subj_summary(df: pd.DataFrame, trial_mask: pd.Series) -> None:
    trial_df = df[trial_mask].copy()
    # counts by block_type
    practice = trial_df[trial_df["block_type"].str.contains("練習", na=False)]
    formal = trial_df[trial_df["block_type"].str.contains("正式", na=False)]
    events = df[~trial_mask]

    print("\n每人試次數 (練習/正式/事件):")
    merged = (
        pd.DataFrame({
            "practice": practice.groupby("subject").size(),
            "formal": formal.groupby("subject").size(),
            "event": events.groupby("subject").size(),
        })
        .fillna(0)
        .astype(int)
    )
    print(merged)

    # condition balance
    cond = trial_df.groupby(["subject", "condition"]).size().unstack(fill_value=0)
    print("\n每人 Condition 分布:")
    print(cond)


def _issue(row, code: str, detail: str) -> dict:
    return {
        "subject": row["subject"],
        "phase": row.get("phase"),
        "trial_global": row.get("trial_global"),
        "block_type": row.get("block_type"),
        "condition": row.get("condition"),
        "event": row.get("event"),
        "rt": row.get("rt"),
        "acc": row.get("acc"),
        "issue": code,
        "detail": detail,
    }


if __name__ == "__main__":
    main()
