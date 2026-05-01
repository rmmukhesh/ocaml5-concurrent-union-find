"""
plot_bench_time.py — visualise union-find benchmark results with time on y-axis.

Produces three separate PNG files:
  bench_trend_20_time.png   — trend benchmark, 20% union mix
  bench_trend_50_time.png   — trend benchmark, 50% union mix
  bench_graph_time.png      — graph-structure benchmark (CAS only)
  bench_connectivity_time.png — connectivity benchmark (CAS only)

Usage:
    python plot_bench_time.py \
        --trend  bench_trend_results.csv \
        --graph  bench_graph_structure_results.csv \
        --connectivity bench_connectivity_results.csv \
        --outdir .
"""

import argparse
import sys
from pathlib import Path
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import numpy as np
import pandas as pd

# ---------------------------------------------------------------------------
# Aesthetics
# ---------------------------------------------------------------------------
IMPL_COLORS = {
    "cas_union_find":       "#e05a2b",
    "mutex_union_find":     "#3a7ebf",
    "node_lock_union_find": "#2dab6f",
}
IMPL_LABELS = {
    "cas_union_find":       "CAS",
    "mutex_union_find":     "Global mutex",
    "node_lock_union_find": "Node lock",
}
IMPL_MARKERS = {
    "cas_union_find":       "o",
    "mutex_union_find":     "s",
    "node_lock_union_find": "^",
}

PROFILE_COLORS = {
    "sparse_random": "#7b5ea7",
    "dense_random":  "#e0993a",
    "star":          "#d94f4f",
    "grid":          "#2dab6f",
    "chain":         "#3a7ebf",
    "hot_set":       "#d94f4f",
}
PROFILE_LABELS = {
    "sparse_random": "Sparse random (low contention)",
    "dense_random":  "Dense random (medium contention)",
    "star":          "Star / hot-set (high contention)",
    "grid":          "Grid",
    "chain":         "Chain",
    "hot_set":       "Star / hot-set (high contention)",
}
PROFILE_MARKERS = {
    "sparse_random": "o",
    "dense_random":  "s",
    "star":          "^",
    "grid":          "D",
    "chain":         "v",
    "hot_set":       "^",
}

BACKGROUND  = "#f7f5f0"
GRID_COLOR  = "#d8d4cc"
SPINE_COLOR = "#aaa49a"


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def make_figure(title):
    fig, ax = plt.subplots(figsize=(8, 5))
    fig.patch.set_facecolor(BACKGROUND)
    ax.set_facecolor(BACKGROUND)
    ax.set_title(title, fontsize=12, fontweight="bold", pad=12)
    ax.set_xlabel("Domains", fontsize=10)
    ax.set_ylabel("Time (seconds)", fontsize=10)
    ax.yaxis.set_major_formatter(ticker.FormatStrFormatter("%.2fs"))
    ax.grid(axis="y", color=GRID_COLOR, linewidth=0.8, zorder=0)
    ax.grid(axis="x", color=GRID_COLOR, linewidth=0.4, linestyle=":", zorder=0)
    for spine in ax.spines.values():
        spine.set_edgecolor(SPINE_COLOR)
        spine.set_linewidth(0.8)
    ax.tick_params(labelsize=9)
    return fig, ax


def plot_lines(ax, df, key_col, color_map, label_map, marker_map):
    all_y = []
    for key in df[key_col].unique():
        sub = df[df[key_col] == key].sort_values("domains")
        x      = sub["domains"].values
        y      = sub["median_seconds"].values
        # error bars: distance from median to min/max
        err_lo = y - sub["min_seconds"].values   # positive: median above min
        err_hi = sub["max_seconds"].values - y   # positive: max above median
        all_y.extend(y)

        ax.errorbar(
            x, y,
            yerr=[err_lo, err_hi],
            label=label_map.get(key, key),
            color=color_map.get(key, "#888888"),
            marker=marker_map.get(key, "o"),
            markersize=7,
            linewidth=2,
            capsize=4,
            capthick=1.3,
            elinewidth=1,
            zorder=3,
        )

    ax.set_xticks(sorted(df["domains"].unique()))

    # zoom y-axis tight around actual data
    y_min = min(all_y)
    y_max = max(all_y)
    margin = (y_max - y_min) * 0.08
    ax.set_ylim(max(0, y_min - margin), y_max + margin)

    ax.legend(fontsize=8.5, framealpha=0.88, edgecolor=SPINE_COLOR)


def save(fig, path):
    fig.tight_layout(pad=1.8)
    fig.savefig(path, dpi=150, bbox_inches="tight", facecolor=BACKGROUND)
    plt.close(fig)
    print(f"saved: {Path(path).resolve()}")


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawTextHelpFormatter)
    parser.add_argument("--trend",  required=True,
                        help="Path to bench_trend_results.csv")
    parser.add_argument("--graph",  required=True,
                        help="Path to bench_graph_structure_results.csv")
    parser.add_argument("--connectivity", required=True,
                        help="Path to bench_connectivity_results.csv")
    parser.add_argument("--outdir", default=".",
                        help="Directory for output PNGs (default: current dir)")
    args = parser.parse_args()

    trend_path        = Path(args.trend)
    graph_path        = Path(args.graph)
    connectivity_path = Path(args.connectivity)
    outdir            = Path(args.outdir)
    outdir.mkdir(parents=True, exist_ok=True)

    for p in (trend_path, graph_path, connectivity_path):
        if not p.exists():
            print(f"error: file not found: {p}", file=sys.stderr)
            sys.exit(1)

    trend        = pd.read_csv(trend_path)
    graph        = pd.read_csv(graph_path)
    connectivity = pd.read_csv(connectivity_path)

    trend20 = trend[trend["union_percent"] == 20].copy()
    trend50 = trend[trend["union_percent"] == 50].copy()

    fig, ax = make_figure("Time vs Domains — 20% union mix\n(3 implementations)")
    plot_lines(ax, trend20, "implementation", IMPL_COLORS, IMPL_LABELS, IMPL_MARKERS)
    save(fig, outdir / "bench_trend_20_time.png")

    fig, ax = make_figure("Time vs Domains — 50% union mix\n(3 implementations)")
    plot_lines(ax, trend50, "implementation", IMPL_COLORS, IMPL_LABELS, IMPL_MARKERS)
    save(fig, outdir / "bench_trend_50_time.png")

    fig, ax = make_figure("Time vs Domains — graph structure\n(CAS only, 3 graph profiles)")
    plot_lines(ax, graph, "graph_profile", PROFILE_COLORS, PROFILE_LABELS, PROFILE_MARKERS)
    save(fig, outdir / "bench_graph_time.png")

    fig, ax = make_figure("Time vs Domains — connectivity\n(CAS only, 5 graph profiles)")
    plot_lines(ax, connectivity, "graph_profile", PROFILE_COLORS, PROFILE_LABELS, PROFILE_MARKERS)
    save(fig, outdir / "bench_connectivity_time.png")


if __name__ == "__main__":
    main()
