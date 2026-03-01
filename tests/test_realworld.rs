//! Real-world project tests for the syn → ruast → Display round-trip pipeline.
//!
//! These tests clone popular Rust OSS projects and verify that every `.rs` file
//! can be parsed by `syn`, converted to `ruast::Crate`, rendered via `Display`,
//! and re-parsed by `syn` without errors.
//!
//! Run all (with regression check):
//!   cargo test -p ruast --test test_realworld --features syn -- --ignored test_realworld_all --nocapture
//!
//! Run a single project:
//!   cargo test -p ruast --test test_realworld --features syn -- --ignored test_realworld_anyhow --nocapture

#[cfg(feature = "syn")]
mod realworld {
    use std::collections::BTreeMap;
    use std::fmt;
    use std::fs;
    use std::path::{Path, PathBuf};
    use std::process::Command;

    // =====================================================================
    // Project definitions
    // =====================================================================

    struct Project {
        name: &'static str,
        repo_url: &'static str,
    }

    const PROJECTS: &[Project] = &[
        Project {
            name: "serde",
            repo_url: "https://github.com/serde-rs/serde",
        },
        Project {
            name: "anyhow",
            repo_url: "https://github.com/dtolnay/anyhow",
        },
        Project {
            name: "thiserror",
            repo_url: "https://github.com/dtolnay/thiserror",
        },
        Project {
            name: "once_cell",
            repo_url: "https://github.com/matklad/once_cell",
        },
        Project {
            name: "itertools",
            repo_url: "https://github.com/rust-itertools/itertools",
        },
        Project {
            name: "regex",
            repo_url: "https://github.com/rust-lang/regex",
        },
        Project {
            name: "clap",
            repo_url: "https://github.com/clap-rs/clap",
        },
        Project {
            name: "tokio",
            repo_url: "https://github.com/tokio-rs/tokio",
        },
    ];

    // =====================================================================
    // Cache directory
    // =====================================================================

    fn cache_dir() -> PathBuf {
        if let Ok(dir) = std::env::var("RUAST_REALWORLD_DIR") {
            PathBuf::from(dir)
        } else {
            PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("target/realworld-repos")
        }
    }

    // =====================================================================
    // Git clone
    // =====================================================================

    fn clone_project(project: &Project) -> PathBuf {
        let base = cache_dir();
        fs::create_dir_all(&base).expect("failed to create cache directory");

        let project_dir = base.join(project.name);
        if project_dir.exists() {
            eprintln!(
                "  [{}] Using cached clone at {}",
                project.name,
                project_dir.display()
            );
            return project_dir;
        }

        eprintln!("  [{}] Cloning {} ...", project.name, project.repo_url);
        let status = Command::new("git")
            .args([
                "clone",
                "--depth",
                "1",
                "--single-branch",
                project.repo_url,
                &project_dir.to_string_lossy(),
            ])
            .status()
            .expect("failed to execute git");

        assert!(
            status.success(),
            "git clone failed for {} (exit code: {:?})",
            project.name,
            status.code()
        );

        project_dir
    }

    // =====================================================================
    // File collection (recursive walk with std::fs only)
    // =====================================================================

    fn collect_rs_files(dir: &Path, files: &mut Vec<PathBuf>) {
        let entries = match fs::read_dir(dir) {
            Ok(entries) => entries,
            Err(_) => return,
        };

        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                let name = path.file_name().unwrap_or_default().to_string_lossy();
                // Skip directories that are not Rust source
                if name == "target" || name == ".git" || name == "vendor" {
                    continue;
                }
                collect_rs_files(&path, files);
            } else if path.extension().is_some_and(|ext| ext == "rs") {
                files.push(path);
            }
        }
    }

    // =====================================================================
    // Round-trip test per file
    // =====================================================================

    #[derive(Debug)]
    enum FileResult {
        /// syn could not parse the original file (skip — not our concern)
        SynParseFailed,
        /// Round-trip succeeded: Display output re-parses successfully
        Success,
        /// Conversion or re-parse failed
        Failed(String),
    }

    fn test_file(path: &Path) -> FileResult {
        let source = match fs::read_to_string(path) {
            Ok(s) => s,
            Err(e) => return FileResult::Failed(format!("read error: {e}")),
        };

        // Step 1: Parse with syn. If syn itself can't parse it, skip.
        let syn_file = match syn::parse_file(&source) {
            Ok(f) => f,
            Err(_) => return FileResult::SynParseFailed,
        };

        // Step 2: Convert syn::File → ruast::Crate (catch panics from unimplemented conversions)
        let krate = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            ruast::Crate::from(syn_file)
        })) {
            Ok(k) => k,
            Err(e) => {
                let msg = if let Some(s) = e.downcast_ref::<&str>() {
                    s.to_string()
                } else if let Some(s) = e.downcast_ref::<String>() {
                    s.clone()
                } else {
                    "unknown panic".to_string()
                };
                return FileResult::Failed(format!("conversion panic: {msg}"));
            }
        };

        // Step 3: Render via Display
        let output = krate.to_string();

        // Step 4: Re-parse the Display output with syn
        match syn::parse_file(&output) {
            Ok(_) => FileResult::Success,
            Err(e) => FileResult::Failed(format!("re-parse failed: {e}\n--- output ---\n{output}")),
        }
    }

    // =====================================================================
    // Per-project results
    // =====================================================================

    struct ProjectResults {
        name: String,
        total_files: usize,
        syn_skipped: usize,
        succeeded: usize,
        failures: Vec<(PathBuf, String)>,
    }

    impl fmt::Display for ProjectResults {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(
                f,
                "[{}] {} files total, {} syn-skipped, {} succeeded, {} failed",
                self.name,
                self.total_files,
                self.syn_skipped,
                self.succeeded,
                self.failures.len(),
            )
        }
    }

    // =====================================================================
    // Test a single project
    // =====================================================================

    fn test_project(project: &Project) -> ProjectResults {
        let project_dir = clone_project(project);

        let mut rs_files = Vec::new();
        collect_rs_files(&project_dir, &mut rs_files);
        rs_files.sort();

        let mut results = ProjectResults {
            name: project.name.to_string(),
            total_files: rs_files.len(),
            syn_skipped: 0,
            succeeded: 0,
            failures: Vec::new(),
        };

        for file in &rs_files {
            match test_file(file) {
                FileResult::SynParseFailed => results.syn_skipped += 1,
                FileResult::Success => results.succeeded += 1,
                FileResult::Failed(e) => {
                    let rel = file.strip_prefix(&project_dir).unwrap_or(file);
                    results.failures.push((rel.to_path_buf(), e));
                }
            }
        }

        eprintln!("  {results}");
        results
    }

    fn run_single_project(name: &str) {
        let project = PROJECTS
            .iter()
            .find(|p| p.name == name)
            .unwrap_or_else(|| panic!("unknown project: {name}"));

        let results = test_project(project);

        if !results.failures.is_empty() {
            eprintln!("\n  Failures in {name}:");
            for (path, err) in &results.failures {
                eprintln!("    FAIL: {}", path.display());
                // Print first 5 lines of the error to keep output manageable
                for line in err.lines().take(5) {
                    eprintln!("      {line}");
                }
            }
            panic!(
                "{}: {} / {} files failed round-trip",
                name,
                results.failures.len(),
                results.total_files
            );
        }
    }

    // =====================================================================
    // Baseline comparison
    // =====================================================================

    /// Per-project entry in baseline / results JSON.
    #[derive(Debug, Clone)]
    struct ProjectEntry {
        passed: usize,
        total: usize,
    }

    /// Minimal JSON parser for baseline file (avoids serde_json dependency).
    /// Expected format: `{ "name": { "passed": N, "total": M }, ... }`
    fn parse_baseline(json: &str) -> BTreeMap<String, ProjectEntry> {
        let mut map = BTreeMap::new();
        // Strip outer braces and iterate over key-value pairs
        let inner = json.trim().trim_start_matches('{').trim_end_matches('}');
        for entry in inner.split("},") {
            let entry = entry.trim().trim_end_matches('}');
            // Parse "name": { "passed": N, "total": M
            let Some((key_part, val_part)) = entry.split_once(':') else {
                continue;
            };
            let name = key_part.trim().trim_matches('"').to_string();
            if name.is_empty() {
                continue;
            }
            let val_part = val_part.trim().trim_start_matches('{');
            let mut passed = 0;
            let mut total = 0;
            for field in val_part.split(',') {
                let field = field.trim();
                if let Some((k, v)) = field.split_once(':') {
                    let k = k.trim().trim_matches('"');
                    let v = v.trim().trim_matches('"');
                    match k {
                        "passed" => passed = v.parse().unwrap_or(0),
                        "total" => total = v.parse().unwrap_or(0),
                        _ => {}
                    }
                }
            }
            map.insert(name, ProjectEntry { passed, total });
        }
        map
    }

    fn write_results_json(
        results: &BTreeMap<String, ProjectEntry>,
        path: &Path,
    ) -> std::io::Result<()> {
        let mut json = String::from("{\n");
        let entries: Vec<_> = results.iter().collect();
        for (i, (name, entry)) in entries.iter().enumerate() {
            json.push_str(&format!(
                "  \"{}\": {{ \"passed\": {}, \"total\": {} }}",
                name, entry.passed, entry.total
            ));
            if i + 1 < entries.len() {
                json.push(',');
            }
            json.push('\n');
        }
        json.push_str("}\n");
        fs::write(path, json)
    }

    /// Compare current results against baseline, return list of regressions.
    fn check_regressions(
        baseline: &BTreeMap<String, ProjectEntry>,
        current: &BTreeMap<String, ProjectEntry>,
    ) -> Vec<String> {
        let mut regressions = Vec::new();
        for (name, cur) in current {
            if let Some(base) = baseline.get(name) {
                if cur.passed < base.passed {
                    regressions.push(format!(
                        "{}: passed {} -> {} (regression of -{})",
                        name,
                        base.passed,
                        cur.passed,
                        base.passed - cur.passed,
                    ));
                }
            }
        }
        regressions
    }

    fn print_comparison_table(
        baseline: &BTreeMap<String, ProjectEntry>,
        current: &BTreeMap<String, ProjectEntry>,
    ) {
        eprintln!();
        eprintln!(
            "  {:<12} {:>10}   {:>10}   {:>6}",
            "Project", "Baseline", "Current", "Delta"
        );
        eprintln!("  {}", "-".repeat(48));

        let mut total_base_passed = 0usize;
        let mut total_base_total = 0usize;
        let mut total_cur_passed = 0usize;
        let mut total_cur_total = 0usize;

        for (name, cur) in current {
            let base = baseline.get(name);
            let (bp, bt) = base.map_or((0, 0), |b| (b.passed, b.total));
            let delta = cur.passed as i64 - bp as i64;
            let delta_str = if delta > 0 {
                format!("+{delta}")
            } else if delta < 0 {
                format!("{delta}")
            } else {
                "0".to_string()
            };
            let marker = if delta < 0 { " REGRESSION" } else { "" };
            eprintln!(
                "  {:<12} {:>4}/{:<4}    {:>4}/{:<4}   {:>6}{}",
                name, bp, bt, cur.passed, cur.total, delta_str, marker
            );

            total_base_passed += bp;
            total_base_total += bt;
            total_cur_passed += cur.passed;
            total_cur_total += cur.total;
        }

        eprintln!("  {}", "-".repeat(48));
        let total_delta = total_cur_passed as i64 - total_base_passed as i64;
        let total_delta_str = if total_delta > 0 {
            format!("+{total_delta}")
        } else if total_delta < 0 {
            format!("{total_delta}")
        } else {
            "0".to_string()
        };
        eprintln!(
            "  {:<12} {:>4}/{:<4}    {:>4}/{:<4}   {:>6}",
            "TOTAL",
            total_base_passed,
            total_base_total,
            total_cur_passed,
            total_cur_total,
            total_delta_str
        );
        eprintln!();
    }

    /// Generate a Markdown comparison table for CI job summaries.
    fn generate_markdown_report(
        baseline: &BTreeMap<String, ProjectEntry>,
        current: &BTreeMap<String, ProjectEntry>,
        regressions: &[String],
    ) -> String {
        let mut md = String::new();

        md.push_str("## Real-world Round-trip Test Results\n\n");
        md.push_str("| Project | Baseline | Current | Delta |\n");
        md.push_str("|---------|----------|---------|-------|\n");

        let mut total_bp = 0usize;
        let mut total_bt = 0usize;
        let mut total_cp = 0usize;
        let mut total_ct = 0usize;

        for (name, cur) in current {
            let base = baseline.get(name);
            let (bp, bt) = base.map_or((0, 0), |b| (b.passed, b.total));
            let delta = cur.passed as i64 - bp as i64;
            let delta_str = if delta > 0 {
                format!("+{delta}")
            } else if delta < 0 {
                format!("**{delta}**")
            } else {
                "0".to_string()
            };
            md.push_str(&format!(
                "| {} | {}/{} | {}/{} | {} |\n",
                name, bp, bt, cur.passed, cur.total, delta_str
            ));
            total_bp += bp;
            total_bt += bt;
            total_cp += cur.passed;
            total_ct += cur.total;
        }

        let total_delta = total_cp as i64 - total_bp as i64;
        let total_delta_str = if total_delta > 0 {
            format!("+{total_delta}")
        } else if total_delta < 0 {
            format!("**{total_delta}**")
        } else {
            "0".to_string()
        };
        md.push_str(&format!(
            "| **TOTAL** | **{}/{}** | **{}/{}** | **{}** |\n",
            total_bp, total_bt, total_cp, total_ct, total_delta_str
        ));

        md.push('\n');
        if regressions.is_empty() {
            md.push_str("No regressions detected.\n");
        } else {
            md.push_str("### Regressions detected\n\n");
            for r in regressions {
                md.push_str(&format!("- {r}\n"));
            }
        }

        md
    }

    // =====================================================================
    // Individual project tests
    // =====================================================================

    #[test]
    #[ignore]
    fn test_realworld_serde() {
        run_single_project("serde");
    }

    #[test]
    #[ignore]
    fn test_realworld_anyhow() {
        run_single_project("anyhow");
    }

    #[test]
    #[ignore]
    fn test_realworld_thiserror() {
        run_single_project("thiserror");
    }

    #[test]
    #[ignore]
    fn test_realworld_once_cell() {
        run_single_project("once_cell");
    }

    #[test]
    #[ignore]
    fn test_realworld_itertools() {
        run_single_project("itertools");
    }

    #[test]
    #[ignore]
    fn test_realworld_regex() {
        run_single_project("regex");
    }

    #[test]
    #[ignore]
    fn test_realworld_clap() {
        run_single_project("clap");
    }

    #[test]
    #[ignore]
    fn test_realworld_tokio() {
        run_single_project("tokio");
    }

    // =====================================================================
    // All projects at once — with regression check
    // =====================================================================

    #[test]
    #[ignore]
    fn test_realworld_all() {
        eprintln!("\n=== Real-world round-trip tests ===\n");

        // Run all projects and collect results
        let mut current: BTreeMap<String, ProjectEntry> = BTreeMap::new();
        let mut all_failures: Vec<(String, PathBuf, String)> = Vec::new();

        for project in PROJECTS {
            let results = test_project(project);
            current.insert(
                results.name.clone(),
                ProjectEntry {
                    passed: results.succeeded,
                    total: results.total_files,
                },
            );
            for (path, err) in results.failures {
                all_failures.push((results.name.clone(), path, err));
            }
        }

        // Write current results to JSON
        let results_path =
            PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("target/realworld-results.json");
        if let Err(e) = write_results_json(&current, &results_path) {
            eprintln!("  Warning: could not write results JSON: {e}");
        } else {
            eprintln!("  Results written to {}", results_path.display());
        }

        // Load baseline
        let baseline_path =
            PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/realworld_baseline.json");
        let baseline = match fs::read_to_string(&baseline_path) {
            Ok(json) => parse_baseline(&json),
            Err(e) => {
                eprintln!(
                    "  Warning: could not read baseline ({}): {e}",
                    baseline_path.display()
                );
                BTreeMap::new()
            }
        };

        // Print comparison table
        print_comparison_table(&baseline, &current);

        // Check for regressions
        let regressions = check_regressions(&baseline, &current);

        // Generate Markdown report for CI
        let md = generate_markdown_report(&baseline, &current, &regressions);
        let report_path =
            PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("target/realworld-report.md");
        if let Err(e) = fs::write(&report_path, &md) {
            eprintln!("  Warning: could not write Markdown report: {e}");
        }

        // Print failures for debugging
        if !all_failures.is_empty() {
            eprintln!("=== Failures ({}) ===", all_failures.len());
            for (project, path, err) in &all_failures {
                eprintln!("  [{project}] {}", path.display());
                for line in err.lines().take(3) {
                    eprintln!("    {line}");
                }
            }
        }

        // Fail ONLY on regression (not on absolute failures)
        if !regressions.is_empty() {
            eprintln!("\n=== REGRESSION DETECTED ===");
            for r in &regressions {
                eprintln!("  {r}");
            }
            panic!(
                "Regression detected: {} project(s) have fewer passing files than baseline",
                regressions.len()
            );
        }

        let total_passed: usize = current.values().map(|e| e.passed).sum();
        let total_files: usize = current.values().map(|e| e.total).sum();
        eprintln!("=== OK: {total_passed}/{total_files} files passed, no regressions ===");
    }
}
