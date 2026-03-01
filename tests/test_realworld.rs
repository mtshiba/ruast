//! Real-world project tests for the syn → ruast → Display round-trip pipeline.
//!
//! These tests clone popular Rust OSS projects and verify that every `.rs` file
//! can be parsed by `syn`, converted to `ruast::Crate`, rendered via `Display`,
//! and re-parsed by `syn` without errors.
//!
//! Run all:
//!   cargo test -p ruast --test test_realworld --features syn -- --ignored --nocapture
//!
//! Run a single project:
//!   cargo test -p ruast --test test_realworld --features syn -- --ignored test_realworld_anyhow --nocapture

#[cfg(feature = "syn")]
mod realworld {
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
    // All projects at once
    // =====================================================================

    #[test]
    #[ignore]
    fn test_realworld_all() {
        eprintln!("\n=== Real-world round-trip tests ===\n");

        let mut all_failures: Vec<(String, PathBuf, String)> = Vec::new();
        let mut total_files = 0;
        let mut total_succeeded = 0;
        let mut total_skipped = 0;

        for project in PROJECTS {
            let results = test_project(project);
            total_files += results.total_files;
            total_succeeded += results.succeeded;
            total_skipped += results.syn_skipped;
            for (path, err) in results.failures {
                all_failures.push((results.name.clone(), path, err));
            }
        }

        eprintln!("\n=== Summary ===");
        eprintln!(
            "  Total: {} files, {} succeeded, {} syn-skipped, {} failed",
            total_files,
            total_succeeded,
            total_skipped,
            all_failures.len()
        );

        if !all_failures.is_empty() {
            eprintln!("\n=== Failures ===");
            for (project, path, err) in &all_failures {
                eprintln!("  [{project}] {}", path.display());
                for line in err.lines().take(3) {
                    eprintln!("    {line}");
                }
            }
            panic!(
                "{} / {} files failed round-trip across {} projects",
                all_failures.len(),
                total_files,
                PROJECTS.len()
            );
        }
    }
}
