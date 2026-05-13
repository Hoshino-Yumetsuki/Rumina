use rumina::RuminaError;
use std::env;
use std::fs;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::process;

const MAGIC: &[u8] = b"\x52\x4D\x50\x4B\x53\x52\x43\x00"; // RMPKSRC\0 (8 bytes)

fn main() {
    if let Some(source) = try_extract_embedded_source() {
        run_embedded_source(&source);
        return;
    }

    run_as_packager();
}

fn try_extract_embedded_source() -> Option<String> {
    let exe_path = env::current_exe().ok()?;
    let mut file = fs::File::open(&exe_path).ok()?;

    let mut content = Vec::new();
    file.read_to_end(&mut content).ok()?;

    let magic_pos = content
        .windows(MAGIC.len())
        .rposition(|window| window == MAGIC)?;

    let length_start = magic_pos + MAGIC.len();
    if content.len() < length_start + 8 {
        return None;
    }

    let mut length_bytes = [0u8; 8];
    length_bytes.copy_from_slice(&content[length_start..length_start + 8]);
    let source_length = u64::from_le_bytes(length_bytes) as usize;

    let source_start = length_start + 8;
    let source_end = source_start + source_length;

    if content.len() < source_end {
        return None;
    }

    let source_data = &content[source_start..source_end];
    String::from_utf8(source_data.to_vec()).ok()
}

fn run_embedded_source(source: &str) {
    #[cfg(not(target_arch = "wasm32"))]
    {
        ctrlc::set_handler(move || {
            process::exit(0);
        })
        .ok();
    }

    match rumina::run_rumina(source) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("Runtime error: {}", e);
            process::exit(1);
        }
    }
}

fn run_as_packager() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        print_usage();
        process::exit(1);
    }

    if args.contains(&"--help".to_string()) || args.contains(&"-h".to_string()) {
        print_usage();
        return;
    }

    let mut config = PackageConfig::default();
    let mut input_file = None;
    let mut output_file = None;

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "--no-optimize" => config.optimize = false,
            "--debug" => config.debug_info = true,
            arg if arg.starts_with("--") => {
                eprintln!("Error: Unknown option '{}'", arg);
                eprintln!("Use --help for usage information");
                process::exit(1);
            }
            arg => {
                if input_file.is_none() {
                    input_file = Some(arg.to_string());
                } else if output_file.is_none() {
                    output_file = Some(arg.to_string());
                } else {
                    eprintln!("Error: Too many arguments");
                    process::exit(1);
                }
            }
        }
        i += 1;
    }

    let input_file = match input_file {
        Some(f) => f,
        None => {
            eprintln!("Error: No input file specified");
            process::exit(1);
        }
    };

    let output_file = output_file.unwrap_or_else(|| {
        let input_stem = std::path::Path::new(&input_file)
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("output");

        #[cfg(target_os = "windows")]
        return format!("{}.exe", input_stem);

        #[cfg(not(target_os = "windows"))]
        return input_stem.to_string();
    });

    if !std::path::Path::new(&input_file).exists() {
        eprintln!("Error: Input file '{}' does not exist", input_file);
        process::exit(1);
    }

    let input_path = Path::new(&input_file);
    if input_path.is_file() && !input_file.ends_with(".lm") {
        eprintln!("Warning: Input file is not a .lm file");
    }

    config.input_file = input_file;
    config.output_file = output_file;

    let packager = Packager::new(config);

    if let Err(e) = packager.package() {
        eprintln!("Packaging failed: {}", e);
        process::exit(1);
    }
}

fn print_usage() {
    println!("Rumina Packager - Package .lm files into standalone executables");
    println!();
    println!("Usage:");
    println!("  rmpack <input.lm|extension-dir> [output]");
    println!();
    println!("Arguments:");
    println!("  <input>      Input Lamina source file or LSR-003 extension directory");
    println!("  [output]     Output executable name (optional)");
    println!();
    println!("Options:");
    println!("  --no-optimize   Disable optimization");
    println!("  --debug         Include debug information");
    println!("  --help, -h      Show this help message");
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ExtensionManifest {
    interface: String,
    entry: String,
}

impl ExtensionManifest {
    fn from_optional_manifest(manifest: Option<&str>) -> Result<Self, RuminaError> {
        let mut extension_manifest = Self::default();
        let Some(manifest) = manifest else {
            return Ok(extension_manifest);
        };

        let value: serde_json::Value = serde_json::from_str(manifest).map_err(|err| {
            RuminaError::runtime(format!("InterfaceBindError: invalid lampm.json: {err}"))
        })?;
        let object = value.as_object().ok_or_else(|| {
            RuminaError::runtime("InterfaceBindError: lampm.json must be a JSON object")
        })?;

        if let Some(interface) = object.get("interface") {
            extension_manifest.interface = interface
                .as_str()
                .ok_or_else(|| {
                    RuminaError::runtime(
                        "InterfaceBindError: lampm.json interface must be a string",
                    )
                })?
                .to_string();
        }

        if let Some(entry) = object.get("entry") {
            extension_manifest.entry = entry
                .as_str()
                .ok_or_else(|| {
                    RuminaError::runtime("InterfaceBindError: lampm.json entry must be a string")
                })?
                .to_string();
        }

        if extension_manifest.interface.is_empty() || extension_manifest.entry.is_empty() {
            return Err(RuminaError::runtime(
                "InterfaceBindError: lampm.json interface and entry must be non-empty",
            ));
        }

        Ok(extension_manifest)
    }
}

impl Default for ExtensionManifest {
    fn default() -> Self {
        Self {
            interface: "lib.lm".to_string(),
            entry: "lsr_init".to_string(),
        }
    }
}

#[derive(Debug, Clone)]
struct PackageConfig {
    input_file: String,
    output_file: String,
    optimize: bool,
    debug_info: bool,
}

impl Default for PackageConfig {
    fn default() -> Self {
        Self {
            input_file: String::new(),
            output_file: String::new(),
            optimize: true,
            debug_info: false,
        }
    }
}

struct Packager {
    config: PackageConfig,
}

impl Packager {
    fn new(config: PackageConfig) -> Self {
        Self { config }
    }

    fn package(&self) -> Result<(), RuminaError> {
        let source_path = self.resolve_input_source()?;
        println!("Reading {} ...", source_path.display());

        let source = fs::read_to_string(&source_path)
            .map_err(|e| RuminaError::runtime(format!("Failed to read input file: {}", e)))?;

        println!("Source code size: {} bytes", source.len());
        println!("Generating executable {} ...", self.config.output_file);

        let exe_path = env::current_exe().map_err(|e| {
            RuminaError::runtime(format!("Failed to get current executable path: {}", e))
        })?;

        let mut rmpack_binary = fs::read(&exe_path)
            .map_err(|e| RuminaError::runtime(format!("Failed to read rmpack binary: {}", e)))?;

        rmpack_binary.extend_from_slice(MAGIC);
        let length = source.len() as u64;
        rmpack_binary.extend_from_slice(&length.to_le_bytes());
        rmpack_binary.extend_from_slice(source.as_bytes());

        fs::write(&self.config.output_file, rmpack_binary)
            .map_err(|e| RuminaError::runtime(format!("Failed to write output file: {}", e)))?;

        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mut perms = fs::metadata(&self.config.output_file)
                .map_err(|e| RuminaError::runtime(format!("Failed to get file metadata: {}", e)))?
                .permissions();
            perms.set_mode(0o755);
            fs::set_permissions(&self.config.output_file, perms).map_err(|e| {
                RuminaError::runtime(format!("Failed to set executable permissions: {}", e))
            })?;
        }

        println!("✓ Packaging completed successfully!");
        Ok(())
    }

    fn resolve_input_source(&self) -> Result<PathBuf, RuminaError> {
        let input_path = Path::new(&self.config.input_file);
        if !input_path.is_dir() {
            return Ok(input_path.to_path_buf());
        }

        let manifest_path = input_path.join("lampm.json");
        let manifest_source = if manifest_path.exists() {
            Some(
                fs::read_to_string(&manifest_path)
                    .map_err(|e| RuminaError::runtime(format!("Failed to read lampm.json: {e}")))?,
            )
        } else {
            None
        };
        let manifest = ExtensionManifest::from_optional_manifest(manifest_source.as_deref())?;
        let interface_path = input_path.join(&manifest.interface);

        if !interface_path.exists() {
            return Err(RuminaError::runtime(format!(
                "InterfaceBindError: extension interface '{}' does not exist",
                interface_path.display()
            )));
        }

        if !interface_path.is_file()
            || interface_path
                .extension()
                .and_then(|extension| extension.to_str())
                != Some("lm")
        {
            return Err(RuminaError::runtime(format!(
                "InterfaceBindError: extension interface '{}' must be a readable .lm file",
                interface_path.display()
            )));
        }

        fs::read_to_string(&interface_path).map_err(|err| {
            RuminaError::runtime(format!(
                "InterfaceBindError: extension interface '{}' must be a readable .lm file: {err}",
                interface_path.display()
            ))
        })?;

        println!("Extension entry symbol: {}", manifest.entry);
        Ok(interface_path)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn unique_temp_dir(name: &str) -> PathBuf {
        let nonce = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        env::temp_dir().join(format!("rmpack-{name}-{}-{nonce}", process::id()))
    }

    #[test]
    fn lsr003_extension_manifest_uses_default_interface_and_entry_when_absent() {
        let manifest = ExtensionManifest::from_optional_manifest(None).unwrap();

        assert_eq!(manifest.interface, "lib.lm");
        assert_eq!(manifest.entry, "lsr_init");
    }

    #[test]
    fn lsr003_extension_interface_must_be_readable_file() {
        let extension_dir = unique_temp_dir("interface-dir");
        let interface_dir = extension_dir.join("iface.lm");
        fs::create_dir_all(&interface_dir).unwrap();
        fs::write(
            extension_dir.join("lampm.json"),
            r#"{"interface":"iface.lm"}"#,
        )
        .unwrap();

        let packager = Packager::new(PackageConfig {
            input_file: extension_dir.display().to_string(),
            output_file: "unused".to_string(),
            ..PackageConfig::default()
        });

        let error = packager.resolve_input_source().unwrap_err().to_string();

        fs::remove_dir_all(&extension_dir).unwrap();
        assert!(error.contains("InterfaceBindError"));
        assert!(error.contains("readable .lm file"));
    }
}
