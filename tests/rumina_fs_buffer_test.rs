use rumina::{Value, run_rumina_with_dir};
use std::fs;

#[test]
fn test_rumina_buffer_basic_ops() {
    let code = r#"
include "rumina:buffer";
var b = Buffer.alloc(5);
b.set(0, 72);
b.set(1, 101);
b.set(2, 108);
b.set(3, 108);
b.set(4, 111);
b.toText();
"#;

    let result = run_rumina_with_dir(code, None).expect("execution failed");
    match result {
        Some(Value::String(s)) => assert_eq!(s, "Hello"),
        other => panic!("Expected Some(String), got {:?}", other),
    }
}

#[test]
fn test_rumina_fs_text_and_bytes() {
    let temp_dir = std::env::temp_dir().join("rumina_fs_buffer_test");
    fs::create_dir_all(&temp_dir).expect("create temp dir failed");

    let text_path = temp_dir.join("a.txt");
    let bin_path = temp_dir.join("b.bin");
    let text_path_str = text_path.to_string_lossy().replace('\\', "\\\\");
    let bin_path_str = bin_path.to_string_lossy().replace('\\', "\\\\");

    let code = format!(
        r#"
include "rumina:buffer";
include "rumina:fs";

fs.writeText("{text_path}", "Alpha");
fs.append("{text_path}", "Beta");

var ok1 = fs.exists("{text_path}");
var t = fs.readText("{text_path}");

var buf = Buffer.alloc(3);
buf.set(0, 65);
buf.set(1, 66);
buf.set(2, 67);
fs.writeBytes("{bin_path}", buf);

var r = fs.readBytes("{bin_path}");
var g0 = r.get(0);
var g1 = r.get(1);
var g2 = r.get(2);

if (ok1 && t == "AlphaBeta" && g0 == 65 && g1 == 66 && g2 == 67) {{
    "ok";
}} else {{
    "bad";
}}
"#,
        text_path = text_path_str,
        bin_path = bin_path_str
    );

    let result = run_rumina_with_dir(&code, Some(temp_dir.to_string_lossy().to_string()))
        .expect("execution failed");
    match result {
        Some(Value::String(s)) => assert_eq!(s, "ok"),
        other => panic!("Expected Some(String), got {:?}", other),
    }

    fs::remove_dir_all(&temp_dir).ok();
}

#[test]
fn test_rumina_buffer_extended_apis() {
    let code = r#"
include "rumina:buffer";

var a = Buffer.from("48656c6c6f", "hex");
var b = Buffer.from("SGk=", "base64");
var c = Buffer.from("abc");
var u = Buffer.from("SGk", "base64url");
var h2 = Buffer.from("48656c6c6fzz", "hex");

var d = Buffer.alloc(6);
d.fill(120); // 'x'
var copied = c.copy(d, 1, 0, 3);
var merged = Buffer.concat([a, b]);

var idx = d.indexOf("abc");
var has = d.includes("bc");
var idx2 = a.indexOf("6c", 0, "hex");
var idx3 = a.indexOf("bG8=", -2, "base64");
var has2 = a.includes("bG8=", 3, "base64");
var eq = a.equals(Buffer.from("Hello"));
var cmp = a.compare(Buffer.from("Hellp"));
var sub = a.subarray(1, -1);

if (
  a.toText() == "Hello" &&
  b.toText() == "Hi" &&
  c.toHex() == "616263" &&
  b.toBase64() == "SGk=" &&
  b.toBase64Url() == "SGk" &&
  u.toText() == "Hi" &&
  h2.toText() == "Hello" &&
  copied == 3 &&
  merged.toText() == "HelloHi" &&
  idx == 1 &&
  has &&
  idx2 == 2 &&
  idx3 == 3 &&
  has2 &&
  eq &&
  cmp == -1 &&
  sub.toText() == "ell"
) {
  "ok";
} else {
  "bad";
}
"#;

    let result = run_rumina_with_dir(code, None).expect("execution failed");
    match result {
        Some(Value::String(s)) => assert_eq!(s, "ok"),
        other => panic!("Expected Some(String), got {:?}", other),
    }
}

#[test]
fn test_rumina_fs_write_options_flag() {
    let temp_dir = std::env::temp_dir().join("rumina_fs_write_flag_test");
    fs::create_dir_all(&temp_dir).expect("create temp dir failed");
    let text_path = temp_dir.join("flag.txt");
    let text_path_str = text_path.to_string_lossy().replace('\\', "\\\\");

    let code = format!(
        r#"
include "rumina:fs";

fs.writeText("{text_path}", "A", {{ flag = "w" }});
fs.writeText("{text_path}", "B", {{ flag = "a" }});
var t = fs.readText("{text_path}");
var h = fs.readText("{text_path}", "hex");
var b64 = fs.readText("{text_path}", {{ encoding = "base64" }});

if (t == "AB" && h == "4142" && b64 == "QUI=") {{ "ok"; }} else {{ "bad"; }}
"#,
        text_path = text_path_str
    );

    let result = run_rumina_with_dir(&code, Some(temp_dir.to_string_lossy().to_string()))
        .expect("execution failed");
    match result {
        Some(Value::String(s)) => assert_eq!(s, "ok"),
        other => panic!("Expected Some(String), got {:?}", other),
    }

    fs::remove_dir_all(&temp_dir).ok();
}
