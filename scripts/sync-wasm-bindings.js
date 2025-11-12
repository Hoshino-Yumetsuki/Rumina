import { readFileSync, writeFileSync } from 'node:fs'
import { resolve, dirname } from 'node:path'
import { fileURLToPath } from 'node:url'

const __filename = fileURLToPath(import.meta.url)
const __dirname = dirname(__filename)

const wasmBgJsPath = resolve(__dirname, '../lib/rumina_bg.js')
const wasmBinaryPath = resolve(__dirname, '../lib/rumina_bg.wasm')
const outputPath = resolve(__dirname, '../crates/rumina/src/bindings.ts')

console.log('正在从 lib 同步 wasm 绑定代码...')

try {
  let wasmBgJs = readFileSync(wasmBgJsPath, 'utf-8')
  const wasmBinary = readFileSync(wasmBinaryPath)
  const wasmBase64 = wasmBinary.toString('base64')

  wasmBgJs = wasmBgJs.replace(/^import .* from .*?;?\s*$/gm, '')

  wasmBgJs = wasmBgJs.replace(/^export function /gm, 'function ')
  wasmBgJs = wasmBgJs.replace(/^export const /gm, 'const ')

  const exportedFunctions = []
  const functionRegex = /^function (\w+)\s*\(/gm
  for (
    let match = functionRegex.exec(wasmBgJs);
    match !== null;
    match = functionRegex.exec(wasmBgJs)
  ) {
    exportedFunctions.push(match[1])
  }

  const header = `// @ts-nocheck
// Auto-generated from lib/rumina_bg.js and lib/rumina_bg.wasm
// Do not edit manually

// Inlined WASM binary (base64 encoded)
const wasmBase64 = '${wasmBase64}'

`

  const initCode = `
// Decode and instantiate WASM module
const wasmBytes = Uint8Array.from(atob(wasmBase64), c => c.charCodeAt(0))
const wasmModule = new WebAssembly.Module(wasmBytes)

// Build imports object
const imports = {
  './rumina_bg.js': {
    __wbg_getTime_14776bfb48a1bff9,
    __wbg_getTimezoneOffset_d391cb11d54969f8,
    __wbg_new_93d9417ed3fb115d,
    __wbg_new_0_f9740686d739025c,
    __wbg_getRandomValues_1c61fac11405ffdc,
    __wbg___wbindgen_throw_b855445ff6a94295,
    __wbindgen_init_externref_table,
    __wbindgen_cast_d6cd19b81560fd6e
  }
}

const wasmInstance = new WebAssembly.Instance(wasmModule, imports)
wasm = wasmInstance.exports

// Start WASM
if (wasm.__wbindgen_start) {
  wasm.__wbindgen_start()
}
`

  const exportStatements =
    exportedFunctions.length > 0
      ? `\nexport { ${exportedFunctions.join(', ')} }`
      : ''

  const finalCode = header + wasmBgJs + initCode + exportStatements

  writeFileSync(outputPath, finalCode, 'utf-8')

  console.log('✓ 成功同步 wasm 绑定代码到:', outputPath)
  console.log(`  导出的函数: ${exportedFunctions.join(', ')}`)
} catch (error) {
  console.error('✗ 同步失败:', error.message)
  console.error(error.stack)
  process.exit(1)
}
