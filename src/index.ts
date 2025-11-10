import * as bindings from './bindings'

let initialized = false

async function ensureInit() {
  if (initialized) return

  // bindings 模块会在导入时自动初始化 WASM
  // 这里只需要标记已初始化
  initialized = true
}

/**
 * 执行 Lamina 代码
 * @param {string} code - Lamina 源代码
 * @returns {Promise<string>} 执行结果
 */
export async function rumina(code: string): Promise<string> {
  await ensureInit()
  return bindings.rumina(code)
}
