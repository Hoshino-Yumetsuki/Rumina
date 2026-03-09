import { spawnSync } from 'node:child_process'

const [, , ...cargoArgs] = process.argv

if (cargoArgs.length === 0) {
  console.error('Usage: node scripts/run-rust-command.js <cargo args...>')
  process.exit(1)
}

const result = spawnSync('cargo', cargoArgs, {
  stdio: 'inherit',
  shell: process.platform === 'win32',
  env: {
    ...process.env,
    RUST_MIN_STACK: process.env.RUST_MIN_STACK ?? '134217728'
  }
})

if (result.error) {
  console.error(result.error.message)
  process.exit(1)
}

process.exit(result.status ?? 1)
