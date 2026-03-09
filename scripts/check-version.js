import { readFileSync } from 'node:fs'
import { dirname, resolve } from 'node:path'
import { fileURLToPath } from 'node:url'

const __filename = fileURLToPath(import.meta.url)
const __dirname = dirname(__filename)

const ROOT_DIR = resolve(__dirname, '..')
const CARGO_TOML_PATH = resolve(ROOT_DIR, 'Cargo.toml')
const PACKAGE_JSON_PATH = resolve(ROOT_DIR, 'package.json')

export function extractCargoWorkspaceVersion(cargoTomlContent) {
  const workspacePackageMatch = cargoTomlContent.match(
    /\[workspace\.package\][\s\S]*?^version\s*=\s*"([^"]+)"/m
  )

  if (!workspacePackageMatch) {
    throw new Error('Cargo.toml is missing a workspace package version')
  }

  return workspacePackageMatch[1]
}

export function extractPackageJsonVersion(packageJsonContent) {
  return JSON.parse(packageJsonContent).version
}

export function findVersionMismatches({ cargoVersion, packageVersion }) {
  const mismatches = []

  if (cargoVersion !== packageVersion) {
    mismatches.push({
      source: 'package.json',
      expected: cargoVersion,
      actual: packageVersion
    })
  }

  return mismatches
}

export function checkVersionParity({
  cargoTomlPath = CARGO_TOML_PATH,
  packageJsonPath = PACKAGE_JSON_PATH
} = {}) {
  const cargoVersion = extractCargoWorkspaceVersion(
    readFileSync(cargoTomlPath, 'utf8')
  )
  const packageVersion = extractPackageJsonVersion(
    readFileSync(packageJsonPath, 'utf8')
  )

  return {
    cargoVersion,
    packageVersion,
    mismatches: findVersionMismatches({ cargoVersion, packageVersion })
  }
}

function main() {
  const result = checkVersionParity()

  if (result.mismatches.length > 0) {
    for (const mismatch of result.mismatches) {
      console.error(
        `Version mismatch in ${mismatch.source}: expected ${mismatch.expected}, got ${mismatch.actual}`
      )
    }
    process.exit(1)
  }

  console.log(`Version check passed: ${result.cargoVersion}`)
}

if (import.meta.url === new URL(process.argv[1], 'file:').href) {
  main()
}
