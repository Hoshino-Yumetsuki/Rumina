import test from 'node:test'
import assert from 'node:assert/strict'

import {
  extractCargoWorkspaceVersion,
  findVersionMismatches
} from './check-version.js'

test('extractCargoWorkspaceVersion reads workspace package version', () => {
  const cargoToml = `
[workspace]
members = ["."]

[workspace.package]
version = "1.2.7"
`

  assert.equal(extractCargoWorkspaceVersion(cargoToml), '1.2.7')
})

test('findVersionMismatches reports mismatched package version', () => {
  assert.deepEqual(
    findVersionMismatches({
      cargoVersion: '1.2.7',
      packageVersion: '1.2.6'
    }),
    [
      {
        source: 'package.json',
        expected: '1.2.7',
        actual: '1.2.6'
      }
    ]
  )
})

test('extractCargoWorkspaceVersion throws when workspace version is missing', () => {
  assert.throws(
    () => extractCargoWorkspaceVersion('[workspace]\nmembers = ["."]\n'),
    /workspace package version/i
  )
})
