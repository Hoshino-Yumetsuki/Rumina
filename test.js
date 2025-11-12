import { rumina } from './lib/index.mjs'

const result = await rumina('bigint a = 114514; bigint b = 1919810; a ^ b;')

if (result.startsWith('Error:')) {
  console.error(result)
} else {
  console.log('计算结果:', result)
}
