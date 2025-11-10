import { rumina } from './lib/index.mjs';

const result = await rumina('10 + 20;');

if (result.startsWith("Error:")) {
  console.error(result);
} else {
  console.log("计算结果:", result);
}