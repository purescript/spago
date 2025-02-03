import path from 'path'
import { mkdirSync } from 'node:fs'
import envPaths from 'env-paths'

const paths = envPaths('test')
console.log(paths)

const d = path.join(paths.temp, 'dir')
mkdirSync(d, { recursive: true })
console.log(d)

process.chdir(d)
const cwd = process.cwd()
console.log(cwd)

