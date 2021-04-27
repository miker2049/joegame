import commonjs from '@rollup/plugin-commonjs';
import { terser } from "rollup-plugin-terser";

export default {
  input: 'src/bondage.js',
  output: {
    file: 'dist/bondage.min.js',
    name: 'bondage',
    format: 'umd'
  },
  plugins: [
    commonjs(),
    terser()
  ]
};
