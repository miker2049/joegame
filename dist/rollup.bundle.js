import resolve from '@rollup/plugin-node-resolve';
// import replace from '@rollup/plugin-replace'
import commonjs from '@rollup/plugin-commonjs';
import { terser } from 'rollup-plugin-terser';
import typescript from '@rollup/plugin-typescript';
export default [
    {
        input: 'test/index.test.ts',
        output: [
            {
                file: 'test/test-bundle.js',
                sourcemap: false,
                format: 'umd',
                name: 'joegametests'
            }
        ],
        plugins: [
            typescript({ tsconfig: false }),
            resolve(),
            commonjs(),
            terser()
        ]
    }
];
//# sourceMappingURL=rollup.bundle.js.map