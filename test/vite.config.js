import { build, defineConfig } from 'vite'

export default defineConfig({
  // ...
    define: {
    'process.env': process.env,
        'mocha': "./node_modules/mocha/mocha.js"
    },
    optimizeDeps: {
        exclude:["mocha", "chai"]
    },
    build: {
    }
})
