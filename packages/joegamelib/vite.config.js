// vite.config.js
import { resolve } from 'path'
import { defineConfig } from 'vite'

export default defineConfig({
  build: {
    alias: {},
    lib: {
      // Could also be a dictionary or array of multiple entry points
      entry: resolve(__dirname, 'src/index.ts'),
      name: 'JL',
      // the proper extensions will be added
      fileName: 'joegamelib'
    },
    rollupOptions: {}
  }
})
