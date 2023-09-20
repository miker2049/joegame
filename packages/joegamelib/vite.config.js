// vite.config.js
import { resolve } from 'path'
import { defineConfig } from 'vite'

export default defineConfig({
  define: {
    'process.env': {}
  },
  build: {
    alias: {},
    lib: {
      // Could also be a dictionary or array of multiple entry points
      entry: resolve(__dirname, 'src/main.ts'),
      name: 'JL',
      // the proper extensions will be added
      fileName: 'joegamelib',
      formats: ['iife']
    },
    rollupOptions: {}
  }
})
