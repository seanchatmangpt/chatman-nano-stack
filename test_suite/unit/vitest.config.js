import { defineConfig } from 'vitest/config'
import vue from '@vitejs/plugin-vue'
import { resolve } from 'path'

export default defineConfig({
  plugins: [vue()],
  test: {
    globals: true,
    environment: 'happy-dom',
    setupFiles: ['./test-setup.js'],
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      exclude: [
        'node_modules/',
        'test/',
        '**/*.config.js',
        '**/*.config.ts'
      ],
      thresholds: {
        global: {
          branches: 80,
          functions: 80,
          lines: 80,
          statements: 80
        }
      }
    },
    reporters: ['verbose', 'json', 'html'],
    outputFile: {
      json: './reports/test-results.json',
      html: './reports/test-results.html'
    }
  },
  resolve: {
    alias: {
      '~': resolve(__dirname, '../../nuxt_ui'),
      '@': resolve(__dirname, '../../nuxt_ui')
    }
  }
})