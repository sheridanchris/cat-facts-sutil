import { defineConfig } from 'vite'

export default defineConfig({
  root: "./src/App",
  build: {
    outDir: "../../deploy/public",
  },
})
