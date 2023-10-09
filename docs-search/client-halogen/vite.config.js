import { defineConfig } from 'vite'

export default defineConfig({
  root: "../../generated-docs/html/",
  server: {
    open: true,
  }
});
