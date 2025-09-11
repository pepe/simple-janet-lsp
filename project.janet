(declare-project
  :name "simple-janet-lsp"
  :description ```A simple janet lsp that works with Helix ```
  :author ```MrWheatley ```
  :dependencies @["spork"]
  :version "0.1.0")

(declare-source
  :source ["simple-janet-lsp"])

(declare-binscript
  :main "bin/janet-lsp"
  :auto-shebang true
  :is-janet true)
