class ClojureLsp < Formula
  LSP_VERSION =  '2021.12.01-12.28.16'

  version LSP_VERSION
  url "https://github.com/clojure-lsp/clojure-lsp/releases/download/#{LSP_VERSION}/clojure-lsp-native-macos-amd64.zip"


  def install
    bin.install "clojure-lsp"
  end
end
