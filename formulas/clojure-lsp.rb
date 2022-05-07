class ClojureLsp < Formula
  LSP_VERSION =  '2022.05.03-12.35.40'

  version LSP_VERSION
  url "https://github.com/clojure-lsp/clojure-lsp/releases/download/#{LSP_VERSION}/clojure-lsp-native-macos-amd64.zip"
  sha256 "32520aa126b0085a3582cc0f87fb62252146dc3e2a60c0c144b57dd3cc09fcd4"


  def install
    bin.install "clojure-lsp"
  end
end
