class ClojureLsp < Formula
  LSP_VERSION =  '2021.12.01-12.28.16'

  version LSP_VERSION
  url "https://github.com/clojure-lsp/clojure-lsp/releases/download/#{LSP_VERSION}/clojure-lsp-native-macos-amd64.zip"
  sha256 "6c53000ca9f4cee8164f9b737a28e506dde26b3991fa5d5145cb8124dc71ba68"


  def install
    bin.install "clojure-lsp"
  end
end
