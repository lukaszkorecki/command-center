class ClojureLsp < Formula
  LSP_VERSION =  '2022.06.22-14.09.50'
  ARCH = `arch`.strip == 'arm64' ? 'aarch64' : 'amd64'


  version LSP_VERSION
  url "https://github.com/clojure-lsp/clojure-lsp/releases/download/#{LSP_VERSION}/clojure-lsp-native-macos-#{ARCH}.zip"
  sha256 "196eb822578ceccefe76e3fb58f27441d06f1aeaa6231ea0dc50ddb87044b8d0"


  def install
    bin.install "clojure-lsp"
  end
end
