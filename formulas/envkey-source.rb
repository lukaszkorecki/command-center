class EnvkeySource < Formula
  VERSION = '1.2.9'
  url "https://github.com/envkey/envkey-source/releases/download/v#{VERSION}/envkey-source_#{VERSION}_darwin_amd64.tar.gz"

  def install
    bin.install 'envkey-source'
  end
end
