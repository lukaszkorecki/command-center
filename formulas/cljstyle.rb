class Cljstyle < Formula
  VERSION =  '0.15.0'

  version VERSION
  url "https://github.com/greglook/cljstyle/releases/download/#{VERSION}/cljstyle_#{VERSION}_macos.tar.gz"
  sha256 "7435e8fe23b5d25028b88906092b3cd6c93e976ca75f39a55fe7f8927696fbe8"

  def install
    bin.install "cljstyle"
  end
end
