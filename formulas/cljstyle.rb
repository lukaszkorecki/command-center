class Cljstyle < Formula
  VERSION =  '0.15.0'

  version VERSION
  url "https://github.com/greglook/cljstyle/releases/download/#{VERSION}/cljstyle_#{VERSION}_macos.tar.gz"


  def install
    bin.install "cljstyle"
  end
end
