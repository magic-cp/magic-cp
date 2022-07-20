#!/usr/bin/env bash
# Setup the development environment
mkdir -p ./bin ./logs

unameOut="$(uname -s)"
case "${unameOut}" in
    Linux*)     machine=Linux;;
    Darwin*)    machine=Mac;;
    CYGWIN*)    machine=Cygwin;;
    MINGW*)     machine=MinGw;;
    *)          machine="UNKNOWN:${unameOut}"
esac
machineName="$(uname -m)"

# Install third party dependencies

if [ "$machine" = "Linux" ]; then

  echo "ℹ️ Installing apt dependencies..."
  sudo apt-get install libcurl4-openssl-dev -y
elif [ "$machine" = "Mac" ]; then

  echo "Installing brew dependencies..."
  brew install wget
fi

# Install cf-tool
echo "ℹ️ Checking if cf-tool is installed..."

if [ ! -f ./bin/cf ]; then
    echo "❌ cf-tool is not installed. Installing..."
    CF_TOOL_VERSION='v1.0.0'
    CF_ZIP=$(mktemp)
    CF_DIR=$(mktemp -d)

    CF_URL=https://github.com/xalanq/cf-tool/releases/download/$CF_TOOL_VERSION/cf_"$CF_TOOL_VERSION"_darwin_64.zip
    echo "Downloading from $CF_URL"
    wget $CF_URL -O $CF_ZIP
    unzip -o $CF_ZIP -d $CF_DIR
    mv $CF_DIR/cf_"$CF_TOOL_VERSION"_darwin_64/cf ./bin/cf
    [ -f ./bin/cf ] && echo "✅ cf-tool was succesfully installed"
else
    echo "✅ cf-tool is installed"
fi

echo "ℹ️ Generating config.cfg file..."
cat << EOF > config.cfg
cf-tool-path = "$PWD/bin"
project-root = "$PWD"
cf-parse-dir = "$PWD/cf/contest"
log-root = "$PWD/logs"
EOF
echo "✅ config.cfg file generated"


echo "ℹ️ Checking if chromedriver is installed..."
if ! command -v chromedriver &> /dev/null
then
    echo "❌ chromedriver is not installed. Installing..."
    mkdir -p $HOME/bin

    if [ "$machine" = "Linux" ]; then
      CHROME_VERSION=$(google-chrome --version | cut -d' ' -f 3)
    elif [ "$machine" = "Mac" ]; then
      CHROME_VERSION=$(/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome --version | cut -d' ' -f 3)
    fi

    # see the following for URL construction for chromedriver download
    # https://chromedriver.chromium.org/downloads/version-selection
    echo "Installed google-chrome version: $CHROME_VERSION"
    CHROME_VERSION_WITHOUT_BUILD_NUMBER=$(echo $CHROME_VERSION | cut -d'.' -f1,2,3)
    LATEST_CHROME_VERSION_AVAILABLE=$(wget https://chromedriver.storage.googleapis.com/LATEST_RELEASE_$CHROME_VERSION_WITHOUT_BUILD_NUMBER -O - 2> /dev/null)

    echo "Latest chromedriver version available: $LATEST_CHROME_VERSION_AVAILABLE"

    if [ "$machine" = "Linux" ]; then
      CHROMEDRIVER_ZIP="chromedriver_linux64.zip"
    elif [ "$machine" = "Mac" ]; then
      if [ "$machineName" = "arm64" ]; then
        CHROMEDRIVER_ZIP="chromedriver_mac64_m1.zip"
      else
        CHROMEDRIVER_ZIP="chromdriver_mac64.zip"
      fi
    fi

    CHROMEDRIVER_URL=https://chromedriver.storage.googleapis.com/$LATEST_CHROME_VERSION_AVAILABLE/$CHROMEDRIVER_ZIP

    CHROMEDRIVER_ZIP_FILE=$(mktemp)
    wget $CHROMEDRIVER_URL -O $CHROMEDRIVER_ZIP_FILE
    unzip -o $CHROMEDRIVER_ZIP_FILE -d $HOME/bin
    [ -f $HOME/bin/chromedriver ] && echo "✅ chromedriver was succesfully installed"
else
    echo "✅ chromedriver is installed"
fi


echo "ℹ️ Checking if stylish-haskell is installed..."
if ! command -v stylish-haskell &> /dev/null
then
    echo "❌ stylish-haskell is not installed. Installing..."
    mkdir -p $HOME/bin
    SH_VERSION=v0.14.2.0
    SH_ARCHIVE=$(mktemp)
    SH_DIR=$(mktemp -d)

    if [ "$machine" = "Linux" ]; then
      SH_FILE_NAME=stylish-haskell-$SH_VERSION-linux-x86_64
      SH_URL=https://github.com/jaspervdj/stylish-haskell/releases/download/$SH_VERSION/$SH_FILE_NAME.tar.gz

      wget $SH_URL -O $SH_ARCHIVE
      tar -xvf $SH_ARCHIVE -C $SH_DIR
      ls $SH_DIR

    elif [ "$machine" = "Mac" ]; then
      SH_FILE_NAME=stylish-haskell-$SH_VERSION-darwin-x86_64
      SH_URL=https://github.com/jaspervdj/stylish-haskell/releases/download/$SH_VERSION/$SH_FILE_NAME.zip

      wget $SH_URL -O $SH_ARCHIVE
      unzip $SH_ARCHIVE -d $SH_DIR
      ls $SH_DIR
    fi

    mv $SH_DIR/$SH_FILE_NAME/stylish-haskell $HOME/bin/


    [ -f $HOME/bin/stylish-haskell ] && echo "✅ stylish-haskell was succesfully installed"
else
    echo "✅ stylish-haskell is installed"
fi

echo "ℹ️ Configuring authentication details for cf"
echo "   Please, choose option number 0 and set your codeforces credentials"
cf config

echo "ℹ️ Overwriting ~/.cf/config"
mkdir -p $HOME/.cf/

# Reason we wrap `ghc`` over `arch` is because `ghc` will be called from cf-tool, which is running thru Rosetta 2 emulation.
# And GHC installation doesn't play well on Rosetta 2. Forcing the arm64 architecture will make it work.
if [ "$machineName" = "arm64" ]; then
  CF_CONFIG_SCRIPT="arch -arm64 ghc $%full%$"
else
  CF_CONFIG_SCRIPT="ghc $%full%$"
fi

cat << EOF > $HOME/.cf/config
{
  "template": [
    {
      "alias": "hs",
      "lang": "12",
      "path": "/home/german/.cf/templates/template.hs",
      "suffix": [
        "hs"
      ],
      "before_script": "$CF_CONFIG_SCRIPT",
      "script": "./$%file%$",
      "after_script": ""
    }
  ],
  "default": 0,
  "gen_after_parse": false,
  "host": "https://codeforces.com",
  "proxy": "",
  "folder_name": {
    "acmsguru": "acmsguru",
    "contest": "contest",
    "group": "group",
    "gym": "gym",
    "root": "cf"
  }
}
EOF
echo "✅ ~/.cf/config properly set"

echo "ℹ️ Setting haskell template for cf-tool"
mkdir -p ~/.cf/templates/
touch ~/.cf/templates/template.hs
echo "✅ Template configured"
