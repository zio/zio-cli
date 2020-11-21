#!/bin/bash

# TODO: read from command line
cliDownloadUrl="https://github.com/JakDar/zio-cli-test/releases/download/0.0.1/zio-cli-test-0.0.1.jar"
cliFile=$(basename "$cliDownloadUrl")
graalvmVersion="20.3.0"
javaVersion="java8"
# TODO: allow to customize
downloadDir="/tmp/test"
installLocation="./local/bin"


case $(uname -s) in
  Darwin*) os="mac" ;;
  Linux*)  os="linux" ;;
  CYGWIN*) os="windows" ;;
  MINGW*)  os="windows" ;;
  MSYS*)   os="windows" ;;
  *)       fail os ;;
esac

graalvmPath="${downloadDir}/graalvm-ce-${javaVersion}-${graalvmVersion}"

# TODO: check if graalvm exists locally
installGraalVm() {
    command -v curl > /dev/null || fail curl
    printf "Downloading GraalVM\n"
    mkdir -p "$downloadDir"
    file="graalvm-ce-${javaVersion}-${os}-amd64-${graalvmVersion}.tar.gz"
    url="github.com/graalvm/graalvm-ce-builds/releases/download/vm-${graalvmVersion}/${file}"
    downloadFile="${downloadDir}/${file}"
    [ -f "${downloadFile}" ] || curl -Lso "${downloadFile}" "$url"
    [ -d "${graalvmPath}" ] || tar xzf "${downloadFile}" -C "${downloadDir}" 
}

installNativeImage() {
    PATH="${PATH}:${graalvmPath}/bin"
    gu install native-image
}

downloadCli() {
    downloadFile="${downloadDir}/${cliFile}"
    [ -f "${downloadFile}" ] || curl -Lso "${downloadFile}" "$cliDownloadUrl"
}

buildNativeImage() {
    PATH="${PATH}:${graalvmPath}/bin"
    GRAALVM_HOME="${graalvmPath}"

    # TODO: do not rebuild every time
    # TODO: set the install location
    native-image -jar "${downloadDir}/${cliFile}" application
}

fail() {
  printf "fail\n\033[1m\033[41m FAIL \033[0m \033[1m"
  case "$1" in
    curl)     printf "Could not find \033[0;36mcurl\033[0m, which is required by the install script." ;;
    os)       printf "Could not identify the operating system type" ;;
  esac
  printf "\033[0m\n"
  exit 1
}

success() {
  printf "success\n\033[1m\033[42m SUCCESS \033[0m \033[1m"
  case "$1" in 
      graalvm) printf "GraalVM successfully installed!" 
  esac
  printf "\033[0m\n"
  exit 1
}

buildNativeImage
