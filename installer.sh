#!/bin/bash

target="./local/bin"
graalvmVersion="20.3.0"
javaVersion="java8"
downloadDir="/tmp/test"
cliUrl=""

jarPath="/home/porcupine/projects/zio-cli-test/target/scala-2.13/zio-awesome-project-assembly-0.0.1.jar"

case $(uname -s) in
  Darwin*) os="mac" ;;
  Linux*)  os="linux" ;;
  CYGWIN*) os="windows" ;;
  MINGW*)  os="windows" ;;
  MSYS*)   os="windows" ;;
  *)       fail os ;;
esac

graalvmPath="${downloadDir}/graalvm-ce-${javaVersion}-${graalvmVersion}"

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
    gu install native-image
}

buildNativeImage() {
    PATH="${PATH}:${graalvmPath}/bin"
    GRAALVM_HOME="${graalvmPath}"

    native-image -jar "$jarPath"
}

downloadCli() {
    file=$(basename "$cliUrl")
    downloadFile="${downloadDir}/${file}"
    [ -f "${downloadFile}" ] || curl -Lso "${downloadFile}" "$cliUrl"
}


fail() {
  printf "fail\n\033[1m\033[41m FAIL \033[0m \033[1m"
  case "$1" in
    curl)     printf "Could not find \033[0;36mcurl\033[0m, which is required by the install script." ;;
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
