#!/usr/bin/env sh

# This is the zio-cli launcher template. Distribute this file in a Git
# repository to make it easier for users to run your zio-cli app. You will need
# to replace `APP_NAME`, `VERSION` and `DOWNLOAD_URL`. downloadUrl is expected
# to be standalone jar file.

# Application will be compiled via GraalVM native-image, if `native-image` is
# detected on target system, user can set environment variable
# ENSURE_NATIVE_IMAGE to download GraalVM and native-image. GraalVM version is
# also configurable through environment variables `GRAALVM_VERSION` and
# `GRAALVM_JAVA_VERSION` Otherwise script will fallback to running JAR file
# directly.

set -e

appName="APP_NAME"
version="VERSION"
downloadUrl="DOWNLOAD_URL"

shouldEnsureNativeImage="${ENSURE_NATIVE_IMAGE}"
graalvmVersion="${GRAALVM_VERSION:-22.3.1}"
javaVersion="${GRAALVM_JAVA_VERSION:-java8}"

xdgUsrHome="${XDG_DATA_HOME:-"$HOME/.local/share"}"
xdgSysHome="${XDG_DATA_HOME:-/usr/share}"
xdgHome="$([ "$EUID" = "0" ] && echo "${xdgSysHome}" || echo "${xdgUsrHome}")"
appDir="${xdgHome}/${appName}"
binName="${appDir}/bin/${appName}"                 # native executable
versionedBin="${binName}_${version}"               # native executable + version
versionedJar="${versionedBin}.jar"                 # downloaded JAR
graalvmDir="${xdgHome}/graalvm-ce-${javaVersion}-${graalvmVersion}"

mkdir -p "${appDir}/bin"

installGraalVm() {
	if ! [ -d "${graalvmDir}" ]; then

		case "$(uname -s)" in
		Darwin*) os="darwin" ;;
		Linux*) os="linux" ;;
		CYGWIN*) os="windows" ;;
		MINGW*) os="windows" ;;
		MSYS*) os="windows" ;;
		*) fail os ;;
		esac

		command -v curl >/dev/null || fail curl
		printf "Downloading GraalVM\n"
		file="graalvm-ce-${javaVersion}-${os}-amd64-${graalvmVersion}.tar.gz"
		url="github.com/graalvm/graalvm-ce-builds/releases/download/vm-${graalvmVersion}/${file}"
		downloadFile="/tmp/${file}"
		[ -f "${downloadFile}" ] || curl -Lo "${downloadFile}" "$url"
		tar xzf "${downloadFile}" -C "${xdgHome}"
		success graalvm
	fi
}

installNativeImage() {
	gu install native-image
	success graalvm-native-image
}

ensureAppJar() {
	if ! [ -f "${versionedBin}" ] && ! [ -f "${versionedJar}" ]; then
		echo "Downloading ${appName}, saving to ${versionedJar}"
		curl -Lso "${versionedJar}" "$downloadUrl"
		success jar
	fi
}

ensureCli() {
	ensureAppJar

	if ! [ -f "${versionedBin}" ]; then

		if [ -n "${shouldEnsureNativeImage}" ] && ! command -v native-image >/dev/null; then
			PATH="${PATH}:${graalvmDir}/Contents/Home/bin"
			installGraalVm && installNativeImage
		fi

		if command -v native-image >/dev/null; then
			buildNativeImage
		fi
	fi
}

buildNativeImage() {
	PATH="${PATH}:${graalvmDir}/Contents/Home/bin"
	export GRAALVM_HOME="${graalvmDir}"
	cd "${appDir}/bin"
	native-image --no-fallback -jar "$(basename "${versionedJar}")" "$(basename "${versionedBin}")"
	ln -s "$(basename "${versionedBin}")" "$(basename "${binName}")"
	cd -
	success native-image
}

fail() {
	printf "fail\n\033[1m\033[41m FAIL \033[0m \033[1m"
	case "$1" in
	curl) printf "Could not find \033[0;36mcurl\033[0m, which is required by the install script." ;;
	os) printf "Could not identify the operating system type" ;;
	general) printf "Couldn't find %s nor %s." "${versionedJar}" "${versionedBin}" ;;
	esac
	printf "\033[0m\n"
	exit 1
}

success() {
	printf "success\n\033[1m\033[42m SUCCESS \033[0m \033[1m"
	case "$1" in
	graalvm) printf "GraalVM successfully installed!" ;;
	graalvm-native-image) printf "GraalVM native-image successfully installed!" ;;
	native-image) printf "Built native image for %s." "${appName}" ;;
	jar) printf "Downloaded %s" "${appName}" ;;
	esac
	printf "\033[0m\n"
}

ensureCli
if [ -f "${versionedBin}" ]; then
	"${versionedBin}" "${@}"
elif [ -f "${versionedJar}" ]; then
	java -jar "${versionedJar}" "${@}"
else
	fail general
fi
