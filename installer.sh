#!/usr/bin/env sh

# TODO: readme

set -e

appName="APP_NAME"
version="VERSION"
downloadUrl="DOWNLOAD_URL"

# appName="zio_cli_test"
# version="0.0.1"
# downloadUrl="https://github.com/JakDar/zio-cli-test/releases/download/0.0.1/zio-cli-test-0.0.1.jar"

shouldEnsureNativeImage="${ENSURE_NATIVE_IMAGE}"
graalvmVersion="${GRAALVM_JAVA_VERSION:-20.3.0}"
javaVersion="${GRAALVM_JAVA_VERSION:-java8}"

xdgUsrHome="${XDG_DATA_HOME:-"$HOME/.local/share"}"
xdgSysHome="${XDG_DATA_HOME:-/usr/share}"
xdgHome="$([ "$EUID" = "0" ] && echo "${xdgSysHome}" || echo "${xdgUsrHome}")"
appDir="${xdgHome}/${appName}"
mkdir -p "${appDir}/bin"
versionedBin="${appDir}/bin/${appName}_${version}" # native executable
versionedJar="${versionedBin}.jar"                 # downloaded JAR

graalvmDir="${xdgHome}/graalvm-ce-${javaVersion}-${graalvmVersion}"

installGraalVm() {
	if ! [ -d "${graalvmDir}" ]; then

		case "$(uname -s)" in
		Darwin*) os="mac" ;;
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
		echo "Installed GraalVM"
	fi
}

installNativeImage() {
	echo "Installing GraalVM native image"
	gu install native-image
}

ensureAppJar() {
	# TODO:bcm  handle targz and zip
	if ! [ -f "${versionedBin}" ] && ! [ -f "${versionedJar}" ]; then
		echo "Downloading ${appName}, saving to ${versionedJar}"
		curl -Lso "${versionedJar}" "$downloadUrl"
		echo "Downloaded ${appName}."
	fi
}

ensureCli() {
	ensureAppJar

	if ! [ -f "${versionedBin}" ]; then

		if [ -n "${shouldEnsureNativeImage}" ] && ! command -v native-image >/dev/null; then
			PATH="${PATH}:${graalvmDir}/bin"
			installGraalVm && installNativeImage
		fi

		if command -v native-image >/dev/null; then
			buildNativeImage
		fi
	fi
}

buildNativeImage() {
	PATH="${PATH}:${graalvmDir}/bin"
	export GRAALVM_HOME="${graalvmDir}"

	native-image -jar "${versionedJar}" "${versionedBin}"
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
	esac
	printf "\033[0m\n"
	exit 1
}

ensureCli
if [ -f "${versionedBin}" ]; then
	"${versionedBin}" "${@}"
elif [ -f "${versionedJar}" ]; then
	java -jar "${versionedJar}" "${@}"
else
	fail general
fi
