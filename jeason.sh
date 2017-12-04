# This script calls ./_build/main.native (the conversion script) and pipe it
# through refmt. It's more elaborated than a one-iner because we need to find
# where our refmt is installed; we can't just reach into node_modules since
# we're being installed globally.

if [ $# -eq 0 ]
then
  echo "Please provide as argument the JS file to convert over."
  exit 1
fi

# Find the path (drilling through symlinks, if necessary) of this script

# http://stackoverflow.com/questions/59895/can-a-bash-script-tell-what-directory-its-stored-in
THISSCRIPT="${BASH_SOURCE[0]}"
while [ -h "$THISSCRIPT" ]; do # resolve $THISSCRIPT until the file is no longer a symlink
  SCRIPTDIR="$( cd -P "$( dirname "$THISSCRIPT" )" && pwd )"
  THISSCRIPT="$(readlink "$THISSCRIPT")"
  [[ $THISSCRIPT != /* ]] && THISSCRIPT="$SCRIPTDIR/$THISSCRIPT" # if $THISSCRIPT was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done

SCRIPTDIR="$( cd -P "$( dirname "$THISSCRIPT" )" && pwd )"

SOURCE=$(pwd)/$1

cd $SCRIPTDIR; ./_build/main.native $SOURCE | refmt --parse binary --print re
