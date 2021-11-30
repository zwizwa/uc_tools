# Shared between different build.sh scripts.

# The purpose of a build.sh script is to abstract a build command as a
# collection of environment variables, so it can easily be integrated
# in different build systems, or saved as a closure for re-running.

assert_vars() {
    for var in $@; do
        # echo "checking $var=\"$(eval "echo \$$var")\"" >&2
        if [ -z "$(eval "echo \$$var")" ]; then
             echo "$var is undefined" >&2
             exit 1
        fi
    done
}

# Create a closure that captures any non-zero input variables.  This
# can be used to re-run the build for simpler build debugging.

dump_closure() {
    if [ -z "$1" ]; then
        echo "ERROR: dump_closure <file>"
        exit 1
    fi
    local file="$1"
    shift
    dump_var() {
        # FIXME: don't add if var is empty
        # echo $1
        local val=$(eval "echo \$$var")
        [ ! -z "$val" ] && echo "export $1=\"$val"\"
    }
    mkdir -p $(dirname "$file")
    cat <<EOF >$file
#!/bin/sh
echo "\$0:2: "
$(for var in $*; do dump_var $var; done)
export CLOSURE=$file
cd $(pwd)
exec $(readlink -f $0)
EOF
    chmod +x $file
}

dump_closure_to_file() {
    assert_vars CLOSURE_VARS
    dump_closure "$1" $CLOSURE_VARS
    # This is for emacs to recompile the file.
    # Make this configurable? Write it somewhere else?
    echo "(compile \"$1\")"
}

# Default behavior for the above
dump_closure_default() {
    assert_vars TYPE CLOSURE_VARS
    # FIXME: Allow this to be user-definable.
    CLOSURE_DIR=/tmp/uc_tools_build
    # Don't create new closure if we're already running a closure.
    [ ! -z "$CLOSURE" ] && CLOSURE_DIR=
    # Only create closure when directory exists.
    if [ -d "$CLOSURE_DIR" ]; then
        # There is currently no good way to use the name of the build
        # product, so use the current pid instead.  Also, separate
        # files by type.
        PID=$(basename $(readlink -f /proc/self))
        CLOSURE=$CLOSURE_DIR/$TYPE/$PID
        dump_closure_to_file $CLOSURE
    fi
}
