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

dump_closure() {
    local file=$1
    shift

    dump_var() {
        # FIXME: don't add if var is empty
        # echo $1
        local val=$(eval "echo \$$var")
        [ ! -z "$val" ] && echo "export $1=\"$val"\"
    }
    mkdir -p $(dirname $file)
    cat <<EOF >$file
#!/bin/sh
set -ex
$(for var in $*; do dump_var $var; done)
cd $(pwd)
exec $(readlink -f $0)
EOF
    chmod +x $file
}
