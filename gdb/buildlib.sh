# The build system is a little complex as it is constructed to support
# a number of use cases and is still in flux.  Some history:
#
# 1. This Started out as Makefiles, but those were deemed to be too
# cumbersome to use due to the well known "recursive make" issues.
#
# 2. I discovered apenwarr's redo, which solved the recursive make
# issues, but landed me in horrible shell programming to be able to
# support different target and link types in the same build system.
#
# 3. I wrote a version of redo in Erlang, which was able to provide
# the expressivity needed to implement the rules in a sane way, but
# this landed me in non-standard territory.


# So what is the design I ended up with?
#
# - 1. Keep separate build.*.sh files that take environment variables
#      and perform a build.
#
# - 2. Keep the default.*.do files to call into those build.*.sh
#      files, to allow people to either use apenwarr redo, or minimal
#      do.
#
# - 3. Use the Erlang system during development.


# Eventually, I would like to get rid of apenwarr redo, and generate
# Makefiles from my Erlang redo.


# Some design rules for the build.*.sh scripts
#
# - These should be standalone executable scripts, not "dot scripts".
#
# - All used variables should be checked with the need() function to
#   show proper error messages on misconfiguration.

need_vars() {
    for var in $@; do
        echo "checking $var=\"$(eval "echo \$$var")\"" >&2
        if [ -z "$(eval "echo \$$var")" ]; then
             echo "$var is undefined" >&2
             exit 1
        fi
    done
}
