# Since we support multiple platform build configurations anyway, it
# is easy to drop in this file and also support building a lib.host.a
# to test platform independent code on linux.

[ -z "$UC_TOOLS" ] && UC_TOOLS=..

GCC=gcc

