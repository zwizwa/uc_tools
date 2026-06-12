#!/usr/bin/env python3
# Iterate over the holes in a file
# https://claude.ai/chat/07491d10-2a43-4469-a82f-51446c38bab4

import os, errno, sys
fd = os.open(
    sys.argv[1],
    os.O_RDONLY)
end = os.fstat(fd).st_size
pos = 0
while pos < end:
    try:
        data = os.lseek(fd, pos, os.SEEK_DATA)
    except OSError:
        break  # only holes remain
    hole = os.lseek(fd, data, os.SEEK_HOLE)
    print(f"data: {data}..{hole} n={hole-data}")
    pos = hole
