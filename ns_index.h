/* This is for RLE fuke indexing.
   It seems that only the skip iteration is important.
   Needs two passes to compute array size, or use a growing array.
*/

/* Create index from sequence, using:
   - next() : get next offset from iterator
   - integer subdivision
   - indexed value is kept in original data structure
*/
#include <stdint.h>
/* Precondition: offset array is large enough to fit all index data. */
static void NS(_create)(NS(_iter) *iter, NS(_offset) *offset, uintptr_t skip_count) {
    uintptr_t idx_count = 0
    for(;;) {
        NS(_offset) o1 = NS(_next)(data, &idx[idx_count++]);
        if (unlikely(NS(_offset_end) == o1)) break;
        *offset++ = o1;
        for (uintptr_t s=0; s<skip_count; s++) {
            NS(_offset) o2 = NS(_next)(data, &idx[idx_count++]);
            if (unlikely(NS(_offset_end) == o2)) break;
        }
    }
}

