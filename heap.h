#ifndef HEAP_H
#define HEAP_H

/* Heap data structure to implement a priority queue / software timer.

   https://en.wikipedia.org/wiki/Heap_%28data_structure%29
   https://stackoverflow.com/questions/16520607/maintaining-a-sorted-list
   https://www.pjsip.org/pjlib/docs/html/group__PJ__TIMER.htm */

/* Use case: software timer that delivers timeout tokes to a set of
   buffers.  My primary use for this is receive timeouts, so timer
   interrupt will run at the same priority as receivers to avoid
   synchronization issues.

   For a timer, "less than" should probably be defined circularly, so
   rolling counters can be used.
*/



#endif
