/* Convert Logic2 Binary Export format to flat 8 channel binary at 2 MHz */
// See also https://sigrok.org/wiki/File_format:Saleae


// TODO:
/* The binary format is odd.  Converting it back to a basic bit stream
   could be done by "painting" a large mmapped file, but this is
   inconvenient.  To produce a streaming version that can pipe data to
   stdout, use a software timer to drive the iteration over the
   diff-encoded signals.  Seems interesting to solve it as this
   generalizes to other diff logic formats like vcd. */

#include "macros.h"
#include "assert_mmap.h"  // read
#include "mmap_file.h"    // write
#include <stdint.h>
#include <stdlib.h>

#define RATE 20000000

// https://support.saleae.com/faq/technical-faq/binary-export-format-logic-2

struct logic2_digital {
    uint8_t identifier[8];
    int32_t version;
    int32_t type;
    uint32_t initial_state;
    double begin_time;
    double end_time;
    uint64_t num_transitions;
    double transition_time[];
} __attribute__((__packed__));

void log_digital(const struct logic2_digital *data) {
    char id[9] = {};
    memcpy(id, data->identifier, sizeof(data->identifier));
    LOG("identifier=%s\n", id);
    LOG("version=%u\n", data->version);
    LOG("type=%u\n", data->type);
    LOG("initial_state=%u\n", data->initial_state);
    LOG("begin_time=%f\n", data->begin_time);
    LOG("end_time=%f\n", data->end_time);
    LOG("num_transitions=%llu\n", data->num_transitions);
}

struct mmap_file out;

void paint(const struct logic2_digital *data,
           double begin, double end)
{
    uint32_t state = data->initial_state;
    size_t left  = 0;
    size_t right = 0;
    double t = begin;
    for (uint64_t i=0; i<data->num_transitions; i++) {
        double dt = data->transition_time[i];
        (void)left;
        (void)state;
        // LOG("%f %f\n", t, dt);
        t += dt;
        left = right;
        right = t * RATE;
    }
}

// FIXME: Add time range

int main(int argc, const char **argv) {
    ASSERT(argc >= 3);
    const char *out_name = argv[1];

    const char **in = &argv[2];
    int n = argc - 2;
    LOG("%d channels\n", n);
    const struct logic2_digital *data[n];
    for (int i=0; i<n; i++) {
        LOG("- channel %d\n", i);
        off_t size;
        data[i] = assert_mmap_rdonly(in[i], 0, &size);
        ASSERT(size >= sizeof(struct logic2_digital));
        // log_digital(data[i]);
    }

    // Assume all files have the same span.  Pick span from first.
    double begin = data[0]->begin_time;

    begin = 2700;

    double end   = data[0]->end_time;
    LOG("(%f,%f)\n", begin, end);

    off_t size = RATE * (end - begin);

    mmap_file_open_rw(&out, out_name, size);

    for (int c=0; c<n; c++) {
        paint(data[c], begin, end);
    }


    // log_data(data[0]);

    mmap_file_close(&out);

    return 0;
}
