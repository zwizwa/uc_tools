-- Code for generating cproc patching code.
--
-- After doing some more hands-on work, it really seems that block
-- processing is essential to be able to use the available
-- parallelism:
--
-- 1. vectorization
-- 2. multi-issue
-- 3. pipelining
--
-- These are two _distinct_ constraints.
--
-- Vectorization is fairly straightforward: make sure that data is
-- organized in memory such that it does not need to be permuted on
-- load/store.
--
-- Multi issue can mostly be lumped in with vectorization.  Typically
-- in a DSP core routine you keep the data math unit busy and use
-- multi issue to perform pointer math.
--
-- Pipelining code that accumulates often needs multiple accumulators
-- to avoid stalls due to data dependencies.
--
--
-- An additional constraint is memory bandwidth between cache memory
-- and the registers, and in lesser extent between slower and faster
-- caches.  There are conflicting constraints here: avoiding pipeline
-- stalls means using more registers, which when they run out will
-- require memory transfers.
--
--
-- The point I am trying to make is that to make something fast, the
-- algorithm needs as much parallelism and data-locality as possible,
-- which boils down to:
-- . block processing
-- . input/output/state memory layout optimization
--
-- Assuming the 'bottom' abstraction is the cproc (element-wise
-- processing), we do have a degree of freedom in the loop patterns
-- and data layout.
--
-- This module deals mostly with that.
--
--
-- Summary: how to write optimal DSP code?
-- . write vectorized assembly for the modules with the highest cpu load
-- . create a high level patcher that has 'wires as multi-sample buffers'
-- . generate the buffer transposition code
-- . automate buffer allocation



-- Cases:
--
-- 1: given single-channel op (e.g. EQ), map it over i/o busses that
--    have arbitrary transposition.


-- The core insight is that the buffer layout is a property of the
-- _wire_, and that we deal with that in the 'bus mapper'.
--
-- In the current mixer-style app the parallelism we exploit is the
-- bus: there are a number of signal buffers with filters, and those
-- filters are implemented with core routines that operate on
-- float32x4_t neon code, e.g. 4 busses are bundled into a single
-- buffer.  Other parts of the app have just float buffers.  We are
-- inerested in generating the loop code for those "connecting
-- processors".

-- Given: a scalar processor + i/o buffers with specific layout
-- Asked: generate a loop that connects buffers to scalar proc

-- For that the basic abstraction is the representation of a signal
-- dereference.  This is in the end just a coordinate transform.
--
-- high level:  bus, sample
-- low level:   buffer, offset
--
-- To make this optimizable, the transformer should produce the data
-- necessray to generate an interator:
-- . init code
-- . update code
-- Assume that stop condition is separate

-- Inputs layed out as 4-vector busses

local function input(chanel,sample)
   local buf = channel % 4
   ...
   return
      update = '+= 4',
   }
end




local m = {}
function m.render_map(cproc, input, output)
   for i=1,12 do
      log(input(i))
   end
end
return m



-- I wonder if what I am trying to do is just this:
-- . Keep transposers simple to/from vec4
-- . Generalize wire with 1-element proc
-- . Put all transposers in the schematic explicitly
--
-- It's easy enough to make the vectorization explicit in the toplevel
-- schematic.

