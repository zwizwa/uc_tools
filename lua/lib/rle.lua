local m = {}

local band   = bit.band
local rshift = bit.rshift

function m.compile(spec)
   local chars = {}
   for i,delay in ipairs(spec) do
      -- Amount of extra delay to insert in 128us increments.
      local extra_delay = 0
      assert(delay >= 1)
      -- Compute number of extra no-flip delay commands to insert
      -- if the 7 bit delay in the flip command cannot represent
      -- it.
      while delay > 128 do
         table.insert(chars, string.char(0x7f))
         extra_delay = extra_delay + 1
         delay = delay - 128
      end
      -- Flip delay
      assert(delay <= 128)
      table.insert(chars, string.char(0x80 + band(delay - 1, 0x7F)))

      -- Insert the extra delay commands if there was overflow.
      -- First handle the overflow of the extra_edelay itself by
      -- adding multiple commands.
      --
      -- FIXME: There is still some issue with this.  40000 us
      -- seems to work, but double that gives a bad signal result.
      while extra_delay > 128 do
         table.insert(chars, string.char(0x00 + 0x7f))
         extra_delay = extra_delay - 128
         logf("WARNING: extra_delay overflow, this is still buggy\n")
      end
      -- Then insert a single extra delay with the remaining number
      -- of multiples of 128us.
      if extra_delay > 0 then
         assert(extra_delay <= 128)
         table.insert(chars, string.char(0x00 + band(extra_delay - 1, 0x7F)))
      end
   end
   return table.concat(chars)
end


return m

