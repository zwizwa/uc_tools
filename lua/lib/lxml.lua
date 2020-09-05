-- Mapping from Lua syntax to XML
-- An element is represented as a 3-element array: tag, attributes, child elements.
-- {'a',{href = 'http://127.0.0.1'},{'Link'}}

-- 1. XML TO STRING SEQUENCE / STRING

-- Expose a single function (elements) instead of having both element
-- and elements (element sequence).  This reduces notational overhead.
-- Two variants are provided: a writer to allow a more efficinet
-- implementation later, and a _to_string renderer for convenience.
--

local quote = {
   [34] = '&quot;', -- "
   [38] = '&amp;',  -- &
   [39] = '&apos;', -- '
   [60] = '&lt;',   -- <
   [62] = '&gt;',   -- >
}

local function w_string(w, str)
   for i=1,#str do
      local q = quote[str:byte(i)]
      if q then
         -- There is at least one quoted character.  Special case the
         -- whole string so we don't need to re-hash anything if there
         -- are no quoted characters.
         for j=1,#str do
            local c = str:byte(j)
            local q = quote[c]
            if q then
               w(q)
            else
               w(string.char(c))
            end
         end
         return
      end
   end
   w(str)
end


local lxml = {}
function lxml.w_elements(w, elements)
   for i,element in ipairs(elements) do
      assert(w)
      assert(element)
      if type(element) == 'string' then
         w_string(w, element)
         return
      end
      local tag, attrs, elements = unpack(element)
      assert(tag)
      if not attrs then attrs = {} end
      if not elements then elements = {} end
      w('<') ; w(tag)
      for attr, val in pairs(attrs) do
         -- FIXME: Do proper string quoting.
         w(' ') ; w(attr) ; w('="') ; w(val) ; w('"')
      end
      w('>')
      lxml.w_elements(w, elements)
      w('</') ; w(tag) ; w('>\n')
   end
end

function lxml.elements_to_string(elements)
   local strs = {}
   local function w(str)
      -- assert(type(str) == 'string') -- number is ok.
      table.insert(strs, str)
   end
   lxml.w_elements(w, elements)
   return table.concat(strs)
end



-- 2. XML CONSTRUCTORS

-- Some design decisions:
--
-- - Something I almost always do wrong initially when writing
--   printers / renderers, but not this time!  Printing (rendering) is
--   parametized using single config (environment) table that is
--   passed down the call chain.  It is typical to want to
--   parameterize leaf nodes, which is awkward to do in another way.
--
-- - Dynamic types are a pain in this setting, so add as many asserts
--   as possible to catch errors early.
--

-- FIXME: Currently I only need SVG, so this is not yet translated.



return lxml
