-- Mapping from Lua syntax to XML
-- An element is represented as a 3-element array: tag, attributes, child elements.
-- {'a',{href = 'http://127.0.0.1'},{'Link'}}

local lxml = {}
function lxml.w_element(w, element)
   assert(w)
   assert(element)
   if type(element) == 'string' then
      -- FIXME: Do proper string quoting. For now these are not needed.
      -- (") &quot;
      -- (&) &amp;
      -- (') &apos;
      -- (<) &lt;
      -- (>) &gt;
      w(element)
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
   w('</') ; w(tag) ; w('>')
end
function lxml.w_elements(w, elements)
   assert(elements)
   for i, element in ipairs(elements) do
      lxml.w_element(w, element)
   end
end

-- Only expose a single function, which prints multiple elements.
-- Just call it with a singleton to print one element.
function lxml.elements_to_string(elements)
   local strs = {}
   local function w(str)
      -- assert(type(str) == 'string') -- number is ok.
      table.insert(strs, str)
   end
   lxml.w_elements(w, elements)
   return table.concat(strs)
end

return lxml
