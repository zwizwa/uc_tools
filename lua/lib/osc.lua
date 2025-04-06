-- C renderer for osc.h style static OSC tree data structure.

-- TODO: This makes matrices and other structure products inefficient.
-- Maybe make an implementation that can just store the pointer to the
-- float instead of requiring a setter.

local list = require('lib.tools.list')
local iolist = require('lure.iolist')
local prefix = iolist.prefix
local map = list.map


local m = {}

function m.render_c(param_tree)
   local set_code = {}
   local def_code = {}

   local path = {}
   local function cpath(sep)
      return map(function(p) return({p,sep}) end, path)
   end

   function render_atom(typ, c_name)
      local osc_name = c_name
      local full_c_name = {cpath('_'),c_name}
      local var = {'s->',cpath('.'),c_name}
      if true then -- FIXME: make this configurable
         -- Use setters
         table.insert(
            set_code, {
               {'set_',full_c_name,'(struct areal *s, ',typ,' val) ',
                '{ ', var,' = val; }\n'},
         })
         local typ_def = {
            float = 'DEF_OSC_SET_FLOAT',
            int   = 'DEF_OSC_SET_INT',
         }
         table.insert(
            def_code, {
               {typ_def[typ],'(',full_c_name,', "',osc_name,
                '", set_',full_c_name, ');\n'},
         })
      else
         -- Use pointers
         local typ_def = {
            float = 'DEF_OSC_PTR_FLOAT',
            int   = 'DEF_OSC_PTR_INT',
         }
         table.insert(
            def_code, {
               {typ_def[typ],'(',full_c_name,', "',osc_name,
                '", &',var, ');\n'},
         })
      end
      return full_c_name
   end
   function render_record(name, thing)
      if type(thing) == 'string' then
         -- Atom
         return render_atom(thing, name)
      else
         -- Sub list
         local sub_c_names = {}
         for _, record in ipairs(thing) do
            local name1, thing1 = unpack(record)
            table.insert(path, name)
            local sub_c_name = render_record(name1, thing1)
            table.insert(sub_c_names, sub_c_name)
            table.remove(path)
         end
         local full_c_name = {cpath('_'),name}
         local osc_name = name
         table.insert(
            def_code,
            {'DEF_OSC_LIST(',full_c_name,', "',osc_name,'"',
             prefix(', ', sub_c_names),');\n'})
         return full_c_name
      end
   end

   render_record('root', param_tree)

   return {set_code, def_code}
end

return m
