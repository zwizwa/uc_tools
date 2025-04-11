-- C renderer for osc.h style static OSC tree data structure.

-- TODO: This makes matrices and other structure products inefficient.
-- Maybe make an implementation that can just store the pointer to the
-- float instead of requiring a setter.

local list = require('lib.tools.list')
local iolist = require('lure.iolist')
local prefix = iolist.prefix
local map = list.map


local m = {}

function m.render_c(param_tree, opt)
   -- Accumulators
   local set_code = {}
   local def_code = {}
   local osc_paths = {}

   -- Current path, gets pushed/pop for each level
   local path = {}

   -- Config defaults
   opt = opt or { }
   local root = opt.root or 'root'
   local var_set = opt.var_set or function(s_var, path, value)
      return {'s->', iolist.join('.',path), ' = ', value, ';'}
   end
   function osc_path(path)
      return prefix('/',path,2)
   end

   local function cpath(sep, path)
      return map(function(p) return({p,sep}) end, path)
   end
   function render_atom(typ, c_name)
      local osc_name = c_name
      local full_c_name = {cpath('_',path),c_name}
      local path1 = list.concat({path, {c_name}})
      local var_set_code = var_set('s',path1,'val')

      table.insert(osc_paths, osc_path(path1))

      if true then
         -- Use setters
         table.insert(
            set_code, {
               {'static void set_',full_c_name,'(struct param_context *s, ',typ,' val) ',
                '{ ', var_set_code, ' }\n'},
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
         -- Use pointers (FIXME, API changed from var_ref to var_set)
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
   function render_record(name, thing, is_root)
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
         local full_c_name = {cpath('_',path),name}

         if is_root and opt.extern then
            for _,extern_name in ipairs(opt.extern) do
               table.insert(sub_c_names, extern_name)
            end
         end

         local osc_name = name
         local prefix_sub_c_names = prefix(',\n    &', sub_c_names)


         if #prefix_sub_c_names == 0 then
            -- Workaround: macro doesn't work with empty list, so
            -- insert an extra NULL terminator.
            prefix_sub_c_names = ", NULL"
         end
         table.insert(
            def_code,
            {'DEF_OSC_LIST(',full_c_name,', "',osc_name,'"',
             prefix_sub_c_names,');\n'})
         return full_c_name
      end
   end

   render_record(root, param_tree, true)

   return {set_code, def_code}, osc_paths
end


return m
