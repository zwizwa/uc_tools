-- Shared language front-end
-- Preprocessor that performs the following:
--
-- MACRO-EXPAND
-- A-NORMAL FORM
-- VARIABLE RENAMING (FIXME)
-- SIMPLIFY BLOCK (TODO)
-- SINGLE LAMBDA EXPR (TODO)
--
-- Separate later passes:
--
-- BLOCK FLATTENING
--
-- OUTPUT CLEANUP / LANG PPRINT
--
-- Macro expansion and ANF seem to go hand-in hand.
-- Variable renaming is useful for later block-flattening.

-- FIXME: This is still written in old se.unpack() style, and might
-- benefit from using the pattern matcher.  However, it works, so not
-- touching it for now.

local se = require('lure.se')
local comp = require('lure.comp')

local ins = table.insert
local a2l = se.array_to_list
local l = se.list


local class = {
   parameterize = comp.parameterize,
}

-- Bind macros to state object for gensym.
class.macro = {} ; do
   for name, m in pairs(require('lure.scheme_macros')) do
      -- log("MACRO: " .. name .. "\n")
      class.macro[name] = function(s, expr)
         return m(expr, { state = s, void = '#<void>' })
      end
   end
end


local function s_id(s, thing) return thing end

class.expander = {
   ['string'] = s_id,
   ['number'] = s_id,
   ['pair'] = function(s, expr)
      local car, cdr = unpack(expr)
      local m = s.macro[car]
      if m ~= nil then
         return m(s, expr), 'again'
      else
         return expr
      end
   end
}

local function trace(tag, expr)
   log('\n') ; log_se_n(expr, tag .. ": ")
end

function class.expand_step(s, expr)
   trace("STEP",expr)
   local typ = se.expr_type(expr)
   local f = s.expander[typ]
   if f == nil then error('expand: bad type ' .. typ) end
   return f(s, expr)
end

function class.expand(s, expr)
   local again = 'again'
   while again do
      assert(again == 'again')
      expr, again = s:expand_step(expr)
   end
   return expr
end

local void = '#<void>'

-- Note that primitive form evaluation and macro expansion are
-- intertwined.  Gives more compact code.

class.form = {
   -- ['block'] = function(s, expr)
   --    local _, bindings, forms = se.unpack(expr, {n = 2, tail = true})
   --    local function tx_binding(binding)
   --       local var, expr = comp.unpack_binding(binding, void)
   --       trace('BINDING',var)
   --       local cexpr = s:compile(expr)
   --       local rvar = s:rename_def(var)
   --       return l(rvar, cexpr)
   --    end
   --    local function tx_form(seqform)
   --       return s:compile(seqform)
   --    end
   --    return {'block',
   --            {se.map(tx_binding, bindings),
   --             se.map(tx_form, forms)}}
   -- end,
   -- This is like 'begin', but without support for local definitons.
   ['sequence'] = function(s, expr)
      local _, forms = se.unpack(expr, {n = 1, tail = true})
      local function tx_form(seqform)
         return l('_', s:compile(seqform))
      end
      return l('block',se.map(tx_form, forms))
   end,
   ['set!'] = function(s, expr)
      local _, var, vexpr = se.unpack(expr, {n = 3})
      return s:anf(
         l(vexpr),
         function(e) return l('set!', s:rename_ref(var), se.car(e)) end)
   end,
   ['if'] = function(s, expr)
      local _, econd, etrue, efalse = se.unpack(expr, {n = 4})
      return s:anf(
         l(econd),
         function(e) return l('if', se.car(e), s:compile(etrue), s:compile(efalse)) end)
   end,
   ['lambda'] = function(s, expr)
      local _, src_names, body = se.unpack(expr, {n = 2, tail = true})
      local vars = se.map(function(v) return s:rename_def(v) end, src_names)
      local unique_names = se.map(function(v) return v.renamed end, vars)
      return l('lambda', unique_names,
               s:compile_extend({'begin',body}, vars))
   end
}

-- Compile in extended environment
function class.compile_extend(s, expr, vars)
   local new_env = s.env
   for var in se.elements(vars) do
      new_env = {var, new_env}
   end
   s:parameterize(
      {env = new_env},
      function()
         s:compile(expr)
      end)
end


function class.anf(s, exprs, fn)
   local normalform = {}
   local bindings = {}
   for e in se.elements(exprs) do
      -- FIXME: This should compile before checking if it's primitive.
      if type(e) == 'string' then
         ins(normalform, s:rename_def(e))
      elseif type(e) == 'table' then
         -- Composite
         local sym = s:gensym()
         ins(bindings, l(sym, s:compile(e)))
         ins(normalform, sym)
      else
         -- Other values
         ins(normalform, e)
      end
   end
   if #bindings == 0 then
      return fn(a2l(normalform))
   else
      return l('block',
               a2l(bindings),
               fn(a2l(normalform)))
   end
end

local function apply(s, expr)
   trace("APPLY", expr)
   return s:anf(expr, function(e) return e end)
end

class.compiler = {
   ['string'] = s_id,
   ['number'] = s_id,
   ['pair'] = function(s, expr)
      local car, cdr = unpack(expr)
      local f = s.form[car]
      if f ~= nil then
         return f(s, expr)
      else
         return apply(s, expr)
      end
   end
}
function class.compile(s, expr)
   expr = s:expand(expr)
   trace("COMPILE",expr)
   local typ = se.expr_type(expr)
   local f = s.compiler[typ]
   if f == nil then error('compile: bad type ' .. typ) end
   return f(s, expr)
end

function class.gensym(s, prefix)
   -- Gensyms should never clash with source variables.  We can't
   -- guarantee that atm.  FIXME.
   s.count = s.count + 1
   local sym = (prefix or "r") .. s.count
   return sym
end

-- Renames definitions and references.
function var_iolist(var)
   assert(var.var)
   assert(var.rename)
   return {"#<var:",var.var,":",var.rename,">"}
end
function make_var(s, name)
   local sym = s:gensym()
   return { var = name, rename = sym, class = 'var', iolist = var_iolist }
end
function class.rename_def(s, name)
   assert(type(name) == 'string')
   -- Always rename definitions.
   return make_var(s, name)
end
function is_var(var)
   if (type(var) ~= 'string') then
      assert(type(var) == 'table' and var.class == 'var')
      return var
   else
      return false
   end
end

function class.rename_ref(s, var)
   -- Make it idempotent for var objects.
   if (is_var(var)) then return var end
   -- If it's a string, it's always going to be a source symbol, which
   -- has to be renamed.  First check lexical variables in
   -- environment.
   local name = var
   for v in se.elements(s.env) do
      if name == v.var then
         return v
      end
   end
   -- References to non-lexical variables are stored in a separate
   -- table.  We create those on demand, and have to re-use.
   local v = s.globals[name]
   if not v then
      v = make_var(s, name)
      s.globals[name] = v
   end
   return v
end



function class.new()
   local obj = { count = 0, globals = {}, env = {} }
   setmetatable(obj, { __index = class })
   return obj
end


return class

