-- Shared language front-end
-- Preprocessor that performs the following:
--
-- MACRO EXPAND
-- BETA REDUCTION
-- A-NORMAL FORM
-- VARIABLE RENAMING
--
-- Separate later passes:
--
-- BLOCK FLATTENING
--
-- RETURN / BLOCK RESULT
--
-- OUTPUT CLEANUP / LANG PPRINT
--
-- Beta reduction performed before ANF.
-- Variable renaming enables later block-flattening.

-- FIXME: This is still written in old se.unpack() style, and might
-- benefit from using the pattern matcher.  However, it works, so not
-- touching it for now.

local se = require('lure.se')
local tab = require('lure.tab')
local comp = require('lure.comp')

require('lure.log_se')

local ins = table.insert
local a2l = se.array_to_list
local l = se.list

local function trace(tag, expr)
   -- log('\n') ; log_se_n(expr, tag .. ": ")
end

local class = {
   parameterize = comp.parameterize,
}

local void = {
   class = "void",
   iolist = '#<void>'
}
class.void = void

-- Bind macros to state object for gensym.
local scheme_macros = require('lure.scheme_macros')
class.macro = {} ; do
   for name, m in pairs(scheme_macros) do
      -- log("MACRO: " .. name .. "\n")
      class.macro[name] = function(s, expr)
         local config = { state = s, void = void }
         tab.copy(s.config, config)
         return m(expr, config)
      end
   end
end

-- Create new macro as parameterized macro from scheme_macros
local function config_macro(new_name, old_name, config)
   local m = scheme_macros[old_name] ;  assert(m)
   class.macro[new_name] = function(s, expr)
      local c = { state = s, void = void }
      for k,v in pairs(config) do c[k]=v end
      return m(expr, c)
   end
end

-- Behavior is same, just trickle down to call module-set! instead of
-- set! for module definitions.
config_macro('module-begin',  'begin',  { letrec = 'module-letrec' })
config_macro('module-letrec', 'letrec', { set    = 'module-set!'   })

class.macro['module-set!'] = function(s, expr)
   local _, name, expr = se.unpack(expr, {n=3})
   return l('begin',
            l('set!',name,expr),
            l('module-register!',l('quote',name), name)
            )
end


local function s_id(s, thing) return thing end

class.expander = {
   ['string'] = s_id,
   ['number'] = s_id,
   ['void']   = s_id,
   ['prim']   = s_id,
   ['expr']   = s_id,
   ['var']    = s_id,
   ['pair']   = function(s, expr)
      local car, cdr = unpack(expr)
      local m = s.macro[car]
      if m ~= nil then
         return m(s, expr), 'again'
      else
         return expr
      end
   end
}

function class.expand_step(s, expr)
   trace("STEP",expr)
   local typ = se.expr_type(expr)
   local f = s.expander[typ]
   if f == nil then
      log_se_n(expr, "BAD_EXPAND:")
      error('expand: bad type ' .. typ)
   end
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
-- intertwined.  Gives more compact code.  Convention is that the form
-- compilers return a primitive form, which is one of
local prim_out_forms = {'block','set!','if','lambda'}

local function quote_to_iolist(q)
   return {"'", se.iolist(q.expr)}
end

class.form = {
   -- This is like 'begin', but without support for local definitons.
   ['primitive-begin'] = function(s, expr)
      local _, forms = se.unpack(expr, {n = 1, tail = true})
      local function tx_form(seqform)
         return l('_', s:comp(seqform))
      end
      return {'block',se.map(tx_form, forms)}
   end,
   ['set!'] = function(s, expr)
      local _, var, vexpr = se.unpack(expr, {n = 3})
      return s:anf(
         l(vexpr),
         function(e) return l('set!', s:var_ref(var), se.car(e)) end)
   end,
   ['if'] = function(s, expr)
      local _, econd, etrue, efalse = se.unpack(expr, {n = 4})
      return s:anf(
         l(econd),
         function(e) return l('if', se.car(e), s:comp(etrue), s:comp(efalse)) end)
   end,
   ['lambda'] = function(s, expr)
      local _, src_names, body = se.unpack(expr, {n = 2, tail = true})
      local vars = se.map(
         function(src_name)
            return s:var_def(src_name)
         end,
         src_names)
      return l('lambda', vars,
               s:comp_extend(
                  {'begin',body},
                  vars))
   end,
   ['quote'] = function(s, expr)
      local _, datum = se.unpack(expr, {n = 2})
      return { class = 'expr', expr = datum, iolist = quote_to_iolist }
   end,
   ['hint'] = function(s, expr)
      -- Same evaluation as function call, but tagged differently.
      return {'hint', se.cdr(s:apply(se.cdr(expr)))}
   end,

}

-- Compile in extended environment
function class.comp_extend(s, expr, vars)
   local new_env = s.env
   for var in se.elements(vars) do
      new_env = {var, new_env}
   end
   return s:parameterize(
      {env = new_env},
      function()
         -- log_se_n(expr, "COMPILE_EXTEND:")
         return s:comp(expr)
      end)
end

function is_var(var)
   return type(var) == 'table' and var.class == 'var'
end
local function is_prim(expr)
   -- variable
   if is_var(expr) then return false end
   local typ = type(expr)
   if typ == 'table' then
      -- abstract object
      if typ.class then return true
      -- expression
      else return false end
   end
   -- anything else
   return true
end

-- FIXME: This should at least expand to expose quotation.

-- Convention is that fn returns a primitive output form.
function class.anf(s, exprs, fn)
   local normalform = {}
   local bindings = {}
   for e1 in se.elements(exprs) do
      local e = s:expand(e1) -- exposes 'quote'
      trace("ANF", e)

      -- FIXME: This should compile before checking if it's primitive.
      if type(e) == 'string' then
         -- Source variable
         -- ins(normalform, l('ref', s:var_ref(e)))
         ins(normalform, s:var_ref(e))
      elseif is_var(e) then
         -- Abstract variable reference (is this actually possible here?)
         ins(normalform, e)
      elseif is_prim(e) then
         ins(normalform, e)
      elseif se.is_expr(e, 'quote') then
         ins(normalform, s:comp(e))
      else
         if type(e) ~= 'table' then
            error("bad type '" .. type(e) .. "'")
         end
         -- Composite.  Bind it to a variable.  The name here is just
         -- for debugging.
         local var = s:var_def()
         ins(bindings, l(var, s:comp(e)))
         ins(normalform, var)
      end
   end
   if #bindings == 0 then
      return fn(a2l(normalform))
   else
      ins(bindings, l('_',fn(a2l(normalform))))
      return {'block', a2l(bindings)}
   end
end

function class.apply(s, expr)
   local fun, args = unpack(expr)
   -- Expand to expose lambda expressions.
   fun = s:expand(fun)
   -- Perform beta reduction before obscuring these applications into
   -- ANF.  We could use a separate 'let' special form, but doing it
   -- here catches more cases.
   if se.expr_type(fun) == 'pair' and se.car(fun) == 'lambda' then
      trace("BETA", expr)
      local _, fargs, fbody = se.unpack(fun, {n=2,tail=true})
      local bindings = se.zip(
         function(farg, arg)
            return l(s:var_def(farg), s:comp(arg))
         end,
         fargs, args)
      local vars = se.map(se.car, bindings)
      local cexp = s:comp_extend({'begin',fbody}, vars)
      return {'block',se.append(bindings, l(l('_', cexp)))}
   else
      -- Ordinary application.
      trace("APPLY", expr)
      return s:anf(expr, function(e) return {'app', e} end)
   end
end

class.compiler = {
   ['number'] = s_id,
   ['void']   = s_id,
   ['prim']   = s_id,
   ['expr']   = s_id,
   ['string'] = function(s, str)
      local var = s:var_ref(str)
      assert(var and var.class == 'var')
      return var
   end,
   ['pair'] = function(s, expr)
      local car, cdr = unpack(expr)
      assert(car)
      assert(cdr)
      local f = s.form[car]
      if f ~= nil then
         return f(s, expr)
      else
         return s:apply(expr)
      end
   end
}
function class.comp(s, expr0)
   local expr = s:expand(expr0)
   trace("COMPILE",expr)
   local typ = se.expr_type(expr)
   local f = s.compiler[typ]
   if f == nil then
      log_se_n(l(expr,expr0),"BAD_COMP:")
      error('compile: bad type ' .. typ)
   end
   return f(s, expr)
end

-- Entry point
function class.compile(s, expr)
   s:init()

   -- Body is parameterized by a function that is used to perform
   -- symbol lookup for all free variables.
   s.lib_ref = s:var_def('lib-ref')
   local top_args = l(s.lib_ref)
   local body = s:comp_extend(expr,top_args)

   local bs = l(l('_', body))
   -- Compile the module bindings in a loop, since new module bindings
   -- might be introduced during compilation.
   local did
   local have = {}
   local i = 1
   repeat
      did = false
      -- log("modbind " .. i .. "\n")
      for src_name, binding in pairs(s.module_bindings) do
         assert(type(src_name) == 'string')
         -- src_name is either gensym or global, so always unique
         if not have[src_name] then
            have[src_name] = true
            -- log_w(" - ", src_name, "\n")
            local top_var, top_expr = se.unpack(binding, {n = 2})
            local b = l(top_var, s:comp(top_expr))
            bs = {b, bs}
            did = true
         end
      end
      i = i + 1
   until (not did)
   return l('lambda',top_args,{'block', bs})
end

-- All variables are renamed, so gensyms will never clash with anything.
function class.gensym(s)
   s.count = s.count + 1
   local prefix = "r"
   local sym = prefix .. s.count
   return sym
end

-- Insert module-level definition before evaluation of body code.
-- These should all be independent, so we don't care about order.
function class.module_define(s, src_name, expr)
   -- Just register now.  Compile after compiling body.
   assert(type(src_name) == 'string')
   local var = s:var_def(src_name)
   assert(not s.module_bindings[src_name])
   s.module_bindings[src_name] = l(var,expr)
   return var
end

-- Renames definitions and references.
function var_iolist(var)
   assert(var.unique)
   local orig = {".",var.var}
   if nil == var.var then orig = "" end
   return {var.unique,orig}
end
class.var_iolist = var_iolist

function class.make_var(unique, name)
   assert(unique)
   -- No longer support variables in the input: if IR is fed into the
   -- frontend, strip all vars first.
   if name ~= nil and type(name) ~= 'string' then
      log_se_n(name, "BAD_VAR_NAME:")
      error('source var name not a string')
   end
   return { var = name, unique = unique, class = 'var', iolist = var_iolist }
end

function class.var_def(s, name)
   -- Name is allowed to be nil for temporary variables.  They only
   -- have their uinique name.
   local sym = s:gensym()
   return s.make_var(sym, name)
end


function class.var_ref(s, var)
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

   -- Module level variables are handled separately.
   local binding = s.module_bindings[name]
   if binding then return se.car(binding) end

   -- Free variables are mapped to a symbol lookup expression inserted
   -- at the head of the module.  We need to keep track of them so
   -- they always map to the same lexical variable.
   local v = s.free_variables[name]
   if not v then
      v = s:module_define(name, l(s.lib_ref,l('quote',name)))
      s.free_variables[name] = v
   end
   return v
end

-- Initialize state before running compiler.
function class.init(s)
   s.count = 0
   s.free_variables = {}
   s.env = {}
   s.module_bindings = {}
   s.lib_ref = nil
end

function class.new(config)
   local obj = { config = config }
   -- log_desc({scheme_frontend_config = config})
   setmetatable(obj, { __index = class })
   obj:init()
   return obj
end


return class
