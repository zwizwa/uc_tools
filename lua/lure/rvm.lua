input = "Detouq,htgnel-gnirts,fer-gnirts,fi,!rdc-tes,tsil>-rotcev,!tes-gnirts,enifed,!tes-rotcev,?rotcev,=,cc/llac,!tes,adbmal,rddc,gnirts-ekam,fer-rotcev,htgnel-rotcev,rotcev-ekam,lobmys>-gnirts,gnirts>-lobmys,?erudecorp,!rac-tes,tneitouq,enilwen,ton,lave,fer-tsil,rdddac,*,?tcejbo-foe,?lobmys,lper,?gnirts,rotcev>-tsil,+,etirw,rahc-keep,yalpsid,tsil>-gnirts,daer,gnirts>-tsil,?lauqe,,,,?llun,htgnel,,,,,rddac,rdac,-,,,<,,rac,?riap,,rahc-daer,rdc,,snoc,,?vqe,,,,,;8K!K8K@Z%@YGZ#^'i$~YM^YC@PvCvR3y]#7#YS*^z!S*9Bi&:EiS/ai&kkz!S/:kw'k]@'_*Z@aC_G^~F^{!>'^8>YHlbC`^'`~?_G_~F_|]D9C`^Uka_CaG`.ZDdCbAai$G`^~F_|!S+#`kn3^~i$#`kn3^~i$#`kn3^~i$#`kn3^~RJ^~?w)B^~?kH^~R^z]K#YS+a_l{]C#a_k#k_k~?iS/_{!.#b`n9DAd`Ca_#ZCex>#d~TbZBi&:EiS/NeZ@AAfi$i$akS_nM`~?x0^.:EgYOecEfNdboMa_~?x:^.ZKdUlbMbNa_~O?x6_9DAd`Ca_#ZCex>#d~TbZBi&:EiS/NeZ@AAfi$i$akS_nM`~?x0^.:EgYOecEfNdboMa_~?x:^.ZKdUlbMbNa_~O^~^?x1^#cMan~?x=^G_~F_#bUk``m~YL_|!94_@K^{]%4uy]?'i$9?C_@K^G^~F^z]I'i$'i$9IC^@YGG^~F^@KvC~F^z!E8EYS(^89vS7vF~Z(^9?YD^~YJ^8EZ)^~YL^4vL@ZIC^@YGG^@KvK~F^89vLvK~T^89vS;vF~?i%^89vS-vF~Z$^z!G8E^4vE@Z?i%YD^@KvE~YJ^z]O9O8@~?u^'^~Ik^Dy!@8@@D'^9O~?vR0^~I_vC'iS0~YM^YFy!?*V^@D'i&~OOIvD`*V^@D'i&~OO^~^?vL_*V^@D'i&~O^~^?vK^YFy]M*ZM^YC'i&@D~?vL^Wy!C9*`'^~^^YS%^YBAV^@D*Ai&YCx=@D~?vJ^8IYC'i%@D~?vS;^'i$@D~?vS-^YF@D~?vF^9M@D~?vK^'^~Ik^Wy!F'^!S-^Dy]H'^!S-iS.'^~?iS0^!S-^z!-9H^9HYS#~?iS.^'^~?iS0^iS-y!S-iS.!M(iS0^z]27%Z>'_@YS&Jc^@YS'Hc^BBZ>i$zBBZ>i$z]B#l`^{](Ql]+8IZLk^z]59Nb`H^|]-8P`H^{],i+]8i1!I#oS_^z]4Qo].8BZLvC^z]79Nb`H^|];8P`H^{]<i+!Di1!B#nS_^z!JQn]F'_'i$'i$9FLLvR%`YNbuC_~IvR/^~I_vR$G^~F^{]G9Fk^'i$~T^z!S%'i$5_k~^ZG^9GC^~?vPG^'i$~T^YD^z]E'^9E_`~IakAb^YHLYNu``vR%Z&u^{!S(8BZEi&^8BAZEi&L`kvP~Ik^z]3i(@YS)ki#!S,Bi#]P'^!S,AiS,^YS$^9PBa_'^~YA`B^H_~F_{]*9PiS,^z])i+!S$#m_i$z!LQm]J'`9JAca`Ll^~I_k|]L9Ji&`^{]A'^9ALl`C^~I`k{]N9'aZA`^|!P0ZA`^{!<'k8HSC_l~F^z!=(i&^z!O87B^z!76B^z]/+B^z!61B^z]9iS)]'iS'!,i+!0i1!*#k`^{!/Qk!A'i$'i$'i$'i$8AHaH_~YABaB_~YAJaJ_~R`'i$~?pJ_~R_'^~^?`^{]$(i$^z!:9>'i$(bJ^~R^zz!S.Lmk!S0Llk!':lkl!):lkm!8:lkn]>:lko!;:lkp!1:lkq!+:lkr!3:lks!S':lkt!S):lku!S&:lkv.!(:lkv/!2:lkv0!H:lkv1!5:lkv2!N:lkv3]&:lkv4!S#:lkv5!4:lkv6y"

-- Transliteration to Lua from Python.  Python code commented out for reference.
-- Main difficulty is that Lua uses 1-based array indexing.
-- I'm going to simply add a +1 to all array references instead of adding 1 to each constant.
-- Giving this a name so it is easer to search for. The name 'base' was not used.
local base = 1


-- import sys

-- stdo=sys.stdout
local stdo = io.stdout

-- putchar=lambda c:[stdo.write(chr(c)),stdo.flush(),c][2]
local function putchar(c) stdo:write(string.char(c)); stdo:flush(); return c end

local push

-- def getchar():
--  c=sys.stdin.read(1)
--  push(ord(c) if len(c) else -1)
local function getchar()
   push(io.stdin:read(1):byte(base) or -1)
end

-- -- debug = False #debug#
local debug = false  --debug--

local function chars2str(s) if is==NIL then return "" else return chr(s[0+base]) .. chars2str(s[1+base]) end end --debug--
local function sym2str(s) return chars2str(s[1+base][0+base]) end --debug--
local function str(o) return "" .. o end --debug--

-- local function is_rib(x)  return type(x) == 'table' and x.class == 'rib' end  -- FIXME
local function is_rib(x)  return type(x) == 'table' end  -- FIXME

local pair_type = 0
local procedure_type = 1
local symbol_type = 2
local string_type = 3
local vector_type = 4
local singleton_type = 5

local function instance(typ)
   return function(x)
      return is_rib(x) and typ == x[2+base]
   end
end
local is_pair = instance(pair_type)


local function show_opnd(o)        --debug--
   if is_rib(o) then               --debug--
      return 'sym ' .. sym2str(o)  --debug--
   else                            --debug--
      return 'int ' .. str(o)      --debug--
   end
end

-- stack=0
local stack=0

local function show_stack() --debug--
   -- local s = stack --debug--
   -- local r = {} -- debug --
   -- while is_rib(s[2+base]) do table.insert(r, s[0+base]); s = s[1+base]; end --debug--
   -- log_desc(r) --debug-
end

-- pos=-1
-- def get_byte():
--  global pos
--  pos+=1
--  return ord(input[pos])

local pos=0
local function get_byte()
   pos = pos+1
   local int = input:byte(pos) or 0
   -- log_desc({'get_byte',pos,int})
   return int
end

-- VM

--FALSE=[0,0,5]
--TRUE=[0,0,5]
--NIL=[0,0,5]
local function rib(a,b,c)
   assert(a)
   assert(b)
   assert(c)
   return {
      class = 'rib', -- FIXME: bootstrap
      [1]=a,[2]=b,[3]=c
   }
end


local FALSE=rib(0,0,5)
local TRUE=rib(0,0,5)
local NIL=rib(0,0,5)

--to_bool=lambda x:TRUE if x else FALSE
--is_rib=lambda x:type(x) is list
local function to_bool(x)
   if x then
      return TRUE
   else
      return FALSE
   end
end


--def push(x):
-- global stack
-- stack=[x,stack,0]
push = function(x)
   stack = rib(x,stack,0)
end

--def pop():
-- global stack
-- x=stack[0]
-- stack=stack[1]
-- return x
local function pop()
   local x = stack[0+base]
   stack = stack[1+base]
   assert(x)
   return x
end

-- Agrument evaluation order is undefined in Lua, vs. Python left-to-right.
-- Since we need to be explicit anyway, we use normal argument order for f,
-- as opposed to Python RVM which has them reversed.
local function prim1(f) return function() local a=pop(); push(f(a)) end end
local function prim2(f) return function() local b=pop();local a=pop(); push(f(a,b)) end end
local function prim3(f) return function() local c=pop();local b=pop(); local a=pop(); push(f(a,b,c)) end end


local function arg2() local x = pop(); pop(); push(x) end
local function close() push(rib(pop()[0+base],stack,1)) end
local function f0s(x,y) x[0+base]=y; return y; end
local function f1s(x,y) x[1+base]=y; return y; end
local function f2s(x,y) x[2+base]=y; return y; end

local primitives = {
   prim3(rib), -- 0
   prim1(function(x) return x end), -- 1
   pop, -- 2
   arg2, -- 3
   close, -- 4
   prim1(function(x) return to_bool(is_rib(x)) end), -- 5
   prim1(function(x) return x[0+base] end), -- 6
   prim1(function(x) return x[1+base] end), -- 7
   prim1(function(x) return x[2+base] end), -- 8
   prim2(f0s), -- 9
   prim2(f1s), -- 10
   prim2(f2s), -- 11
   prim2(function(x,y) return to_bool(x == y) end), -- 12
   prim2(function(x,y) return to_bool(x<y) end), -- 13
   prim2(function(x,y) return x + y end), -- 14
   prim2(function(x,y) return x - y end), -- 15
   prim2(function(x,y) return x * y end), -- 16
   prim2(function(x,y) return math.floor(x / y) end),  -- 17  -- FIXME
   getchar, -- 18
   prim1(putchar) -- 19
}



--def get_code():
-- x=get_byte()-35
-- return 57 if x<0 else x

local function get_code()
   local x = get_byte() - 35
   if x<0 then
      return 57
   else
      return x
   end
end

--def get_int(n):
-- x=get_code()
-- n*=46
-- return n+x if x<46 else get_int(n+x-46)

local function get_int(n)
   local x = get_code()
   n = n * 46
   if x<46 then
      return n+x
   else
      return get_int(n + x -46)
   end
end


-- list_tail=lambda lst,i:lst if i==0 else list_tail(lst[1],i-1)
local function list_tail(lst, i)
   if i==0 then
      return lst
   else
      return list_tail(lst[1+base],i-1)
   end
end



-- build the initial symbol table

-- symtbl=NIL
-- n=get_int(0)
-- while n>0:
--  n-=1
--  symtbl=[[0,[NIL,0,3],2],symtbl,0]

-- accum=NIL
-- n=0
-- while 1:
--  c=get_byte()
--  if c==44:
--   symtbl=[[0,[accum,n,3],2],symtbl,0]; accum=NIL; n=0
--  else:
--   if c==59: break
--   accum=[c,accum,0]
--   n+=1

-- symtbl=[[0,[accum,n,3],2],symtbl,0]


local n -- used a couple of times below

local symtbl


-- { "get_byte", 456, 59,  }
-- end build_symtbl
-- symtbl-end
-- { "get_byte", 457, 56,  }


local function build_symtbl()
   symtbl = NIL
   n = get_int(0)
   while n>0 do
      n = n - 1
      -- log_desc({n=n,pos=pos})
      symtbl = rib(rib(0,rib(NIL,0,3),2),symtbl,0)
   end

   local accum = NIL
   n=0
   while true do
      local c = get_byte()
      if c == 44 then
         symtbl = rib(rib(0,rib(accum,n,3),2),symtbl,0)
         accum = NIL
         n = 0
      else
         if c == 59 then break end
         accum = rib(c, accum, 0)
         n = n + 1
      end
   end

   symtbl=rib(rib(0,rib(accum,n,3),2),symtbl,0)

   return symtbl
end

local function car(p) return p[1] end
local function cdr(p) return p[2] end
-- local function cons(a,d) return rib(a,d,0) end -- pair-type
-- local function set_car(p,v) p[1] = v end

local function sym(n)
   return car(list_tail(symtbl,n))
end



-- make_symboltbl()



local function run()

--symtbl=[[0,[accum,n,3],2],symtbl,0]
--symbol_ref=lambda n: list_tail(symtbl,n)[0]


end

-- while 1:
--  x=get_code()
--  n=x
--  d=0
--  op=0
--  while 1:
--   d=[20,30,0,10,11,4][op]
--   if n<=2+d:break
--   n-=d+3;op+=1
--  if x>90:
--   n=pop()
--  else:
--   if op==0:stack=[0,stack,0];op+=1
--   n = get_int(0)if n==d else symbol_ref(get_int(n-d-1))if n>=d else symbol_ref(n)if op<3 else n
--   if 4<op:
--    n=[[n,0,pop()],0,1]
--    if not stack:break
--    op=4
--  stack[0]=[op-1,n,stack[0]]

-- pc = n[0][2]

-- get_opnd=lambda o:(o if is_rib(o) else list_tail(stack,o))

-- def get_cont():
--  s=stack
--  while not s[2]:s=s[1]
--  return s

-- def set_global(val):
--  global symtbl
--  symtbl[0][0]=val
--  symtbl=symtbl[1]

-- set_global([0,symtbl,1]) # primitive 0
-- set_global(FALSE)
-- set_global(TRUE)
-- set_global(NIL)

-- stack=[0,0,[5,0,0]] # primordial continuation (executes halt instr.)

local rt = require('lure.slc_runtime')

local se = require('lure.se')
function convert(obj)
   if obj == FALSE then
      return false
   elseif obj == TRUE then
      return true
   elseif obj == NIL then
      return se.empty
   elseif not is_rib(obj) then
      -- log_desc({obj=obj})
      return obj
   else
      local typ = obj[2+base]
      -- log_desc({typ=typ})
      if typ == pair_type then
         -- return '#<pair>'
         return se.cons(convert(car(obj)), convert(cdr(obj)))
      elseif typ == procedure_type then
         return "<procedure>"
      elseif typ == symbol_type then
         -- return "<symbol>"
         -- return rt['string->symbol'](convert(obj[1+base]))
         return convert(obj[1+base])
      elseif typ == string_type then
         return rt['list->string'](se.map(rt['integer->char'],convert(obj[0+base])))
         -- return convert(obj[0+base])
      elseif typ == vector_type then
         return se.list('vector', convert(obj[0+base]))
      else
         return "<unknown>"
      end
   end
end

local function trace_instruction(name, opnd, stack)
   if not debug then return end

   function show_stack(stack)
      if is_pair(stack) then
         return se.cons(convert(car(stack)),
                        show_stack(cdr(stack)))
      else
         if stack == 0 then
            return se.empty
         else
            return se.list(
               'stack:', show_stack(stack[0+base]),
               'pc: ', convert(stack[2+base]))
         end
      end
   end

   log(": ")
   log(name)
   log(",")
   --if opnd then -- FIXME, if?
      log_w(se.iolist(convert(opnd)))
      log(",")
   --end
   log_w(se.iolist(show_stack(stack)))
   log("\n")

end




local function symbol_ref(n)
   return list_tail(symtbl,n)[0+base]
end

function decode_loop()
while true do
 local x=get_code()
 -- log_desc({get_code=x})
 local n=x
 local d=0
 local op=0
 while true do
  local ds={20,30,0,10,11,4}
  d=ds[op+base]
  if n<=2+d then break end
  n=n-(d+3) ; op=op+1
 end
 if x>90 then
  n=pop()
 else
  if op==0 then
     stack=rib(0,stack,0);
     op = op + 1
  end
  if n==d then
     n = get_int(0)
  elseif n>= d then
     n = symbol_ref(get_int(n-d-1))
  elseif op<3 then
     n = symbol_ref(n)
  end
  -- log_se_n(convert(n))
  if 4<op then
   n=rib(rib(n,0,pop()),0,1)
   if not is_rib(stack) then
      return n
   end
   op=4
  end
 end
 stack[0+base]=rib(op-1,n,stack[0+base])
end

end

-- function decode1()
--    local stack = 0
--    build_symtbl()

--    local function add_instruction(op, opnd, stack)
--       set_car(stack, rib(op, opnd, car(stack)))
--    end

--    while true do
--       local x = get_code()

--       local n = x
--       local d = 0
--       local op = 0
--       -- This part is taken from Python code.  Scheme code uses mixed
--       -- tail call loops that can't be expressed in Lua.
--       while true do
--          log_desc({op=op})
--          local t = {20,30,0,10,11,4}
--          d=t[op+base]
--          assert(d)
--          if n<=2+d then break end
--          n = n - (d+3)
--          op = op + 1
--       end
--       -- This part is transliteration of Scheme code, with non-local
--       -- return.
--       if 90 < x then
--          add_instruction(4, car(stack), cdr(stack)) -- if
--       else
--          if op == 0 then
--             stack = cons(0,stack)
--          end
--          local opnd
--          if (n < d) then
--             if op < 3 then
--                opnd = sym(n)
--                opnd = n
--             else
--                if (n == d) then
--                   opnd = get_int(0)
--                else
--                   opnd = sym(get_int(n - d - 1))
--                end
--             end
--          end
--          if 4 < op then
--             local proc = rib(rib(opnd,0,car(stack)),
--                              NIL,1) -- procedure-type
--             stack = cdr(stack)
--             if is_rib(stack) then
--                add_instruction(3, -- const-proc
--                                proc,
--                                stack)
--             else
--                return proc
--             end
--          else
--             local ins
--             if 0 < op then
--                ins = op - 1
--             else
--                ins = 0
--             end
--             add_instruction(ins, opnd, stack)
--          end
--       end
--    end
-- end

-- local pc

-- local function decode(n)

-- while true do
--    local x = get_code()
--    log_desc({get_code = x})
--    local n = x
--    local d = 0
--    local op = 0
--    while true do
--       local lookup = {20,30,0,10,11,4}
--       d=lookup[op+base]
--       if n<=2+d then break end
--       n = n - d+3
--       op = op + 1
--    end
--    if x >90 then
--       n=pop()
--    else
--       if op==0 then
--          stack={0,stack,0}
--          op = op + 1
--       end
--       if n == d then
--          n = get_int(0)
--       elseif n>=d then
--          n = symbol_ref(get_int(n-d-1))
--       elseif op<3 then
--          if op<3 then
--             n = symbol_ref(n)
--          end
--       end
--       if 4<op then
--          n=rib(rib(n,0,pop()),0,1)
--          if not stack then break end
--          op=4
--       end
--    end

--    stack[0+base]=rib(op-1,n,stack[1])

-- end

-- end



local get_opnd = function(o)
   if is_rib(o) then
      return o
   else
      return list_tail(stack,o)
   end
end

local function get_cont()
   -- log_desc({'get_cont'})
   local s = stack
   while not is_rib(s[2+base]) do
      s = s[1+base]
   end
   return s
end

local function set_global(val)
   symtbl[0+base][0+base]=val
   symtbl=symtbl[1+base]
end



local function decode()
   build_symtbl()
   local main_proc = decode_loop()

   set_global(rib(0,symtbl,1)) -- procedure type, primitive 0
   set_global(FALSE)
   set_global(TRUE)
   set_global(NIL)

   return main_proc
end

-- stack=rib(0,0,rib(5,0,0)) -- primordial continuation (executes halt instr.)

-- pc = n[0+base][2+base]  ???


local function run()
   local count = 0
   local n = decode()
   local pc = n[0+base][2+base]
   stack=rib(0,0,rib(5,0,0)) -- primordial continuation (executes halt instr.)

   while true do
      count = count + 1
      local o=pc[1+base]
      local i=pc[0+base]
      -- log_desc({count,i})
      if i<1 then -- jump/call
         trace_instruction("jump/call",o,stack)
         o=get_opnd(o)[0+base]
         c=o[0+base]
         if is_rib(c) then
            local c2=rib(0,o,0)
            local s2=c2
            nargs=c[0+base]
            while nargs > 0 do
               s2=rib(pop(),s2,0)
               nargs=nargs-1
            end
            if is_rib(pc[2+base]) then -- call
               c2[0+base]=stack
               c2[2+base]=pc[2+base]
            else -- jump
               k=get_cont()
               c2[0+base]=k[0+base]
               c2[2+base]=k[2+base]
            end
            stack=s2

         else
            -- log_desc({'prim',c})
            primitives[c+base]()
            if is_rib(pc[2+base]) then -- call
               c=pc
            else --  jump
               c=get_cont()
               stack[1+base]=c[0+base]
            end
         end
         pc=c[2+base]

      elseif i<2 then -- set
         trace_instruction("set",o,stack)
         x=pop()
         get_opnd(o)[0+base]=x
         pc=pc[2+base]

      elseif i<3 then -- get
         trace_instruction("get",o,stack)
         push(get_opnd(o)[0+base])
         pc=pc[2+base]

      elseif i<4 then -- const
         trace_instruction("const",o,stack)
         push(o)
         pc=pc[2+base]

      elseif i<5 then -- if
         trace_instruction("if",o,stack)
         local index
         if pop() == FALSE then
            index = 2
         else
            index = 1
         end
         pc=pc[index+base]

      else -- halt
         break
      end
   end

end

local function get_byte_or_read_char()
   if pos < #input then
      return get_byte()
   else
      local c = io.stdin:read(1)
      return c:byte(1) or -1
   end
end

-- local function get_int(n)
--    local x = get_code()
--    local y = n * 46
--    if x < 46 then
--       return y + x
--    else
--       return get_int(y + (x - 46))
--    end
-- end

return {
   ['primitives-lua'] = primitives,
   input = input,
   putchar = putchar,
   getchar = getchar,
   ['get-input-byte'] = get_byte,
   ['_false'] = FALSE,
   ['_true'] = TRUE,
   ['_nil'] = NIL,
   ['build-symtbl-lua'] = build_symtbl,
   ['_rib'] = rib,
   ['get-byte-or-read-char'] = get_byte_or_read_char,
   ['get-code'] = get_code,
   ['get-int'] = get_int,
   ['sym'] = sym,
   ['decode-lua'] = decode,
   ['run-lua'] = run,
   ['trace-instruction'] = trace_instruction,
}
