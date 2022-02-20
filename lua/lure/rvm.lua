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

-- debug = False #debug#
local debug = false

-- sym2str = lambda s:chars2str(s[1][0]) #debug#
-- chars2str = lambda s:"" if s is NIL else chr(s[0])+chars2str(s[1]) #debug#
-- show_opnd = lambda o:"sym "+sym2str(o) if is_rib(o) else "int "+str(o) #debug#

-- def show_stack(): #debug#
--  s = stack #debug#
--  r = [] #debug#
--  while not s[2]: r.append(s[0]); s=s[1] #debug#
--  print(r) #debug#

-- pos=-1
-- def get_byte():
--  global pos
--  pos+=1
--  return ord(input[pos])

local pos=-1
local function get_byte()
   pos = pos+1
   return input:byte(pos+base)
end

-- VM

--FALSE=[0,0,5]
--TRUE=[0,0,5]
--NIL=[0,0,5]
FALSE={0,0,5}
TRUE={0,0,5}
NIL={0,0,5}

--to_bool=lambda x:TRUE if x else FALSE
--is_rib=lambda x:type(x) is list
local function to_bool(x) if x then return TRUE else return FALSE end end
local function is_rib(x)  return typeof(x) == 'table' end

-- stack=0
local stack=0

--def push(x):
-- global stack
-- stack=[x,stack,0]
push = function(x)
   stack = {x,stack,0}
end

--def pop():
-- global stack
-- x=stack[0]
-- stack=stack[1]
-- return x
local function pop(x)
   local x = stack[0+base]
   stack = stack[1+base]
   return x
end

--prim1=lambda f:lambda:push(f(pop()))
--prim2=lambda f:lambda:push(f(pop(),pop()))
--prim3=lambda f:lambda:push(f(pop(),pop(),pop()))
-- arument evaluation order is undefined in lua, vs. python left-to-right
local function prim1(f) return function() local a=pop(); push(f(a)) end end
local function prim2(f) return function() local a=pop();local b=pop(); push(f(a,b)) end end
local function prim3(f) return function() local a=pop();local b=pop(); local c=pop(); push(f(a,b,c)) end end


--def arg2():x = pop();pop();push(x)
--def close():push([pop()[0],stack,1])
--def f0s(y,x):x[0]=y;return y
--def f1s(y,x):x[1]=y;return y
--def f2s(y,x):x[2]=y;return y

local function arg2() local x = pop(); pop(); push(x) end
local function close() push({pop()[0+base],stack,1}) end
local function f0s(y,x) x[0+base]=y; return y; end
local function f1s(y,x) x[1+base]=y; return y; end
local function f2s(y,x) x[2+base]=y; return y; end

local primitives = {
   -- prim3(lambda z,y,x:[x,y,z]),
   prim3(function(z,y,x) return {x,y,z} end),
   -- prim1(lambda x:x),
   prim1(function(x) return x end),
   pop,
   arg2,
   close,
   -- prim1(lambda x:to_bool(is_rib(x))),
   prim1(function(x) return to_bool(is_rib(x)) end),
   --prim1(lambda x:x[0]),
   --prim1(lambda x:x[1]),
   --prim1(lambda x:x[2]),
   prim1(function(x) return x[0+base] end),
   prim1(function(x) return x[1+base] end),
   prim1(function(x) return x[2+base] end),
   prim2(f0s),
   prim2(f1s),
   prim2(f2s),
   --prim2(lambda y,x:to_bool(x is y if is_rib(x) or is_rib(y) else x==y)),
   prim2(function(y,x) return y == x end), -- FIXME check this
   -- prim2(lambda y,x:to_bool(x<y)),
   prim2(function(y,x) return to_bool(x<y) end),
   -- prim2(lambda y,x:x+y),
   -- prim2(lambda y,x:x-y),
   -- prim2(lambda y,x:x*y),
   -- prim2(lambda y,x:x//y),
   prim2(function(y,x) return x + y end),
   prim2(function(y,x) return x - y end),
   prim2(function(y,x) return x * y end),
   prim2(function(y,x) return x / y end),  -- FIXME: integer division?
   getchar,
   prim1(putchar)
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

--symtbl=NIL
--n=get_int(0)
--while n>0:
-- n-=1
-- symtbl=[[0,[NIL,0,3],2],symtbl,0]


local function run()

local n -- used a couple of times below

local symtbl = NIL
n = get_int(0)
while n>0 do
   n = n - 1
   symtbl = {{0,{NIL,0,3},2},symtbl,0}
end

-- accum = NIL
-- n=0
-- while 1:
--  c=get_byte()
--  if c==44:
--   symtbl=[[0,[accum,n,3],2],symtbl,0]; accum=NIL; n=0
--  else:
--   if c==59: break
--   accum=[c,accum,0]
--   n+=1

local accum = NIL
n=0
while true do
   local c = get_byte()
   if c == 44 then
      symtbl={{0,{accum,n,3},2},symtbl,0}; accum=NIL; n=0
   else
      if c==59 then break end
      accum={c,accum,0}
      n = n + 1
   end
end

--symtbl=[[0,[accum,n,3],2],symtbl,0]
--symbol_ref=lambda n: list_tail(symtbl,n)[0]

local symtbl={{0,{accum,n,3},2},symtbl,0}
local function symbol_ref(n)
   return list_tail(symtbl,n)[0]
end


end
-- decode the RVM instructions

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




-- NEXT



-- while true do
--    local x = get_code()
--    local n = x
--    local d = 0
--    local op = 0
--    while true do
--       d={20,30,0,10,11,4}[op+base]
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
--          n = symbol_ref(n)if op<3
--       end

-- -- NEXT
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

-- while 1:
--  o=pc[1]
--  i=pc[0]
--  if i<1: # jump/call
--   if debug: print(("--- call " if is_rib(pc[2]) else "--- jump ") + show_opnd(o)); show_stack() #debug#
--   o=get_opnd(o)[0]
--   c=o[0]
--   if is_rib(c):
--    c2=[0,o,0]
--    s2=c2
--    nargs=c[0]
--    while nargs:s2=[pop(),s2,0];nargs-=1
--    if is_rib(pc[2]): # call
--     c2[0]=stack
--     c2[2]=pc[2]
--    else: # jump
--     k=get_cont()
--     c2[0]=k[0]
--     c2[2]=k[2]
--    stack=s2
--   else:
--    primitives[c]()
--    if is_rib(pc[2]): # call
--     c=pc
--    else: # jump
--     c=get_cont()
--     stack[1]=c[0]
--   pc=c[2]
--  elif i<2: # set
--   if debug: print("--- set " + show_opnd(o)); show_stack() #debug#
--   x=pop()
--   get_opnd(o)[0]=x
--   pc=pc[2]
--  elif i<3: # get
--   if debug: print("--- get " + show_opnd(o)); show_stack() #debug#
--   push(get_opnd(o)[0])
--   pc=pc[2]
--  elif i<4: # const
--   if debug: print("--- const " + str(o)); show_stack() #debug#
--   push(o)
--   pc=pc[2]
--  elif i<5: # if
--   if debug: print("--- if"); show_stack() #debug#
--   pc=pc[2 if pop()is FALSE else 1]
--  else: # halt
--   break


return {
   primitives = primitives
}
