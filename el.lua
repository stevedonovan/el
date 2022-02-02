#!/usr/bin/lua54
---@diagnostic disable: lowercase-global
-- Flatten ALL those tidy tables and add environment lookup

local saved_globals = {}
local bin2int,debug
local auto_save, print_newline, return_code
local insert,push,pop = table.insert,table.insert,table.remove
local tfind = string.find
--~ local set_call

remove = table.remove
packs,unpacks=string.pack,string.unpack
pack,unpack=table.pack,table.unpack
space,lf,tab,empty_string = ' ','\n','\t',''

function squote(s)
    return '\''..s..'\''
end

local function maybe_num(ns)
    local n = tonumber(ns)
    if n then ns = n end
    return ns
end

function maparg(t,delim)
    if type(t) == 'string' then
        t = split(t,delim or lf)
    end
    return t
end

local tables

function global_lookup(key)
    local v = rawget(_G,key)
    if v ~= nil then
        return v
    end
    for _,m in ipairs(tables) do
        local v = m[key]
        if v ~= nil then
            this_table = m
            _G[key] = v
            return v
        end
    end
    if key:match '^b[01]+$' then
        return bin2int(key:sub(2))
    end
    return os.getenv(key)
end

function set_autosave()
    auto_save = true
end

-- Lua 5.4
if not bit32 then
  bit32 = {}
  function bit32.band(b1,b2) return b1 & b2 end
  function bit32.bor(b1,b2) return b1 | b2 end
  function bit32.bxor(b1,b2) return b1 ~ b2 end
  function bit32.bnot(b) return ~b  end
  function bit32.rshift(b,n) return b >> n end
  function bit32.lshift(b,n) return b << n end
end

function set_global_lookup()
    tables = {math,io,os,string,bit32,table,saved_globals}
    g_saved_globals = saved_globals
    setmetatable(_G,{
        __index = function(t,key)
            local v = global_lookup(key)
            if v then
                return v
            end
            local msg = squote(key) .. " is undefined"
            if debug then assert(msg) else quit(msg) end
        end
    })
end

local not_massaging = os.getenv("EL_NO_PAT_MASSAGE")

local simple_patterns = {WORD = '(%S+)', DIGIT = '(%d+)', REST = '(.+)',
    IDEN = '(%a[%w_]*)', SSTR = "'(.-)'", DSTR = '"(.-)"', LAZY = '(.-)'}

local function massage_pattern(patt)
    if not_massaging then return patt end
    return (patt:gsub('%f[%a](%u+)%f[%A]',simple_patterns))
end

------ some useful functions at your fingertips ----
--- some globals need their return values massaged a bit
gsub = function(s,p,r)
    return (s:gsub(massage_pattern(p),r))
end

match = function(s,p)
    return s:match(massage_pattern(p))
end

local ipat,mmatch_res = 1,{}
local patterns,patterns_kind,local_match

local function reset_current_array()
    if local_match then
       push(mmatch_res, local_match)
       local_match = nil
    end
end

local function mmatch_reset()
    ipat = 1
    reset_current_array()
    local res = mmatch_res
    mmatch_res = {}
    return set_call(res)
end

local function remove_trailing_quantifier(a)
    local trailing = a:match('[^%%]([%?%*%+])$')
    if trailing then
        a = a:sub(1,#a-1)
    end
    return a, trailing
end

function mmatch (s,...)
    local res
    if s == nil then -- finally finished - return what we have collected (if any)
        return mmatch_reset()
    end
    if not patterns then
       local args = {...}
       patterns_kind = {}
       patterns = {}
       for i = 1,#args do
          local patt,question = remove_trailing_quantifier(args[i])
          patterns[i] = massage_pattern(patt)
          patterns_kind[i] = question or 'S'
       end
    end
    local kind = patterns_kind[ipat]
    if kind ~= 'S' then
        inext = (ipat % #patterns)+1
        if s:match(patterns[inext]) then -- our run is over --
            if debug=='T' then print('MO',inext,s) end
            if ipat == 1 then
                res = mmatch_reset()
            else
                ipat = inext
                reset_current_array()
            end
            kind = patterns_kind[ipat]
        end -- and let's start the next...
    end
    local caps = {s:match(patterns[ipat])}
    if #caps > 0 then -- keep collecting
        if kind == 'S' or kind == '?' then -- singles
            if debug=='T' then printx('M'..kind,ipat,caps) end
            extend(mmatch_res, caps)
            ipat = ipat + 1
        else
            if not local_match then
                local_match = caps
            else
                extend(local_match, caps)
            end
            if debug=='T' then printx('M'..kind,ipat,caps) end
        end
    end
    if ipat > #patterns then -- until we run out of patterns
        res = mmatch_reset()
    end
    return res
end

sort = function(t,cmp,dir)
    local tcmp = type(cmp)
    if tcmp == 'string' or tcmp == 'number' then
        local field = cmp
        local orderfn = dir or lt
        cmp = function(x,y) return orderfn(x[field],y[field]) end
    end
    t = maparg(t)
    table.sort(t,cmp)
    return t
end

-- we need this because table.len and string.len collide
function len(v)
    if v == nil then return 0 end
    return #v
end

function write(...)
    io.write(...)
    print_newline = true
end

function empty(t)
    return #t == 0
end

function extend(t1,t2)
   for _,v in ipairs(t2) do
        push(t1,v)
    end
    return t1
end

function append(t,...)
    auto_save = true
    return extend(t,{...})
end

function first(s)
    return s:match('^%S+')
end

function printx(...)
    local not_nil
    local args = {...}
    if #args == 0 or #args==1 and args[1] == '' then
        return ...
    end
    -- only print if there is a non-nil value
    for _,v in ipairs(args) do
        if v ~= nil then
            not_nil = true
            break
        end
    end
    if not_nil then
        local n = #args
        if n == 1 and args[1] == false then
            return_code = 1
        end
        for i = 1,n do
            local a = args[i]
            if type(a) == 'table' then
                a = lua(a)
            else
                a = tostring(a)
            end
            io.write(a)
            if i ~= n then
                io.write(ELSEP)
            end
        end
        print()
    end
    return ...
end

function put(...)
    printx(...)
    return ...
end

function seq(i1,i2,inc)
    local i = i1
    if not inc then inc = 1 end
    if not i2 then
        i2 = i1
        i = 1
    end
    return function()
        if i > i2 then
            return nil
        end
        local res = i
        i = i + inc
        return res
    end

end

null = setmetatable({},{
    __tostring = function(t)
        return "null"
    end
})

function json(t)
    local ty = type(t)
    if ty == 'table' and t ~= null then
        if #t > 0 then
            local res = map(t,json)
            return '['..table.concat(res,',')..']'
        else
            local res = {}
            for k,v in pairs(t) do
                local vs = json(v)
                push(res,('"%s":%s'):format(k,vs))
            end
            return '{'..table.concat(res,',')..'}'
        end
    elseif ty == 'string' then
        local res = ('%q'):format(t):gsub('\\\n','\\n')
        return res
    elseif ty == 'nil' then
        return 'null'
    else
        return tostring(t)
    end
end

function lua(t)
    local ty = type(t)
    if ty == 'table' then
        local res = map(t,lua)
        for k,v in pairs(t) do
            local lv = lua(v)
            local kt = type(k)
            if kt == 'number' then
                if not (k >= 1 and k <= #t) then
                    push(res,('[%d]=%s'):format(k,lv))
                end
            elseif kt == 'string' then
                if not is_iden(k) then
                    k = "['"..k.."']"
                end
                push(res,('%s=%s'):format(k,lv))
            else
                push(res,('[%s]=%s'):format(tostring(k),lv))
            end
        end
        return '{'..table.concat(res,',')..'}'
    elseif ty == 'string' then
        return ('%q'):format(t)
    else
        return tostring(t)
    end
end

local red,green,yellow,blue,magenta,cyan,white = '31','32','33','34','35','36','37'
local colours = {r=red, g=green, y=yellow, b=blue, m=magenta, c=cyan, w=white}
local reset = '\x1b[0m'

local function wrap_colour(prefix,clr,body)
    return '\x1b['..colours[clr]..';1m'..prefix..body..reset
end

local function massage_format(fmt)
    return fmt:gsub('(%%)(%a)([%d.]*%a)',wrap_colour)
end

paint = setmetatable({},{
    __index = function(t,clr)
        return function(body)
            return wrap_colour('',clr,body)
        end
    end
})

function foldgen (op)
    return function(...)
        local args = {...}
        if #args == 1 then return args[1] end
        local res = op(args[1],args[2])
        for i = 3,#args do
            res = op(res,args[i])
        end
        return res
    end
end

-- n-ary cat cannot be generally made from classic folding
add = foldgen(function(acc,v) return acc + v end)
mul = foldgen(function(acc,v) return acc * v end)
cat = foldgen(function(acc,v) return acc .. v end)

function gt(a,b) return a > b end
function lt(a,b) return a < b end

local call_meta

local function is_list(t)
    return getmetatable(t) == call_meta
end

call_meta = {
    __call = function(t,k)
        return t[k]
    end,
    __concat = function(t1,t2)
        local res
        if is_list(t1) then
            res = slice(t1,1)
        else
            res = {t1}
        end
        for i = 1,#t2 do push(res,t2[i]) end
        return set_call(res)
    end
}

function prepend(v,t)
    local res = slice(t,1)
    table.insert(res,1,v)
    return res
end

function set_call(t)
    return setmetatable(t,call_meta)
end

function list(...)
    return set_call({...})
end

function stop(val)
    if val then
        printx(val)
        os.exit(0)
    end
end

function iter(t)
    local i,n = 0,#t
    return function()
        if i > n then
            return nil
        end
        i = i + 1
        return t[i]
    end
end

function items(...)
    return iter{...}
end

local _iterators = {pairs=true,iter=true,items=true,seq=true}

function expand_home(path)
    if path:match('^~') then
        path = path:gsub('^~',os.getenv('HOME'))
    end
    return path
end

function readf(f)
    local res = f:read('*a')
    f:close()
    return res
end

function openf(sa)
    local f
    if sa == '-' or sa == 'stdin' then
        f = io.stdin
    else
        f = assert(io.open(expand_home(sa)))
    end
    return f
end

function readfile(sa)
    return readf(openf(sa))
end

local last_n

function line(n)
    local f,i = io.stdin,last_n or 0
    local line
    for l in f:lines() do
        line = l
        i = i + 1
        if i == n then
            last_n = i
            row = i
            return line
        end
    end
    if n == -1 then
        row = i
        return line
    end
    return nil
end

local function _sh(fmt,...)
    local cmd = fmt
    if ... ~= nil then
        cmd = fmt:format(...)
    end
    return assert(io.popen(cmd,'r'))
end

function sh(fmt,...)
    local res = readf(_sh(fmt,...)):gsub('%s+$','')
    return res
end

function shl(fmt,...)
    local ff = _sh(fmt,...)
    return ff:lines()
end

function writefile(path,contents,ext)
    if ext then path = path .. ext end
    local f = assert(io.open(expand_home(path),'w'))
    if type(contents) ~= 'table' then
        f:write(contents)
    else
        for i = 1,#contents do f:write(contents[i],'\n') end
    end
    f:close()
end

function join(dir,file)
    return dir .. '/' .. file
end

function exists(dir,file)
    if file then
        dir = join(dir,file)
    end
    return io.open(dir) and dir
end

function printf(fmt,...)
    write(fmt:format(...))
end

function split2(arg,sep)
    local idx = index(arg,sep)
    if idx then
        local a = slice(arg,1,idx-1)
        local b = slice(arg,idx+1)
        return a,b,arg[idx]
    else
        return arg,{}
    end
end

function index(t,val)
    local pred
    if type(val) == 'function' then
        pred = val
    else
        pred = function(a) return a == val end
    end
    for i,v in ipairs(t) do
        if pred(v) then
            return i
        end
    end
end

function wordpat(word) return '%f[%a_]'..word..'%f[^%a_]' end

function match_word(s, word)
    return s:match(wordpat(word))
end

function contains(aa, word)
    if index(aa,word) then
        return true
    end
    -- match just the word with the frontier pattern
    word = wordpat(word)
    for _,a in ipairs(aa) do
        if a:match(word) then
            return true
        end
    end
    return false
end

function eq(a,b) return a == b end
function neq(a,b) return a ~= b end

function map(t,fun,a)
    t = maparg(t)
    local pp = a == pairs
    local res = {}
    for i = 1,#t do
        if pp then a = t[i+1] end
        local val = fun(t[i],a)
        if val ~= nil then push(res,val) end
    end
    return set_call(res)
end

function filter(t,pred,a)
    return map(t,function(x,ex) if pred(x,ex) then return x end end,a)
end

function mapm(t,fun)
    local res = {}
    for k,v in pairs(t) do
        local nv,nk = fun(v,k)
        res[nk or k] = nv
    end
    return set_call(res)
end

function mapma(t,fun)
    local res = {}
    if not fun then
        fun = function(k,v)
            if type(v) ~= 'table' then
                v = set_call{k,v}
            else
                v._key = k
            end
            return v
        end
    end
    for k,v in pairs(t) do
        append(res,fun(k,v))
    end
    return set_call(res)
end

function mapam(t,fun,collect)
    t = maparg(t)
    local res = {}
    if not fun then
        fun = function(v,k)
            return v[1],v[2]
        end
    end
    for i = 1,#t do
        local k,v = fun(t[i])
        if k ~= nil then
            if v == nil then v = t[i] end
            if collect then
                if res[k] == nil then res[k] = {} end
                insert(res[k],v)
            else
                res[k] = v
            end
        end
    end
    return set_call(res)
end

function each(t,fun,arg)
    for i = 1,#t do fun(t[i],arg) end
end

function forall(t,fun)
    for k,v in pairs(t) do fun(k,v) end
end

function splitp(s,delim)
    local idx = tfind(s,delim or ' ',1,true)
    if not idx then
        return nil
    else
        return s:sub(1,idx-1):gsub('%s*$',''), (s:sub(idx+1):gsub('^%s*',''))
    end
end

function split(s,re,buff)
    local find,sub = string.find, string.sub
    local i1,ls = 1,buff or {}
    if not re then re = '%s+' end
    if re == '' then return {s} end
    while true do
        local i2,i3 = find(s,re,i1)
        if not i2 then
            local last = sub(s,i1)
            if last ~= '' then push(ls,last) end
            if #ls == 1 and ls[1] == '' then
                return {}
            else
                return set_call(ls)
            end
        end
        push(ls,sub(s,i1,i2-1))
        i1 = i3+1
    end
end

function spliti(s,re)
    return iter(split(s,re))
end

function run(file)
    local ext = file:match('%.%a+$')
    if ext == '.el' then
        evaluate(split(readfile(file)))
    else
        print(dofile(file))
    end
end

vars = set_call

function glob(t)
    for k,v in pairs(t) do
        _G[k] = v
    end
    return t
end

let = glob

local field_names, patt

function fields(parms,glob)
    if type(parms) == 'string' then
        parms = { parms }
    end
    if not field_names and parms.cols then
        field_names = split(parms.cols,',')
        _G.COLS = field_names
    end
    if not patt and parms.pat then
        patt = massage_pattern(parms.pat)
    end
    parms.cols = nil
    parms.pat = nil
    local delim = eat(parms,'delim')
    local line = eat(parms,1)
    -- and we will REUSE the table
    local result = parms
    local splitted
    if patt then
        splitted = {line:match(patt)}
    elseif type(line) ~= 'table' then
        splitted = split(line,delim)
    else -- it's already an array
        splitted = line
    end
    if not field_names then -- create indices from 1 to N
        field_names = seqa(#splitted)
        if glob then
            for i = 1,#splitted do
                field_names[i] = 'f' .. field_names[i]
            end
        end
    end
    if glob then result = _G end
    for i = 1,#splitted do
        result[field_names[i]] = maybe_num(splitted[i])
    end
    if #splitted == 0 then
        return nil
    end
    return set_call(result)
end

function gfields(parms)
    return fields(parms,true)
end

function quit(msg)
    io.stderr:write(msg,'\n')
    os.exit(1)
end

function slice(t,i1,i2)
    local res = {}
    if not i2 then
        i2 = #t
    end
    for i = i1,i2 do
        push(res,t[i])
    end
    return set_call(res)
end

function acc(name,val,op)
    op = op or add
    local acc = rawget(_G,name)
    if acc == nil then
        if op==add then acc = 0 elseif op==mul then acc = 1 else acc = {} end
    end
    _G[name] = op(acc,val)
    -- return val
end

function index_by(t,arr,key_index)
    local res = {}
    for i = 1,#arr do
        local idx = arr[i]
        local val = t[idx]
        val = val or null
        if key_index then
            res[idx] = val
        else
            push(res,val)
        end
    end
    return set_call(res)
end

_RES_ = {}

_G.push = function (v)
    push(_RES_,v)
end

function collect(...)
    local res = {}
    for v in ... do
        push(res,v)
    end
    return set_call(res)
end

local _seq = 0

function nseq()
    _seq = _seq + 1
    return _seq
end

function seqa(...)
    return collect(seq(...))
end

function zip(t1,t2,op)
    if not op then
        op = function(v1,v2) return {v1,v2} end
    end
    local res, n  = {}, math.min(#t1,#t2)
    for i = 1,n do
        res[i] = op(t1[i],t2[i])
    end
    return set_call(res)
end

function read_num()
    return io.read('*n')
end

function fun(f)
    return function(...)
        return f(...)
    end
end

local function olines(n)
    for _ = 1,n do io.write('\n') end
end

function tablefy(t)
    local cols,painter,left,top,bottom
    if t.cols or t.paint or t.left or t.top or t.bottom then
        cols = t.cols
        painter = t.paint
        left = t.left
        top = t.top
        bottom = t.bottom
        local margin = t.margin
        if margin then
            left = margin; top = margin; bottom = margin
        end
        t = t[1]
    end
    local widths,ncols,nrows,rows = {},#t[1],#t,{}
    local obj = ncols==0 and next(t[1])
    if obj and not cols then
        local gcols = global_lookup("COLS")
        if gcols then
            cols = gcols
        else
            cols = mapma(t[1],function(k) return k end)
        end
    end
    if cols then
        cols = maparg(cols,',')
        insert(t,1,cols)
        nrows = nrows + 1
        ncols = #cols
    end
    for i = 1,nrows do
        local srow,row = {},t[i]
        if obj and i > 1 then
            row = index_by(row,cols)
        end
        if type(row) ~= 'table' then
            for j = 1,ncols do srow[j] = '' end
        else
            for j = 1,ncols do
                srow[j] = tostring(row[j])
            end
        end
        rows[i] = srow
    end
    for j = 1,ncols do -- for each column
        local w = 0
        for i = 1,nrows do -- max over each row
            w = math.max(w,#rows[i][j])
        end
        insert(widths,w+2) -- for luck
    end
    local pad = ''
    if left then pad = (' '):rep(left) end
    if top then olines(top) end
    for i = 1,nrows do
        for j = 1,ncols do
            local txt = rows[i][j]
            local n = #txt
            if i == 1 and painter then txt = painter(txt) end
            io.write(pad,txt,(' '):rep(widths[j]-n))
        end
        io.write('\n')
    end
    if bottom then olines(bottom) end
end

function bin(n)
    local t = {}
    for i = 1, 32 do
        table.insert(t, bit32.band(n, 1))
        n = bit32.rshift(n, 1)
        if n == 0 then
            break
        end
    end
    return table.concat(t)
end

function hex(n)
    return ('%X'):format(n)
end

local _conversions = {bin = true, hex = true, stop=true, put=true, json = true}

function bin2int(s)
    local res = 0
    for i = 1,#s do
        local ch = s:byte(i)
        if ch == 49 then
            res = res + bit32.lshift(1,i-1)
        end
    end
    return res
end

function inc(v)
    return (v or 0) + 1
end

local function dotted_ref(t,key)
    if type(key) == 'string' and key:match('%.') then
        local parts = split(key,'%.')
        key = maybe_num(parts[1])
        t = t[key]
        for i = 2,#parts do
            if type(t) == 'table' then
                key = maybe_num(parts[i])
                break
            end
            if t == nil then return nil end
            t = t[parts[i]]
        end
    else
        key = maybe_num(key)
    end
    return t,key
end

function get(t,key)
    if type(key) == "table" then
        return index_by(t,key)
    end
    t,key = dotted_ref(t,key)
    return t[key]
end

function update(t,...)
    local orig, args = t, {...}
    for i = 1,#args,2 do
        local key,v = args[i],args[i+1]
        t,key = dotted_ref(orig,key)
        if type(v) == 'function' then
            v = v(t[key])
        end
        if v == nil and type(key)=="number" then
           remove(t,key)
        else
            t[key] = v
        end
    end
    return orig
end

-- very unscientific
function make_abs(f)
    if not f:match('^/') then
        f = PWD..'/'..f
    end
    return f
end

function base_name(f)
    return f:match('([^.]+)%.%ta+$')
end

function take(t,key,ref)
    ref.val = t[key]
    t[key] = nil
    return ref.val
end

function eat(t,key)
    local val = t[key]
    t[key] = nil
    return val
end

-- this is not a happy function
local function is_function(v)
    return type(v) == 'string' and v:match('^function%(')
end

local literals = {}

function save(tbl)
    local ref = {}
    if tbl == nil then
        tbl = {}
    elseif take(tbl,'add_file',ref) then
        local file = ref.val
        local path = make_abs(file)
        dofile(path)
        literals[file] = "dofile('"..path.."')"
    elseif take(tbl,'remove_file',ref) then
        literals[ref.val] = nil
    elseif take(tbl,'add_mod',ref) then
        local mod = ref.val
        assert(require(mod))
        literals[mod] = "require('"..mod.."')"
    elseif take(tbl,'remove_mod',ref)   then
        literals[ref.val] = nil
    end
    local out = {}
    push(out,'local saved = {_G=_G};_ENV=saved')
    for k,v in pairs(tbl) do
        if v == null then -- i.e clear the var...
            v = nil
        end
        if literals[k] and v == nil then
            literals[k] = nil
        elseif is_function(v) or dotted_lookup(v) then
            literals[k] = v
        else
            update(saved_globals,k,v)
        end
    end
    saved_globals._LITERAL_ = literals
    for k,v in pairs(saved_globals) do
        local vs = tostring(v)
        if type(k) == 'string' then
            local vt = type(v)
            if vt == 'string' then
                vs = ('%q'):format(vs)
            elseif vt == 'table' then
                vs = lua(v)
            elseif vt == 'function' then
                vs = nil
            end
            if vs ~= nil then
                push(out,('%s=%s'):format(k,vs))
            end
        else
            push(out,vs)
        end
    end
    if next(literals) then
        push(out,'_ENV=saved._G')
        for k,v in pairs(literals) do
            if v:match('^require') then -- force these chaps to the front!
                push(out,('%s=%s'):format(k,v))
            end
        end
        for k,v in pairs(literals) do
            if v:match('^dofile') then
                push(out,v)
            elseif not v:match('^require') then
                push(out,('%s=%s'):format(k,v))
            end
        end
    end
    push(out,'return saved\n')
    writefile('~/.el',table.concat(out,'\n'))
end

set = save

function loads()
    local path = expand_home('~/.el')
    local ok, t = pcall(dofile,path)
    if ok then
        saved_globals = t
        literals = t._LITERAL_ or {}
        saved_globals._G = nil
        for k,v in pairs(saved_globals) do
            if type(v) == 'table' then
                set_call(v)
            end
        end
    elseif not t:match('No such file or directory') then -- (ewww)
        print(t)
    end
end

--- mangling el notation into regular Lua -----

-- shell does NOT like print() so we have print[]
function bquote(a)
    local res = a:gsub('%b[]',function(aa)
        aa = aa:sub(2,#aa-1)
        return '('..bquote(aa)..')'
    end)
    return res
end

function split_key_val(a)
    local var,expr = a:match '^([%w_%-%.]+[%+%*]*)=([^=].*)'
    if var then
        a = expr
    end
    return a,var
end

local stringer,filer = '^','@'
if os.getenv('EL_AT') then
    stringer = '@'
    filer = '@@'
end
local string_arg = '^%'..stringer..'(.*)$'
local string_inter = '([^%w)%]])%'..stringer..'([^)%],]+)'
local file_arg = '^'..filer..'(.+)'

function quote(a)
    -- an argment may be @file
    local sa = a:match(file_arg)
    if sa then
        return ('readfile(%q)'):format(sa)
    end
    -- help with quoting strings

    sa = a:match(string_arg)
    if sa then
        return squote(sa)
    end
    if a:match(stringer) then
        -- there may be embedded ^strings
        sa = a:gsub(string_inter,function(pre,str)
            return pre .. squote(str)
        end)
    end
    if sa then
        if sa:match('^%s') then
            sa = sa:sub(2)
        end
        a = sa
    end
    return bquote(a)
end

function dump(msg,t)
    print(msg,lua(t))
end

function collect_subexprs(args)
    local subexprs = {}
    local idx = 1
    local nsub = 0
    local subx = {list={},prefix=''}
    push(subexprs,subx)
    while true do
        local arg = args[idx]
        if not arg then break end
        idx = idx + 1

        local set,block,opens,inner
        -- only an assignment or nothing can go before {
        opens,block = arg:match '^({+)(.*)'
        if not block then
            set,opens,block = arg:match '^([%w_-]+=)({+)(.*)'
        end
        if block then
            inner = block:match ('^[^=]+=.+')
        end
        if inner then
            table.insert(args,idx,inner)
            block = ''
        end
        if block then
            for _ = 1,#opens do
                push(subexprs,subx)
                subx = {list={},prefix=set}
                set = nil
            end
            if block:match('}$') then
                -- so foo}} becomes foo, with }} needing to be further processed
                block,arg = block:match('([^}]*)(}+)')
            end
            if #block > 0 then -- first element of this sublist
                push(subx.list,block)
            end
        end
        local endblock,closes = arg:match '^([^}]*)(}+)$'
        -- hack: if we DID match a { then it would have been stripped at this point..
        -- this allows things like {word} to appear in strings. But it's a hack...
        if arg:match '{' then endblock = nil end
        if endblock then
            if #endblock > 0 then
                push(subx.list,endblock)
            end
            for _ = 1,#closes do
                local xsub = chain_expr(subx.list,'subexpr')
                -- and add to the parent list
                local prefix = subx.prefix or ''
                subx = pop(subexprs)
                push(subx.list,prefix..xsub)
            end
        end
        if not block and not endblock then
            push(subx.list,arg)
        end
    end
    local res = pop(subexprs)
    if res == nil then quit("too many closing braces") end
    arg = res.list
    return arg, nsub
end

function is_iden(name)
    return name:match('^[%a_][%w_]*$')
end

function is_op(name)
    return name and (name:match('^[%+%*%-%%<>/|~&=]+$') or name=='and' or name=='or')
end

function dotted_lookup(expr)
    if type(expr) ~= 'string' then
        return nil
    end
    if expr:match('%.') then
        local parts = split(expr,'%.')
        local v = global_lookup(parts[1])
        for i = 2,#parts do
            if v == nil then return false end
            v = v[parts[i]]
        end
        return v
    else
        return global_lookup(expr)
    end
end

function _iter_(f,...)
    local ts = type(f)
    if ts == 'table' then
        return iter(f,...)
    elseif ts == 'number' then
        return seq(f,...)
    else
        return f,...
    end
end

function is_global_fun(expr)
    return type(dotted_lookup(expr)) == 'function'
end

function is_conv_fun(expr)
    if expr == 'not' then
        return true
    end
    return is_global_fun(expr) and _conversions[expr]
end

local iden = '^(%a[%w_]*)'
local string_fun = '^(.+)'..stringer..'$'
local string_name = iden..stringer..'$'

function is_iterator_fun(expr)
    local name = expr:match(string_fun)
    if name then expr = name end
    return is_global_fun(expr) and _iterators[expr]
end

-- the IDENTITY function
function eval(...) return ... end

local function quote_key(s)
    if not is_iden(s) then
        s = "['"..s.."']"
    end
    return s
end

function subexpr(arg,iter,lambda_args,chain)
    --  f a b ... becomes f(a,b,...)
    local conv
    if #arg == 0 then
        return '{}'
    end
    -- strip off the ^ prefix indicating 'quote following args'
    if arg[1] == stringer and iter == 'subexpr' then
        arg[1] = 'list'..stringer -- for people in a hurry
    end
    local quoted_fun = arg[1]:match(string_name)
    if quoted_fun then
        arg[1] = quoted_fun
    end
    local args,vars,implicit = {},{},nil
    -- collect any {..} and apply quote() to them
    --arg = collect_subexprs(arg)
    local expr = quote(arg[1])

    -- top-level - allow for output conversions like bin or hex
    if iter == 'expr' and is_conv_fun(expr) then
        conv = expr
        arg = slice(arg,2)
        expr = quote(arg[1])
    end

    if iter == 'iter' then
        -- implicit 'seq'
        if tonumber(expr) ~= nil then
            implicit = 'seq'
        elseif type(expr) == 'table' then
            implicit = 'pairs'
        end
    elseif iter == 'expr' then
        if expr:match '^\'' and expr:match '%%' then
            -- implicit 'format'
            implicit = 'format'
        elseif not is_global_fun(expr) and not is_op(arg[2]) then
            -- allow multiple values on the top-level using identity function
            implicit = 'eval'
        end
    elseif iter == 'subexpr' then
        local global_fun = is_global_fun(expr)
        if not lambda_args then
            if not global_fun then
                if is_op(arg[2]) then
                    implicit = ''
                    conv = ''
                else
                    implicit = 'list'
                end
            elseif expr == 'eval' then
                expr = ''
                conv = ''
            end
        elseif not global_fun then
            implicit = ''
        end
    end
    if implicit then
        push(arg,1,expr)
        expr = implicit
    end

    local has_vars
    for i = 2,#arg do
        local val,var = split_key_val(arg[i])
        val = quote(val)
        local im1 = i - 1
        if var then
            has_vars = true
            vars[im1] = var
        else
            vars[im1] = ''
        end
        if quoted_fun and tostring(val) ~= 'null' then
            val = squote(val)
        end
        args[im1] = val
    end
    local dcl = false
    if implicit == 'eval' and has_vars and chain then
        dcl = true
    end
    if is_global_fun(expr) then
        local call
        if has_vars then
            -- key-value pairs as args means collect as single TABLE
            for i = 1,#args do
               if vars[i] ~= '' then
                   local a,v = args[i],vars[i]
                   -- functions get string-quoted (to be saved) but do check them!
                    if is_function(a) then
                        local _,e = load('return '..a)
                        if e then
                            quit('cannot compile '..e)
                        end
                        a = ('%q'):format(a)
                    end
                    if v:match('[%+%*]$') then
                        v,op = v:sub(1,#v-1),v:sub(#v)
                        args[i] = v..'='..v..op..a 
                    else
                        args[i] = quote_key(v)..'='..a
                    end
               end
            end
        end
        -- this ad-hockery is for COOL COLOUR FORMATS
        if expr == 'format' or expr == 'printf' then
            args[1] = massage_format(args[1])
        end
        if dcl then
            expr = table.concat(args,';')
        else
            call = table.concat(args,',')
            if not has_vars then
                expr = expr..'('..call..')'
            else
                -- this is to by-pass the list() function when we have a table containing k-v pairs
                if expr == 'list' then expr = '' end
                expr = expr..'{'..call..'}'
            end
        end
    else
        expr = expr .. ' ' .. table.concat(args,' ')
    end

    if conv then
        if conv == 'not' then
            expr = conv..' '..expr
        else
            expr = conv..'('..expr..')'
        end
    end
    return expr
end

-- there can be a chain of expressions 'ex1 : ex2 : ex2'.
-- The first is special (sexpra) because it will determine implicit looping
-- The last is special because it does the final print and may have an if-clause
function chain_expr(expra,kind,no_print)
    local lambda_args
        -- {} subexpressions may be {x: f[x]} lambdas or {f _} implicit lambdas
    if kind == 'subexpr' and #expra > 0 then
        lambda_args = expra[1]:match('^([%w_,]*):$')
        if lambda_args then
            expra = slice(expra,2)
        elseif contains(expra,'_') then
            lambda_args = '_'
        end
    end
    expra = collect_subexprs(expra)
    local cexpra, rest = split2(expra,':')
    local first_expr = cexpra
    local subx,conditional,escape = '',kind ~= 'iter','goto fin'
    if kind == 'subexpr' then
        escape = 'return nil'
    end
    if #rest > 0 then
        local k,init = 1,false
        subx = ''
        while #rest > 0 do
            -- generally a chain of expressions, but 'pass' allows a 'statement'
            -- also if we first encounter a var=val
            local pass = (cexpra[1] == 'pass') and '' or 'it,this='
            if cexpra[1]:match('^[^=]+=%S') then pass = ''
            elseif pass=='' then remove(cexpra,1)
            elseif not init then subx = 'local '; init=true end
            local cexpr = subexpr(cexpra,'expr',false,true)
            subx = subx .. pass..cexpr..'\n'
            if conditional and pass ~= '' then
                subx = subx..'if not it then ' .. escape .. ' end\n'
            end
            if debug == 'T' then  subx = ("%s; printx('%02d',it); "):format(subx,k); k = k + 1 end
            cexpra,rest = split2(rest,':')
        end
        expra = cexpra
    end
    local ifcond
    local cexpra,fexpra = split2(expra,'if')
    if #fexpra > 0 then
        if kind == 'iter' then quit('cannot have if clause with iterator expression') end
        expra = cexpra
        local fexpr = subexpr(fexpra,'expr')
        ifcond = 'if '..fexpr..' then '
    end
    local subwrap = kind == 'subexpr' and (ifcond or subx ~= '')
    -- the final expression
    local fkind = kind
    if lambda_args or #subx > 0 then fkind = 'expr' end
    -- final expression need not print...
    if expra[1] == 'pass' then remove(expra,1); no_print=true end
    local iexpr = subexpr(expra,kind,lambda_args) -- also needs to know that we are a FINAL expression
    if kind == 'expr' and not no_print then -- final expression prints things out
        iexpr = 'printx('..iexpr..')'
    elseif subwrap or lambda_args then
        iexpr = 'return '..iexpr
    end
    if ifcond then
        iexpr = ifcond .. iexpr .. ' end'
    end
    local endf = ''
    if #subx > 0 and conditional and kind ~= 'subexpr' then
        endf = ' ::fin::\n'
    end
    if kind ~= 'iter' then
        subx = subx .. iexpr .. endf
    end
    if lambda_args then
        subx = 'function('..lambda_args..') '..subx..' end '
    elseif subwrap then
        subx = '(function() '..subx..' end)()'
    end
    return subx,iexpr,first_expr
end

function evaluate(arg)
    loads()
    set_global_lookup()
    debug = os.getenv('ELDBG')
    ELSEP = '\t'
    row = 1

    local endxa = {}
    local itera,expra = split2(arg,'do')
    if #expra == 0 then
        if is_iterator_fun(itera[1]) then
            -- loop with implicit expr (e.g. 'el seq 10')
            expra = {'it'}
        else
            -- no iterator, just expression...
            expra = itera
            itera = nil
        end
    end
    expra,endxa = split2(expra,'end')

    local expr,_,first_expr = chain_expr(expra,'expr', #endxa > 0)
    local implicit_var = 'it'
    if not itera and contains(first_expr,'L') then
        itera = {'lines'} -- implicit loop over all lines in stdin
        implicit_var = 'L'
    end
    if itera then -- 'do'
        local first,iter = chain_expr(itera,'iter')
        local endx = ''
        if #endxa > 0 then endx = 'local it=_RES_; ' .. chain_expr(endxa,'expr') end
        if debug == 'T' then expr = ("printx('00',%s); %s"):format(implicit_var,expr) end
        expr = ('%s for %s,this in _iter_(%s) do %s; row=row+1 end %s')
            :format(first,implicit_var,iter,expr,endx)
    end

    if debug then
        print(expr)
    end

    local f,e = load(expr,'expr')
    if e then
        print('error',e)
        print(expr)
        os.exit(1)
    end
    f()
    if auto_save then
        save()
    end
    if print_newline then
        print()
    end
    if return_code then
        os.exit(return_code)
    end
end

if #arg == 0 then
    quit('el <expression>. Like sin[2*pi] or date ^%c')
end

evaluate(arg)
