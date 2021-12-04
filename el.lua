#!/usr/bin/lua54
-- Flatten ALL those tidy tables and add environment lookup

local saved_globals = {}
local bin2int,this_table,debug
local auto_save, print_newline, return_code
local push,pop = table.insert,table.remove

local function squote(s)
    return '\''..s..'\''
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
            quit(squote(key) .. " is undefined")
        end
    })
end

local not_massaging = os.getenv("EL_NO_PAT_MASSAGE")

local simple_patterns = {WORD = '(%S+)', DIGIT = '(%d+)', REST = '(.+)'}

local function massage_pattern(patt)
    if not_massaging then return patt end
    return patt:gsub('%f[%a](%u+)%f[%A]',simple_patterns)
end

------ some useful functions at your fingertips ----
--- some globals need their return values massaged a bit
gsub = function(s,p,r)
    local res = s:gsub(massage_pattern(p),r)
    return res
end

match = function(s,p)
    return s:match(massage_pattern(p))
end

local ipat,mmatch_res,patterns = 1,{}

local function mmatch_reset()
    ipat = 1
    local res = mmatch_res
    mmatch_res = {}
    return res
end

function multimatch (s,...)
    if s == nil then -- finally finished - return what we have collected (if any)
        return mmatch_reset()
    end
    if not patterns then
       local args = {...}
       patterns = {}
       for i = 1,#args do
          patterns[i] = massage_pattern(args[i])
       end
    end
    local caps = {s:match(patterns[ipat])}
    if #caps > 0 then -- keep collecting
        extend(mmatch_res, caps)
        ipat = ipat + 1
    end
    if ipat > #patterns then -- until we run out of patterns
        return mmatch_reset()
    end
end

insert = function(t,p,e)
    if not e then
        table.insert(t,p)
    else
        table.insert(t,p,e)
    end
    auto_save = true
end

sort = function(t,cmp)
    table.sort(t,cmp)
    return t
end

-- we need this because table.len and string.len collide
len = function(v)
    return #v
end

write = function(...)
    io.write(...)
    print_newline = true
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
        return
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
        return ('%q'):format(t)
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


function foldgen (zero,op)
    return function(...)
        local args, res = {...}, zero
        for i = 1,#args do
            res = op(res,args[i])
        end
        return res
    end
end

local function foldop (name, zero, op)
    _G[name] = foldgen(zero,op)
end

foldop('add',0,function(acc,v) return acc + v end)
foldop('mul',1,function(acc,v) return acc * v end)
foldop('cat','',function(acc,v) return acc .. v end)

local set_call

local call_meta = {
    __call = function(t,k)
        return t[k]
    end,
    __concat = function(t1,t2)
        local res = slice(t1,1)
        for i = 1,#t2 do push(res,t2[i]) end
        return set_call(res)
    end    
}

function set_call(t)
    return setmetatable(t,call_meta)
end

local function is_list(t)
    return getmetatable(t) == call_meta
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

function line_n(sa,n)
    local f,i = openf(sa),0
    for line in f:lines() do
        --print(line,n,i)
        i = i + 1
        if i == n then
            --f:close()
            return line
        end
    end
    f:close()
    return nil
end

function line(n)
    return line_n('stdin',n or 1)
end

local function has_space(s)
    return s:match('%s') or s:match('"')
end

local function massage(s)
    if s == true or s == 'true' then
        s = ''
    elseif type(s) == 'string' and has_space(s) then
        s = squote(s)
    end
    return s
end

function parameters(arg,...)
    local args,first
    if type(arg) == 'table' then
        first = arg[1]
        args = {}
        for k,v in pairs(arg) do
            if k ~= 1 then
                if type(k) == 'number' then
                    k = k - 1
                end
                args[k] = v
            end
        end
    else
        first = arg
        args = {...}
    end
    return first, args
end

function exec(name,...)
    local name,args = parameters(name,...)
    local cmd = {name}
    for k,v in pairs(args) do
        if not (type(k) == 'number' and k >= 1 and k <= #args) then
            local flagc = '--'
            if #k == 1 then
                flagc = '-'
            end
            local vv = {v}
            if type(v) == 'table' then
                vv = v
            end
            for _,v in ipairs(vv) do
                push(cmd,flagc..k..' '..massage(v))
            end
        end
    end     
    for _,a in ipairs(args) do
        if has_space(a) then
            a = squote(a)
        end
        push(cmd,a)
    end
    
    cmd = table.concat(cmd,' ')
    if debug then
        print('cmd',cmd)
    end
    return shell(cmd)
end

function shell(cmd)
    local res = readf(assert(io.popen(cmd,'r'))):gsub('%s+$','')
    return res
end

function writefile(path,contents)
    local f = assert(io.open(expand_home(path),'w'))
    f:write(contents)
    f:close()
end

function printf(fmt,...)
    write(format(fmt,...))
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

function contains(aa, word)
    if index(aa,word) then
        return true
    end
    -- match just the word with the frontier pattern
    word = '%f[%a]'..word..'%f[%A]'
    for _,a in ipairs(aa) do
        if a:match(word) then
            return true
        end
    end
    return false    
end

function map(t,fun)
    local res = {}
    for i = 1,#t do
        push(res,fun(t[i]))
    end
    return set_call(res)
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

vars = set_call

function glob(t)
    for k,v in pairs(t) do
        _G[k] = v
    end
    return t
end

local field_names

function fields(parms)
    if not field_names then
        field_names = split(parms.cols,',')
        _G.COLS = field_names
    end
    parms.cols = nil
    local delim = eat(parms,'delim')
    local pat = eat(parms,'pat')
    local line = eat(parms,1)
    -- and we will REUSE the table
    local result = parms
    local splitted
    if pat then
        splitted = {line:match(pat)}
    elseif delim then
        splitted = split(line,delim)
    else
        splitted = line
    end 
    for i = 1,#splitted do
        local v = splitted[i]
        local maybe_num = tonumber(v)
        if maybe_num then
            v = maybe_num
        end
        result[field_names[i]] = v
    end
    if #splitted == 0 then
        return nil
    end
    return result
end

local function fixup_url(p)
    if type(p) == 'string' then
        p = {p}
    end
    local url = p[1]
    if not url:match('^%a+://') and URL then
        p[1] = URL..url
    end
    p.silent = true
    table.insert(p,1,'curl')
    p.header = {}
    return p    
end

local function finis(p)
    local headers = eat(p,'headers')
    if headers then
        for k,v in pairs(headers) do
            push(p.header,k..': '..v)
        end
    end
    return exec(p)
end

function post(p)
    p=fixup_url(p)  
    local ty = type(p.data)
    if ty == 'table' then
        p.data = json(p.data)
        if not p.headers then p.headers = {} end
        p.headers['content-type'] = 'application/json'
    end
    return finis(p)
end

function get(p)
    p=fixup_url(p)  
    local query = eat(p,'query')
    if query then
        p.G = true      
        assert(type(query)=='table')
        local data = {}
        for k,v in pairs(query) do
            append(data,k..'='..v)
        end
        p['data-urlencode'] = data
    end
    return finis(p)
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

local first = true
local push = table.insert

function fold(val,accname,start,op)
    local acc = g_saved_globals[accname]
    if first then
        first = false
        acc = start
        set_autosave()
    end
    g_saved_globals[accname] = op(acc,val)
end

function sum(val,accname)
    return fold(val,accname,0,add)
end

function index_by(t,arr,keys)
    local res = {}
    for i = 1,#arr do
        local idx = arr[i]
        local val = t[idx]
        val = val or null
        if keys then
            res[idx] = val
        else
            push(res,val)
        end        
    end
    return set_call(res)
end

function collect(...)
    local res = {}
    for v in ... do
        push(res,v)
    end
    return set_call(res)
end

function seqa(...)
    return collect(seq(...))
end

function zipmap(t1,t2,op)
    if not op then
        op = function(v1,v2) return {v1,v2} end
    end
    local res = {}
    for i = 1,#t1 do
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

-- very unscientific
function make_abs(f)
    return PWD..'/'..f
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
        literals[file] = "dofile('"..path.."')"
    elseif take(tbl,'remove_file',ref) then
        literals[ref.val] = nil
    elseif take(tbl,'add_mod',ref) then
        local mod = ref.val
        literals[mod] = "require('"..mod.."')"
    elseif take(tbl,'remove_mod',ref) then
        literals[ref.val] = nil
    end
    local out = {}
    push(out,'local saved = {_G=_G};_ENV=saved')
    for k,v in pairs(tbl) do
        if is_function(v) or dotted_lookup(v) then
            literals[k] = v
        else
            if v == '' then -- i.e clear the var...
                v = nil
            end
            saved_globals[k] = v
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

local SUBMARKER='\001'

local function has_marker(s)
    return s:sub(1,1)==SUBMARKER
end

local function sub_marker(s)
    local res = false
    if has_marker(s) then
        res = true
        s = s:sub(2)
    end
    return s,res
end

-- shell does NOT like print() so we have print[]
function bquote(a)
    local res = a:gsub('%b[]',function(aa)
        aa = aa:sub(2,#aa-1)
        return '('..bquote(aa)..')'
    end)
    return res
end

function split_key_val(a)
    local var,expr = a:match '^([^=]+)=([^=].*)'
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
    local exprs = {}
    local subexprs = {}
    local i = 1
    local nsub = 0
    local subx = {list={},prefix=''}
    push(subexprs,subx)
    for _,arg in ipairs(args) do
        local set, block = arg:match '(.*){(.*)'
        -- enforce the rule that only an assignment can go before {
        if set and set ~= '' and not set:match('^[%w_-]+=$') then
            block = nil
        end
        if block then
            push(subexprs,subx)
            subx = {list={},prefix=set}
            if block:match('}$') then
                -- so foo}} becomes foo, with }} needing to be further processed
                block,arg = block:match('([^}]*)(}+)')
            end
            if #block > 0 then
                push(subx.list,block)
            end
        end 
        local endblock,closes = arg:match '^([^}]*)(}+)$'
        if endblock then
            if #endblock > 0 then
                push(subx.list,endblock)
            end
            for i = 1,#closes do
                local xsub = SUBMARKER..subexpr(subx.list,'subexpr')
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
    arg = pop(subexprs).list
    return arg, nsub
end

function is_iden(name)
    return name:match('^%a[%w_]*$')
end

function is_op(name)
    return name and (name:match('^[%+%*<>/|~&]+$') or name=='and' or name=='or')
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

function is_global_fun(expr)
    return type(dotted_lookup(expr)) == 'function'
end

function is_conv_fun(expr)
    if expr == 'not' then
        return true
    end
    return is_global_fun(expr) and _conversions[expr]
end

local string_fun = '^(.+)'..stringer..'$'
local string_name = '^(%a[%w_]*)'..stringer..'$'

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

function subexpr(arg,iter)
    --  f a b ... becomes f(a,b,...)
    local tbl,conv,lambda_args
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
    local args,vars,implicit,nsub = {},{}
    -- {} subexpressions may be {x: f[x]} lambdas!
    if iter == 'subexpr' then
        lambda_args = arg[1]:match('^([%w_,]*):$')
        if lambda_args then
            arg = slice(arg,2)
        end
    end
    -- collect any {..} and apply quote() to them
    arg = collect_subexprs(arg)
    local expr = sub_marker(quote(arg[1]))
    
    -- top-level - allow for output conversions like bin or hex
    if iter == 'expr' and is_conv_fun(expr) then
        conv = expr
        arg = slice(arg,2)
        expr = sub_marker(quote(arg[1]))
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
                implicit = 'list'
            elseif expr == 'eval' then
                expr = ''
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
        local val,sub,var = sub_marker(arg[i])
        if not sub then 
            val,var = split_key_val(val)
        end
        if not quoted_fun then
            val = quote(val)
        end
        local im1 = i - 1
        if var then
            has_vars = true
            vars[im1] = var
        else
            vars[im1] = ''      
        end
        val,sub = sub_marker(val)
        if quoted_fun and not sub and tostring(val) ~= 'null' then
            val = squote(val)
        end
        args[im1] = val
    end
    if expr == '' and has_vars then
        expr = 'eval'
    end
    if is_global_fun(expr) then
        local call
        if has_vars then
            -- key-value pairs as args means collect as single TABLE
            for i = 1,#args do
               if vars[i] ~= '' then    
                   local a = args[i]
                   -- functions get string-quoted (to be saved) but do check them!
                   if is_function(a) then
                        local _,e = load('return '..a)
                        if e then
                            quit('cannot compile '..e)
                        end
                        a = ('%q'):format(a)
                   end
                   args[i] = quote_key(vars[i])..'='..a
               end     
            end
        end
        -- this ad-hockery is for COOL COLOUR FORMATS
        if expr == 'format' or expr == 'printf' then
            args[1] = massage_format(args[1])
        end 
        call = table.concat(args,',')
        if not has_vars then
            expr = expr..'('..call..')'
        else
             -- this is to by-pass the list() function when we have a table containing k-v pairs
            if expr == 'list' then expr = '' end
            expr = expr..'{'..call..'}'
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
    elseif lambda_args then
        expr = 'function('..lambda_args..') return '..expr..' end'
    end
    return expr
end

function main(arg)
    if #arg == 0 then
        quit('el <expression>. Like sin[2*pi] or date ^%c')
    end
    loads()
    set_global_lookup()
    debug = os.getenv('ELDBG') or os.getenv('eldbg')
    ELSEP = '\t'

    local itera,expra = split2(arg,'do')
    local print_final,expr = true
    
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
    
    -- there can be a chain of expressions 'ex1 : ex2 : ex2'.
    -- The first is special (sexpra) because it will determine implicit looping
    -- The last is special because it does the final print and may have an if-clause
    local cexpra, rest = split2(expra,':')
    local sexpra = cexpra
    local subx,endf = '',''
    if #rest > 0 then
        subx = 'local '
        while #rest > 0 do
            local cexpr = subexpr(cexpra,'expr')
            subx = subx .. 'it='..cexpr..'\nif not it then goto fin end\n'
            cexpra,rest = split2(rest,':')
        end
        expra = cexpra
        endf = ' ::fin::\n'
    end
    if not itera and contains(sexpra,'it') then
        -- implicit loop over all lines in stdin
        itera = {'lines'}
    end
    
    local ifcond
    local cexpra,fexpra = split2(expra,'if')
    if #fexpra > 0 then
        expra = cexpra
        local fexpr = subexpr(fexpra,'cond')
        ifcond = 'if '..fexpr..' then '
    end
    local iexpr = 'printx('..subexpr(expra,'expr')..')'
    if ifcond then
        iexpr = ifcond .. iexpr .. ' end'
    end
    expr = subx .. iexpr .. endf
    if itera then -- 'do'
        local iter = subexpr(itera,'iter')
        expr = ('local row=1; for it,this in %s do %s; row=row+1 end'):format(iter,expr)
        print_final = false
    end

    if debug then
        print(expr)
    end
    
    local f,e = load(expr,'expr')
    if e then
        print('error',e,expr)
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

main(arg)
