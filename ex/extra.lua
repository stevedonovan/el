local push = table.insert 
local debug = true

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

    local cmdline = table.concat(cmd,' ')
    if debug then
        print('cmd',cmdline)
    end
    return sh(cmdline)
end

function curl(p)
    if type(p) == 'string' then
        p = {p}
    end
    local url = p[1]
    if not url:match('^%a+://') and URL then
        p[1] = URL..url
    end
    p.silent = true
    table.insert(p,1,'curl')
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
    local ty = type(p.data)
    if ty == 'table' then
        p['data-binary'] = json(p.data)
        if not p.headers then p.headers = {} end
        p.headers['content-type'] = 'application/json'
    end    
    local headers = eat(p,'headers')
    if headers then
        p.header = {}
        for k,v in pairs(headers) do
            push(p.header,k..': '..v)
        end
    end
    --~ dump('p',p)
    return exec(p)
end

function patch(s,i1,i2,fn)
    return s:sub(1,i1-1)..fn(s:sub(i1,i2))..s:sub(i2+1)
end

function subst(s,t)
    return gsub(s,'%${([^}]+)}',t)
end
