local ltn12 = require("ltn12")

function GetInput()
    local f = io.open("../../session", "r")
    assert(f ~= nil)
    local session = (f:read "*a"):gsub("[\n\\s]", "")

    local https = require "ssl.https"
    local res = {}
    https.request{
        url = "https://adventofcode.com/2022/day/5/input",
        headers = { ["Cookie"] = "session=" .. session },
        sink = ltn12.sink.table(res)
    }

    return table.concat(res)
end

function ParseState(input)
    local index = 1
    local x = 1
    local maxx = 1
    local stacks = {}
    local chunk = input:sub(1, 4)
    while(chunk:find("^[^0-9]+$")) do
        local letter = chunk:gmatch("[A-Z]")()

        if letter ~= nil then
            stacks[x] = stacks[x] or { last = 0 }
            local slot = stacks[x].last + 1
            stacks[x][slot] = letter
            stacks[x].last = slot
        end

        index = index + 4
        x = x + 1
        if(chunk:find("\n")) then
            x = 1
        end

        maxx = math.max(x, maxx)

        chunk = input:sub(index, index + 3)
    end
    -- skip line with numbers and the blank line after that
    index = index + maxx * 4 + 1

    -- reverse the stacks
    for _, v in pairs(stacks) do
        local i1 = 1
        local i2 = v.last
        while(i1 < i2) do
            local tmp = v[i1]
            v[i1] = v[i2]
            v[i2] = tmp

            i1 = i1 + 1
            i2 = i2 - 1
        end
    end

    return input:sub(index), stacks
end


function PrintStacks(stacks)
    for k, v in pairs(stacks) do
        local s = string.format("%d:", k)
        for i=1,v.last do
            s = s .. string.format(" [%s]", v[i])
        end
        print(s)
    end
    print("")
end

function CloneStacks(stacks)
    local res = {}
    for k, v in pairs(stacks) do
        res[k] = { last = v.last }
        for i=1,v.last do
            res[k][i] = v[i]
        end
    end

    return res
end

function Part1(input, stacks)
    local iter = input:gmatch("[0-9]+")
    local amount, from, to = tonumber(iter()), tonumber(iter()), tonumber(iter())
    while(amount ~= nil) do
        for _=1,amount do
            local slot_to = stacks[to].last + 1
            local slot_from = stacks[from].last

            stacks[to][slot_to] = stacks[from][slot_from]
            table.remove(stacks[from], slot_from)

            stacks[from].last = slot_from - 1
            stacks[to].last = slot_to
        end

        print(string.format("move %d from %d to %d", amount, from, to))
        PrintStacks(stacks)

        amount, from, to = tonumber(iter() or ""), tonumber(iter() or ""), tonumber(iter() or "")
    end

    local result = ""
    for _, v in pairs(stacks) do
        result = result .. v[v.last]
    end
    print("Result: " .. result)

    return result
end

function Part2(input, stacks)
    local iter = input:gmatch("[0-9]+")
    local amount, from, to = tonumber(iter()), tonumber(iter()), tonumber(iter())
    while(amount ~= nil) do
        for a=amount,1, -1 do
            local slot_to = stacks[to].last + a
            local slot_from = stacks[from].last

            stacks[to][slot_to] = stacks[from][slot_from]
            table.remove(stacks[from], slot_from)

            stacks[from].last = slot_from - 1
        end

        stacks[to].last = stacks[to].last + amount

        print(string.format("move %d from %d to %d", amount, from, to))
        PrintStacks(stacks)

        amount, from, to = tonumber(iter() or ""), tonumber(iter() or ""), tonumber(iter() or "")
    end

    local result = ""
    for _, v in pairs(stacks) do
        result = result .. v[v.last]
    end
    print("Result: " .. result)
    
    return result
end

local input, stacks = ParseState(GetInput())

print(" ============ state ============")
PrintStacks(stacks)
print(" ============ part1 ============")
local res1 = Part1(input, CloneStacks(stacks))
print(" ===============================")
print("")

print(" ============ state ============")
PrintStacks(stacks)
print(" ============ part2 ============")
local res2 = Part2(input, stacks)
print(" =========== summary ===========")
print(" Part 1: " .. res1)
print(" Part 2: " .. res2)
print(" ===============================")

