BEGIN {
    pwd = ""
}

/\$ cd / {
    path = $3
    if(path ~ /\/ *$/) { # absolute path
        pwd = path
    } else if(path == "..") {
        n = split(pwd, parts, "/")
        pwd = "/"
        for(i = 2; i < n - 1; i++) { # i = 2 to ignore the starting slash
            pwd = pwd parts[i] "/"
        }
    } else {
        pwd = pwd path "/"
    }
}

/[0-9]+ [a-zA-Z0-9.]+/ {
    file = $2
    size = $1

    n = split(pwd, parts, "/")
    dir = ""
    for(i = 1; i < n; i++) {
        dir = dir parts[i] "/"

        dirs[dir] += size
    }
}

END {
    sum = 0

    total_space = 70000000
    needed_space = 30000000

    used_space = dirs["/"]
    needed_free = used_space + needed_space - total_space

    # size of the smallest directory that if removed would free up enough space
    smallest_size = dirs["/"] # / is always the biggest dir that meets the condition
    for(path in dirs) {
        size = dirs[path]
        if(size <= 100000) {
            sum += size
        }

        if(size >= needed_free && size < smallest_size) {
            smallest_size = size
        }
    }

    print "Result 1: " sum
    print "Result 2: " smallest_size
}
