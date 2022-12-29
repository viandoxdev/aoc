package = "5"
version = "dev-1"
source = {
   url = "none"
}
description = {
   homepage = "none",
   license = "none"
}
dependencies = {
   "lua >= 5.4",
   "luasec >= 1.1.0",
   "luasocket >= 3.0.0"
}
build = {
   type = "builtin",
   modules = {
      main = "main.lua"
   }
}
