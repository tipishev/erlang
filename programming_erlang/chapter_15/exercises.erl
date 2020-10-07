-module(exercises).
-export([cpu_type/0]).

%% ex 15.3
cpu_type() ->
    CpuInfo = os:cmd("cat /proc/cpuinfo"),
    [_, ModelInfoOnward] = string:split(CpuInfo, "model name\t: "),
    [ModelInfo, _] = string:split(ModelInfoOnward, "\n"),
    ModelInfo.
