#!/usr/bin/env escript
-include("../deps/of_protocol/include/of_protocol.hrl").
-include("../deps/of_protocol/include/ofp_v4.hrl").

%% Passive LINCX controller script for Hetzner worker hosts.
%% Waits for connection and setup flow tables.
%% Takes subnet number (172.16.X.0) as an argument.
main([Arg]) ->
    Subnet = list_to_integer(Arg),
    %% get root lincx dir by removing last two entries of script path
    LincxDir = filename:join(lists:reverse(tl(tl(lists:reverse(
        filename:split(filename:absname(escript:script_name()))))))),
    code:add_path(filename:join(LincxDir,"deps/of_protocol/ebin")),

    {ok, LSock} = gen_tcp:listen(6653, [binary, {packet, raw},{active, once},{reuseaddr, true}]),
    {ok, Sock} = gen_tcp:accept(LSock),

    message(Sock, #ofp_hello{}),

    message(Sock, flow_mod(1,9)),
    message(Sock, flow_mod(2,9)),
    message(Sock, flow_mod(3,9)),
    message(Sock, flow_mod(4,9)),
    message(Sock, flow_mod(5,9)),
    message(Sock, flow_mod(6,9)),
    message(Sock, flow_mod(7,9)),
    message(Sock, flow_mod(8,9)),

    message(Sock, flow_mod2(9,1,Subnet)),
    message(Sock, flow_mod2(9,2,Subnet)),
    message(Sock, flow_mod2(9,3,Subnet)),
    message(Sock, flow_mod2(9,4,Subnet)),
    message(Sock, flow_mod2(9,5,Subnet)),
    message(Sock, flow_mod2(9,6,Subnet)),
    message(Sock, flow_mod2(9,7,Subnet)),
    message(Sock, flow_mod2(9,8,Subnet));
main(_) ->
    io:format("usage: hetzner [1-3]\n").

flow_mod(InPort, OutPort) ->
    Fields = [
        #ofp_field{name = in_port,value = <<InPort:32>>}
    ],

    Instruction = #ofp_instruction_apply_actions{
        actions = [#ofp_action_output{port = OutPort}]
    },

    #ofp_flow_mod{
        table_id = 0,
        command = add,
        match = #ofp_match{fields = Fields},
        instructions = [Instruction]
    }.

flow_mod2(InPort, OutPort, Subnet) ->
    Fields = [
        #ofp_field{name = in_port, value = <<InPort:32>>},
        #ofp_field{name = eth_type,value = <<2048:16>>},
        #ofp_field{name = ipv4_dst,value = <<172:8,16:8,Subnet:8,OutPort:8>>}
    ],

    Instruction = #ofp_instruction_apply_actions{
        actions = [#ofp_action_output{port = OutPort}]
    },

    #ofp_flow_mod{
        table_id = 0,
        command = add,
        match = #ofp_match{fields = Fields},
        instructions = [Instruction]
    }.

message(Sock, Body) ->
    {ok, Bin} = 
        of_protocol:encode(#ofp_message{
            version = 4,
            xid = random:uniform(1 bsl 32 - 1),
            body = Body
        }),
    ok = gen_tcp:send(Sock, Bin).
