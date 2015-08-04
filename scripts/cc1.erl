#!/usr/bin/env escript
-include("../deps/of_protocol/include/of_protocol.hrl").
-include("../deps/of_protocol/include/ofp_v4.hrl").

%% LINCX controller script for cc master.
main(_) ->
    %% get root lincx dir by removing last two entries of script path
    LincxDir = filename:join(lists:reverse(tl(tl(lists:reverse(
        filename:split(filename:absname(escript:script_name()))))))),
    code:add_path(filename:join(LincxDir,"deps/of_protocol/ebin")),

    {ok, LSock} = gen_tcp:listen(6653, [binary, {packet, raw},{active, once},{reuseaddr, true}]),
    {ok, Sock} = gen_tcp:accept(LSock),
%    {ok, Sock} = gen_tcp:connect("localhost",6653,[binary, {packet, raw},{reuseaddr, true}]),

    message(Sock, #ofp_hello{}),

    message(Sock, flow_mod(1,2)),
    message(Sock, flow_mod(2,1)),

    io:format("LINCX configured\n").

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

message(Sock, Body) ->
    {ok, Bin} = 
        of_protocol:encode(#ofp_message{
            version = 4,
            xid = random:uniform(1 bsl 32 - 1),
            body = Body
        }),
    ok = gen_tcp:send(Sock, Bin).
