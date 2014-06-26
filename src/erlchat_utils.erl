% Copyright (c) 2014, Dmitry Kataskin
% All rights reserved.
%
% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions are met:
%
% * Redistributions of source code must retain the above copyright notice, this
% list of conditions and the following disclaimer.
%
% * Redistributions in binary form must reproduce the above copyright notice,
% this list of conditions and the following disclaimer in the documentation
% and/or other materials provided with the distribution.
%
% * Neither the name of the erlchat nor the names of its
% contributors may be used to endorse or promote products derived from
% this software without specific prior written permission.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
% OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(erlchat_utils).
-author("Dmitry Kataskin").

%% API
-export([get_unique_id/0, priv_dir/0, generate_uuid/0, get_timestamp/0]).

get_unique_id() ->
        {M, S, U} = erlang:now(),
        <<N:64>> = <<M:24, S:20, U:20>>,
        N.

get_timestamp() ->
        {Mega, Sec, Micro} = erlang:now(),
        (Mega * 1000000 + Sec) * 1000000 + Micro.

generate_uuid() ->
        Now = {_, _, Micro} = now(),
        Nowish = calendar:now_to_universal_time(Now),
        Nowsecs = calendar:datetime_to_gregorian_seconds(Nowish),
        Then = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
        Prefix = io_lib:format("~14.16.0b", [(Nowsecs - Then) * 1000000 + Micro]),
        list_to_binary(Prefix ++ to_hex(crypto:rand_bytes(9))).

to_hex([]) ->
        [];

to_hex(Bin) when is_binary(Bin) ->
        to_hex(binary_to_list(Bin));
to_hex([H|T]) ->
        [to_digit(H div 16), to_digit(H rem 16) | to_hex(T)].

to_digit(N) when N < 10 ->
        $0 + N;
to_digit(N) ->
        $a + N-10.

priv_dir() ->
        Ebin = filename:dirname(code:which(erlchat)),
        filename:join(filename:dirname(Ebin), "priv").