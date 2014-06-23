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

-module(erlchat_handler).
-author("Dmitry Kataskin").

-export([init/4]).
-export([stream/3]).
-export([info/3]).
-export([terminate/2]).

init(_Transport, Req, _Opts, _Active) ->
        case is_session_valid(Req) of
          {{error, _Reason}, Req1} ->
            {shutdown, Req1, undefined_state};

          {{ok, Session}, Req1} ->
            {ok, Req1, Session}
        end.

stream(<<"ping: ", Name/binary>>, Req, State) ->
        io:format("ping ~p received~n", [Name]),
        {reply, <<"pong">>, Req, State};

stream(Data, Req, State) ->
        case jsx:is_json(Data) of
          true ->
            io:format("stream received valid json ~s~n", [Data]),
            {ok, Req, State};
          false ->
            io:format("stream received something ~s~n", [Data]),
            {ok, Req, State}
        end.

info(Info, Req, State) ->
        io:format("info received ~p~n", [Info]),
        {ok, Req, State}.

terminate(_Req, _State) ->
        io:format("bullet terminate~n"),
        ok.

is_session_valid(Req) ->
        case cowboy_req:cookie(<<"erlchat_session_id">>, Req) of
          {undefined, Req1} ->
            {{error, no_cookie}, Req1};

          {SessionId, Req1} ->
            case erlchat_sessions:get_session(SessionId) of
              {error, not_found} ->
                {{error, no_session}, Req1};

              {ok, Session} ->
                {{ok, Session}, Req1}
            end
         end.
