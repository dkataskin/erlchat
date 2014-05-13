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
% * Neither the name of the {organization} nor the names of its
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

-module(erlchat_sessions_tests).
-author("Dmitry Kataskin").

-include("erlchat.hrl").

-include_lib("eunit/include/eunit.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%% API
-export([]).

start_stop_server_test_() ->
                {setup,
                 fun() ->
                   {ok, Pid} = erlchat_sessions:start_link(),
                   Pid
                 end,
                 fun(_) -> erlchat_sessions:stop() end,
                 fun is_registered/1}.

get_session_test_() ->
                {setup,
                  fun() ->
                    erlchat_sessions:start_link(),
                    UserId = <<"user1">>,
                    {initiated, Session} = erlchat_sessions:init_session(UserId, <<"session1">>),
                    {UserId, Session}
                  end,
                  fun(_) -> erlchat_sessions:stop() end,
                  fun get_session/1}.

get_sessions_test_() ->
                {setup,
                  fun() ->
                    erlchat_sessions:start_link(),
                    UserId = <<"user1">>,
                    {initiated, S1} = erlchat_sessions:init_session(UserId, <<"session1">>),
                    {initiated, S2} = erlchat_sessions:init_session(UserId, <<"session2">>),
                    {initiated, S3} = erlchat_sessions:init_session(UserId, <<"session3">>),
                    {UserId, [S1, S2, S3]}
                  end,
                  fun(_) -> erlchat_sessions:stop() end,
                  fun get_sessions/1}.

terminate_session_test_() ->
                {setup,
                  fun() ->
                    erlchat_sessions:start_link(),
                    UserId = <<"user1">>,
                    {initiated, Session} = erlchat_sessions:init_session(UserId, <<"session1">>),
                    {initiated, _} = erlchat_sessions:init_session(UserId, <<"session2">>),
                    {initiated, _} = erlchat_sessions:init_session(UserId, <<"session3">>),
                    {UserId, Session}
                  end,
                  fun(_) -> erlchat_sessions:stop() end,
                  fun terminate_session/1}.

is_registered(Pid) ->
                [?_assert(erlang:is_process_alive(Pid)),
                 ?_assertEqual(Pid, whereis(?session_server))].

get_session({UserId, Session}) ->
                [?_assertMatch({ok, [Session]}, erlchat_sessions:get_sessions(UserId))].

get_sessions({UserId, Sessions}) when is_list(Sessions) ->
                {ok, TestSessions} = erlchat_sessions:get_sessions(UserId),
                FoldFun = fun(Session, Asserts) ->
                            [?_assert(lists:any(fun(Elem) -> Elem =:= Session end, TestSessions)) | Asserts]
                          end,
                Asserts = lists:foldl(FoldFun, [], Sessions),
                [?_assertEqual(lists:flatlength(Sessions), lists:flatlength(TestSessions)) | Asserts].

terminate_session({UserId, #erlchat_session{ id = SessionId }}) ->
                {ok, terminated} = erlchat_sessions:terminate_session(SessionId),
                {ok, Sessions} = erlchat_sessions:get_sessions(UserId),
                Pred = fun(#erlchat_session { id = Id }) ->
                        SessionId =/= Id
                       end,
                [?_assert(lists:all(Pred, Sessions))].