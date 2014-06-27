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

-module(erlchat_session_mgr_tests).
-author("Dmitry Kataskin").

-include("erlchat.hrl").

-include_lib("eunit/include/eunit.hrl").
-include_lib("xmerl/include/xmerl.hrl").

all_test_() ->
        {setup,
          fun start/0,
          fun stop/1,
          fun(Pid) ->
            {with, Pid,
              [fun is_registered/1,
               fun get_session_infos/1,
               fun get_sessions/1,
               fun terminate_session/1]
            }
          end}.

start() ->
        application:start(gproc),
        {ok, _} = erlchat_session_sup:start_link(),
        {ok, Pid} = erlchat_session_mgr:start_link(),
        Pid.

stop(_Pid) ->
        erlchat_session_mgr:stop().

is_registered(Pid) ->
        ?_assert(erlang:is_process_alive(Pid)),
        ?_assertEqual(Pid, whereis(?session_mgr)).

get_session_infos(_Pid) ->
        UserId = erlchat_utils:generate_uuid(),
        {initiated, Session} = erlchat_session_mgr:init_session(UserId),
        ?assertMatch({ok, [Session]}, erlchat_session_mgr:get_session_infos(UserId)),
        ?assertMatch({ok, Session}, erlchat_session_mgr:get_session_info(Session#erlchat_session_info.id)).

get_sessions(_Pid) ->
        UserId = erlchat_utils:generate_uuid(),
        {initiated, S1} = erlchat_session_mgr:init_session(UserId),
        {initiated, S2} = erlchat_session_mgr:init_session(UserId),
        {initiated, S3} = erlchat_session_mgr:init_session(UserId),
        Sessions = [S1, S2, S3],

        {ok, TestSessions} = erlchat_session_mgr:get_session_infos(UserId),
        Fun = fun(#erlchat_session_info { id = Id }) ->
                    ?assert(lists:any(fun(#erlchat_session_info { id = SessionId }) ->
                                        SessionId =:= Id
                                      end, Sessions))
              end,
        lists:foreach(Fun, TestSessions),
        ?_assertEqual(lists:flatlength(Sessions), lists:flatlength(TestSessions)).

terminate_session(_Pid) ->
        UserId = erlchat_utils:generate_uuid(),
        {initiated, Session} = erlchat_session_mgr:init_session(UserId),
        {initiated, _} = erlchat_session_mgr:init_session(UserId),
        {initiated, _} = erlchat_session_mgr:init_session(UserId),

        SessionId = Session#erlchat_session_info.id,

        {ok, terminated} = erlchat_session_mgr:terminate_session(SessionId),
        {ok, Sessions} = erlchat_session_mgr:get_session_infos(UserId),
        Pred = fun(#erlchat_session_info{ id = Id }) ->
                SessionId =/= Id
               end,
        ?assert(lists:all(Pred, Sessions)).