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
        [fun add_topic_test/1,
          fun get_topic_test/1,
          fun add_message_test/1,
          fun get_message_test/1,
          fun get_message_ack_test/1,
          fun get_message_acks_test/1,
          fun delete_message_ack_test/1]
      }
    end}.

start() ->
        application:start(gproc),
        {ok, _} = erlchat_session_sup:start_link(),
        {ok, Pid} = erlchat_session_mgr:start_link(),
        Pid.

stop() ->
        erlchat_session_mgr:stop().

start_stop_server_test_() ->
        {setup,
         fun() ->
           application:start(gproc),
           {ok, _} = erlchat_session_sup:start_link(),
           {ok, Pid} = erlchat_session_mgr:start_link(),
           Pid
         end,
         fun(_) -> erlchat_session_mgr:stop() end,
         fun is_registered/1}.

get_user_session_test_() ->
        {setup,
          fun() ->
            erlchat_session_mgr:start_link(),
            UserId = <<"user1">>,
            {initiated, Session} = erlchat_session_mgr:init_session(UserId),
            {UserId, Session}
          end,
          fun(_) -> erlchat_session_mgr:stop() end,
          fun get_session/1}.

get_users_sessions_test_() ->
        {setup,
          fun() ->
            erlchat_session_mgr:start_link(),
            UserId = <<"user1">>,
            {initiated, S1} = erlchat_session_mgr:init_session(UserId),
            {initiated, S2} = erlchat_session_mgr:init_session(UserId),
            {initiated, S3} = erlchat_session_mgr:init_session(UserId),
            {UserId, [S1, S2, S3]}
          end,
          fun(_) -> erlchat_session_mgr:stop() end,
          fun get_sessions/1}.

terminate_session_test_() ->
        {setup,
          fun() ->
            erlchat_session_mgr:start_link(),
            UserId = <<"user1">>,
            {initiated, Session} = erlchat_session_mgr:init_session(UserId),
            {initiated, _} = erlchat_session_mgr:init_session(UserId),
            {initiated, _} = erlchat_session_mgr:init_session(UserId),
            {UserId, Session}
          end,
          fun(_) -> erlchat_session_mgr:stop() end,
          fun terminate_session/1}.

is_registered(Pid) ->
        [?_assert(erlang:is_process_alive(Pid)),
         ?_assertEqual(Pid, whereis(?session_server))].

get_session({UserId, Session=#erlchat_session_info{}}) ->
        [?_assertMatch({ok, [Session]}, erlchat_session_mgr:get_user_session_infos(UserId)),
         ?_assertMatch({ok, Session}, erlchat_session_mgr:get_session_info(Session#erlchat_session_info.id))].

get_sessions({UserId, Sessions}) when is_list(Sessions) ->
        {ok, TestSessions} = erlchat_session_mgr:get_user_session_infos(UserId),
        FoldFun = fun(Session, Asserts) ->
                    [?_assert(lists:any(fun(Elem) -> Elem =:= Session end, TestSessions)) | Asserts]
                  end,
        Asserts = lists:foldl(FoldFun, [], Sessions),
        [?_assertEqual(lists:flatlength(Sessions), lists:flatlength(TestSessions)) | Asserts].

terminate_session({UserId, #erlchat_session_info{ id = SessionId }}) ->
        {ok, terminated} = erlchat_session_mgr:terminate_session(SessionId),
        {ok, Sessions} = erlchat_session_mgr:get_user_session_infos(UserId),
        Pred = fun(#erlchat_session_info{ id = Id }) ->
                SessionId =/= Id
               end,
        [?_assert(lists:all(Pred, Sessions))].