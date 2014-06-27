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

-module(erlchat_session_mgr).
-author("Dmitry Kataskin").

-define(sessions_table, erlchat_sessions).

-include("erlchat.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).
-export([init_session/1, terminate_session/1]).
-export([get_user_session_infos/1, get_session_info/1]).

%% gen server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API
start_link() ->
        gen_server:start_link({local, ?session_mgr}, ?MODULE, [], []).

stop() ->
        gen_server:call(?session_mgr, stop).

init_session(UserId) ->
        gen_server:call(?session_mgr, {init_session, UserId}).

get_user_session_infos(UserId) ->
        gen_server:call(?session_mgr, {get_session_infos, UserId}).

get_session_info(SessionId) ->
        gen_server:call(?session_mgr, {get_session_info, SessionId}).

terminate_session(SessionId) ->
        gen_server:call(?session_mgr, {terminate_session, SessionId}).

% gen server callbacks
init(_Args) ->
        Id = ets:new(?sessions_table, [bag,
                                      {keypos, #erlchat_session_info.id},
                                      {read_concurrency, true}]),
        {ok, Id}.

handle_call({init_session, UserId}, _From, State) ->
        SessionsTableId = State,
        SessionId = erlchat_utils:generate_uuid(),
        {ok, _Pid} = erlchat_session_sup:start_session(SessionId),
        Session = #erlchat_session_info{ id = erlchat_utils:generate_uuid(),
                                    user_id = UserId,
                                    last_seen = erlang:now() },
        true = ets:insert(SessionsTableId, Session),
        {reply, {initiated, Session}, State};

handle_call({get_session_infos, UserId}, _From, State) ->
        SessionsTableId = State,
        MS = ets:fun2ms(fun(S = #erlchat_session_info{ user_id = SUserId }) when SUserId =:= UserId -> S end),
        Sessions = ets:select(SessionsTableId, MS),
        {reply, {ok, Sessions}, State};

handle_call({get_session_info, SessionId}, _From, State) ->
        SessionsTableId = State,
        Resp = case ets:lookup(SessionsTableId, SessionId) of
                [Session] -> {ok, Session};
                [] -> {error, not_found}
               end,
        {reply, Resp, State};

handle_call({terminate_session, SessionId}, _From, State) ->
        SessionsTableId = State,
        ets:delete(SessionsTableId, SessionId),
        case gproc:lookup_local_name(SessionId) of
          undefined -> ok;
          Pid -> erlchat_session:stop(Pid)
        end,
        {reply, {ok, terminated}, State};

handle_call(stop, _From, State) ->
        {stop, normal, ok, State}.

handle_cast(_Request, State) ->
        {noreply, State}.

handle_info(_Info, State) ->
        {noreply, State}.

terminate(_Reason, _State) ->
        ok.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.
