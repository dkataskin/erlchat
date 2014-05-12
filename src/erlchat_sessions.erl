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

-module(erlchat_sessions).
-author("Dmitry Kataskin").

-define(sessions_table, erlchat_sessions).

-include("erlchat.hrl").

-behaviour(gen_server).

%% API
-export([start/0, init_session/2, terminate_session/1, get_sessions/1]).

%% gen server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API
start() ->
        gen_server:start_link({local, ?session_server}, ?MODULE, [], []).

init_session(UserId, SessionKey) ->
                gen_server:call(?session_server, {init_session, {UserId, SessionKey}}).

get_sessions(UserId) ->
                gen_server:call(?session_server, {get_sessions, UserId}).

terminate_session(SessionKey) ->
                gen_server:call(?session_server, {terminate_session, SessionKey}).

% gen server callbacks
init(_Args) ->
        Id = ets:new(?sessions_table, [duplicate_bag,
                                      {keypos, #erlchat_session.session_key},
                                      {read_concurrency, true}]),
        {ok, Id}.

handle_call({init_session, {UserId, SessionKey}}, _From, State) ->
                SessionsTableId = State,
                true = ets:insert(SessionsTableId, #erlchat_session{ session_key = SessionKey,
                                                                     user_id = UserId,
                                                                     last_seen = erlang:now() }),
                {reply, {ok, initiated}, State};

handle_call({get_sessions, UserId}, _From, State) ->
                SessionsTableId = State,
                Keys = ets:match(SessionsTableId, {erlchat_session, '$1', UserId, '_', '_', '_'}),
                Sessions = lists:map(fun([SessionKey]) -> ets:lookup(SessionsTableId, SessionKey) end, Keys),
                {reply, {ok, Sessions}, State};

handle_call({terminate_session, SessionKey}, _From, State) ->
                SessionsTableId = State,
                ets:delete(SessionsTableId, SessionKey),
                {reply, {ok, terminated}, State}.

handle_cast(_Request, State) ->
                {noreply, State}.

handle_info(_Info, State) ->
                {noreply, State}.

terminate(_Reason, _State) ->
                ok.

code_change(_OldVsn, State, _Extra) ->
                {ok, State}.
