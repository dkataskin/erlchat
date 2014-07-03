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

-module(erlchat_session).
-author("Dmitry Kataskin").

-include("erlchat.hrl").

-behaviour(gen_server).

-record(session_state, { session_id = <<>> }).

% API
-export([start_link/1, stop/1]).
-export([start_topic/4]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% API
start_link(SessionId) ->
        gen_server:start_link(?MODULE, [SessionId], []).

stop(Pid) ->
        gen_server:call(Pid, stop).

start_topic(Pid, Users, Subject, Text) ->
        gen_server:call(Pid, {start_topic, {Users, Subject, Text}}).

% gen_server callbacks
init([SessionId]) ->
        true = gproc:add_local_name(SessionId),
        {ok, #session_state{ session_id = SessionId }}.

handle_call({start_topic, {Owner, Users, Subject, Text}}, _From, State=#session_state{ session_id = SessionId }) ->
        {ok, Session} = get_session(SessionId),
        Owner = Session#erlchat_session_info.user_id,
        {ok, {_, _, MessageAcks}} = erlchat_store:add_topic(Owner, Users, Subject, Text),
        ok = notify(MessageAcks),
        {reply, ok, State};

handle_call(stop, _From, State) ->
        {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
        {reply, ok, State}.

handle_cast(_Request, State) ->
        {noreply, State}.

handle_info(_Info, State) ->
        {noreply, State}.

terminate(_Reason, _State) ->
        ok.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

get_session(SessionId) ->
        erlchat_session_mgr:get_session_info(SessionId).

notify(MessageAcks) ->
        ForEach = fun(MessageAck=#erlchat_message_ack { user_id = UserId }) ->
                    UserSessions = erlchat_session_mgr:get_session_infos(UserId)
                  end,
        ok.

