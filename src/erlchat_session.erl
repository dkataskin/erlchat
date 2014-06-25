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

-record(session_state, { id = <<>>, handlers = []}).

-behaviour(gen_server).

%% API
-export([start_link/1, stop/1]).
-export([reg_sub_session/2, unreg_sub_session/2]).
-export([start_topic/4]).
-export([send_message/3]).

%% gen_serve callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API
start_link(SessionId) ->
        gen_server:start_link(?MODULE, [SessionId], []).

stop(Pid) ->
        gen_server:call(Pid, stop).

reg_sub_session(Pid, HandlerPid) ->
        gen_server:call(Pid, {reg_sub_session, HandlerPid}).

unreg_sub_session(Pid, HandlerPid) ->
        gen_server:call(Pid, {unreg_sub_session, HandlerPid}).

start_topic(Pid, Users, Subject, Text) ->
        gen_server:call(Pid, {start_topic, {Users, Subject, Text}}).

send_message(Pid, TopicId, Text) ->
        gen_server:call(Pid, {send_message, {TopicId, Text}}).

%% gen_server callbacks
init([SessionId]) ->
        gproc:add_local_name(SessionId),
        {ok, #session_state { id = SessionId, handlers = [] }}.

handle_call({reg_sub_session, HandlerPid}, _From, State=#session_state { handlers = Handlers }) ->
        State1 = State#session_state { handlers = [HandlerPid | Handlers] },
        io:format("~p registered ~p sub session~n", [self(), HandlerPid]),
        {reply, ok, State1};

handle_call({unreg_sub_session, HandlerPid}, _From, State=#session_state { handlers = Handlers }) ->
        State1 = State#session_state { handlers = Handlers -- [HandlerPid] },
        io:format("~p unregistered ~p sub session~n", [self(), HandlerPid]),
        {reply, ok, State1};

handle_call({start_topic, {Users, Subject, Text}}, _From, State=#session_state{}) ->
        {reply, ok, State};

handle_call(stop, _From, State) ->
        {stop, normal, State}.

handle_cast(_Request, State) ->
        {noreply, State}.

handle_info(_Info, State) ->
        {noreply, State}.

terminate(_Reason, _State) ->
        ok.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.
