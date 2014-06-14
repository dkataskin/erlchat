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

-module(erlchat_mnesia).
-author("Dmitry Kataskin").

-define(data_dir, data).

-include("erlchat.hrl").

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% gen_server callbacks
init(Args) ->
        DataDir = case proplists:get_value(data_dir, Args) of
          undefined ->
            filename:join([erlchat_utils:priv_dir(), ?data_dir]);
          Dir ->
            Dir
        end,
        application:set_env(mnesia, dir, DataDir),
        LocalNode = erlang:node(),
        install([LocalNode]),
        {ok, no_state}.

handle_call({get_user, Id}, _From, State) ->
        User = #erlchat_user { id = Id },
        {reply, User, State};

handle_call({get_topic, TopicId}, _From, State) ->
        {reply, get_topic(TopicId), State};

handle_call({start_new_topic, Topic=#erlchat_topic{}}, _From, State) ->
        Topic1 = add_topic(Topic),
        {reply, {created, Topic1}, State};

handle_call(stop, _From, State) ->
        {stop, normal, shutdown_ok, State}.

handle_cast(_Request, State) ->
        {noreply, State}.

handle_info(_Info, State) ->
        {noreply, State}.

terminate(_Reason, _State) ->
        ok.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

% Schema
install(Nodes) ->
        case mnesia:create_schema(Nodes) of
          ok ->
            rpc:multicall(Nodes, application, start, [mnesia]),
            ok = create_tables(Nodes);

          {error, {_, {already_exists, _}}} ->
            ok
        end,
        rpc:multicall(Nodes, application, start, [mnesia]),
        ok.

create_tables(Nodes) ->
        {atomic, ok} = mnesia:create_table(erlchat_user, [{attributes, record_info(fields, erlchat_user)},
                                                          %{index, [#erlchat_user.id]},
                                                          {disc_only_copies, Nodes},
                                                          {type, set}]),

        {atomic, ok} = mnesia:create_table(erlchat_topic, [{attributes, record_info(fields, erlchat_topic)},
                                                           %{index, [#erlchat_topic.id]},
                                                           {disc_only_copies, Nodes},
                                                           {type, set}]),

        {atomic, ok} = mnesia:create_table(erlchat_message, [{attributes, record_info(fields, erlchat_message)},
                                                             %{index, [#erlchat_message.id]},
                                                             {disc_only_copies, Nodes},
                                                             {type, set}]),
        ok.

add_topic(Topic=#erlchat_topic{}) ->
        Topic1 = Topic#erlchat_topic{ id = erlchat_utils:generate_uuid() },
        mnesia:activity(transaction, fun() -> mnesia:write(Topic1) end),
        Topic1.

get_topic(TopicId) ->
        case mnesia:activity(transaction, fun() -> mnesia:read({erlchat_topic, TopicId}) end) of
          [Topic] ->
            {ok, Topic};
          [] ->
            {error, not_found}
        end.
