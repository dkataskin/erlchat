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
-define(users_table, erlchat_user).
-define(topics_table, erlchat_topic).
-define(messages_table, erlchat_message).
-define(messages_ack_table, erlchat_message_ack).
-define(erlchat_mnesia_tables, [?users_table, ?topics_table, ?messages_table, ?messages_ack_table]).

-include("erlchat.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

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
        Nodes = [erlang:node()],

        ok = case proplists:get_value(resync, Args) of
              undefined ->
                ok;
              _ ->
                uninstall(Nodes)
             end,

        ok = install(Nodes),
        ok = mnesia:wait_for_tables(?erlchat_mnesia_tables, 5000),
        {ok, no_state}.

handle_call({get_user, Id}, _From, State) ->
        User = #erlchat_user { id = Id },
        {reply, User, State};

handle_call({get_topic, TopicId}, _From, State) ->
        {reply, get_topic(TopicId), State};

handle_call({add_topic, {Owner, Users, Subject, Text}}, _From, State) ->
        {ok, Resp} = add_topic(Owner, Users, Subject, Text),
        {reply, {created, Resp}, State};

handle_call({add_message, {Sender, TopicId, Text}}, _From, State) ->
        case add_message(Sender, TopicId, Text) of
          {ok, Resp} ->
            {reply, {created, Resp}, State};
          {error, Error} ->
            {reply, {error, Error}, State}
        end;

handle_call({get_message, MessageId}, _From, State) ->
        {reply, get_message(MessageId), State};

handle_call({get_message_ack, MessageAckId}, _From, State) ->
        {reply, find_single_tr(?messages_ack_table, MessageAckId), State};

handle_call({get_message_acks, {Sender, TopicId}}, _From, State) ->
        MessageAcks = get_message_acks(Sender, TopicId),
        {reply, {ok, MessageAcks}, State};

handle_call({delete_message_ack, MessageAckId}, _From, State) ->
        ok = delete_message_ack(MessageAckId),
        {reply, {ok, deleted}, State};

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

uninstall(Nodes) ->
       application:stop(mnesia),
       ok = mnesia:delete_schema(Nodes),
       ok.

create_tables(Nodes) ->
        {atomic, ok} = mnesia:create_table(?users_table,
                                           [{attributes, record_info(fields, erlchat_user)},
                                            %{index, [#erlchat_user.id]},
                                            {disc_only_copies, Nodes},
                                            {type, set}]),

        {atomic, ok} = mnesia:create_table(?topics_table,
                                           [{attributes, record_info(fields, erlchat_topic)},
                                            %{index, [#erlchat_topic.id]},
                                            {disc_only_copies, Nodes},
                                            {type, set}]),

        {atomic, ok} = mnesia:create_table(?messages_table,
                                           [{attributes, record_info(fields, erlchat_message)},
                                            %{index, [#erlchat_message.id]},
                                            {disc_only_copies, Nodes},
                                            {type, set}]),

        {atomic, ok} = mnesia:create_table(?messages_ack_table,
                                           [{attributes, record_info(fields, erlchat_message_ack)},
                                            %{index, [#erlhat_message_ack.id]},
                                            {disc_copies, Nodes},
                                            {type, set}]),
        ok.

add_topic(Owner, Users, Subject, Text) ->
        {ok, {Topic, Message, MessageAcks}} = new_topic(Owner, Users, Subject, Text),
        mnesia:activity(transaction, fun() -> write_topic(Topic, Message, MessageAcks) end),
        {ok, {Topic, Message, MessageAcks}}.

get_topic(TopicId) ->
        find_single_tr(?topics_table, TopicId).

add_message(Sender, TopicId, Text) ->
        Fun = fun() ->
                case find_single(?topics_table, TopicId) of
                  {ok, Topic} ->
                    case lists:any(fun(UserId) -> UserId =:= Sender end, Topic#erlchat_topic.users) of
                      true ->
                        {ok, {Message, MessageAcks}} = new_topic_message(Sender, Topic, Text),
                        ok = write_message(Message, MessageAcks),
                        {ok, {Message, MessageAcks}};

                      false ->
                        {error, {user_out_of_topic, Sender}}
                    end;

                  {error, not_found} ->
                    {error, {topic_doesnt_exist, TopicId}}
                end
              end,
        mnesia:activity(transaction, Fun).

get_message(MessageId) ->
        find_single_tr(?messages_table, MessageId).

get_message_acks(UserId, TopicId) ->
        Match = ets:fun2ms(fun(MessageAck=#erlchat_message_ack { user_id = AckUserId,
                                                                topic_id = AckTopicId})
                           when AckUserId =:= UserId andalso AckTopicId =:= TopicId ->
                            MessageAck
                           end),
        mnesia:activity(transaction, fun() -> mnesia:select(?messages_ack_table, Match) end).

write_topic(Topic=#erlchat_topic{}, Message=#erlchat_message{}, MessageAcks) ->
        mnesia:write(Topic),
        ok = write_message(Message, MessageAcks),
        ok.

write_message(Message=#erlchat_message{}, MessageAcks) ->
        mnesia:write(Message),
        lists:foreach(fun(MessageAck) -> mnesia:write(MessageAck) end, MessageAcks),
        ok.

new_topic(Owner, Users, Subject, Text) ->
        Topic = #erlchat_topic{ id = erlchat_utils:generate_uuid(),
                                users = Users,
                                owner = Owner,
                                subject = Subject },
        {ok, {Message, MessageAcks}} = new_topic_message(Owner, Topic, Text),
        {ok, {Topic, Message, MessageAcks}}.

new_topic_message(Sender, #erlchat_topic{ id = TopicId, users = Users }, Text) ->
        MessageId = erlchat_utils:generate_uuid(),
        Message = #erlchat_message { id = MessageId,
                                     sender = Sender,
                                     topic_id = TopicId,
                                     text = Text },
        MessageAcks = generate_message_acks(Users, MessageId, TopicId),
        {ok, {Message, MessageAcks}}.

generate_message_acks(Users, MessageId, TopicId) ->
        MapFun = fun(UserId) -> #erlchat_message_ack { id = erlchat_utils:generate_uuid(),
                                                       message_id = MessageId,
                                                       topic_id = TopicId,
                                                       user_id = UserId }
                 end,
        lists:map(MapFun, Users).

delete_message_ack(MessageAckId) ->
        mnesia:activity(transaction, fun() -> mnesia:delete({?messages_ack_table, MessageAckId}) end).

find_single_tr(Table, Id) ->
        mnesia:activity(transaction, fun() -> find_single(Table, Id) end).

find_single(Table, Id) ->
        case mnesia:read({Table, Id}) of
          [Record] ->
            {ok, Record};
          [] ->
            {error, not_found}
        end.