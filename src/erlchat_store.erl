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


-module(erlchat_store).
-author("Dmitry Kataskin").

-include("erlchat.hrl").

-define(store_type_key, type).
-define(mnesia_backend, mnesia).

%% API
-export([start_link/1, stop/0]).
-export([add_user/2, get_user/1]).
-export([add_topic/2, get_topic/1]).
-export([add_message/3, get_message/1]).
-export([get_message_acks/2]).

start_link(Args) when is_list(Args) ->
        case proplists:get_value(?store_type_key, Args) of
          ?mnesia_backend ->
            gen_server:start_link({local, ?store_server}, erlchat_mnesia, Args, []);
          _ ->
            {error, unknown_store_backend}
        end.

stop() ->
        gen_server:call(?store_server, stop).

add_topic(Users, Subject) ->
        Topic = #erlchat_topic{ users = Users, subject = Subject},
        execute_if_valid(Topic, fun() -> gen_server:call(?store_server, {add_topic, Topic}) end).

get_topic(TopicId) ->
        gen_server:call(?store_server, {get_topic, TopicId}).

add_message(Sender, TopicId, Text) ->
        Message = #erlchat_message { sender = Sender, topic_id = TopicId, text = Text },
        execute_if_valid(Message, fun() -> gen_server:call(?store_server, {add_message, Message}) end).

get_message(MessageId) ->
        gen_server:call(?store_server, {get_message, MessageId}).

%add_message_ack(Sender, MessageId, TopicId) ->
%        MessageAck = #erlchat_message_ack { message_id = MessageId,
%                                            topic_id = TopicId,
%                                            user_id = Sender },
%        execute_if_valid(MessageAck, fun() -> gen_server:call(?store_server, {add_message_ack, MessageAck}) end).

get_message_acks(Sender, TopicId) ->
        gen_server:call(?store_server, {get_message_acks, {Sender, TopicId}}).

add_user(Nickname, Avatar) ->
        User = #erlchat_user{ nickname = Nickname, avatar = Avatar },
        execute_if_valid(User, fun() -> gen_server:call(?store_server, {add_user, User}) end).

get_user(UserId) ->
        gen_server:call(?store_server, {get_user, UserId}).

execute_if_valid(Data, HappyPath) ->
        case is_valid(Data) of
          true ->
            HappyPath();
          false ->
            {error, invalid_data}
        end.

is_valid(Topic=#erlchat_topic{}) ->
        is_list(Topic#erlchat_topic.users) andalso
        (lists:flatlength(Topic#erlchat_topic.users) > 1) andalso
        lists:all(fun is_valid_id/1, Topic#erlchat_topic.users);

is_valid(Message=#erlchat_message{}) ->
        is_valid_id(Message#erlchat_message.sender) andalso
        is_valid_id(Message#erlchat_message.topic_id) andalso
        is_valid_string(Message#erlchat_message.text);

%is_valid(MessageAck=#erlchat_message_ack{}) ->
%        is_valid_id(MessageAck#erlchat_message_ack.message_id) andalso
%        is_valid_id(MessageAck#erlchat_message_ack.topic_id) andalso
%        is_valid_id(MessageAck#erlchat_message_ack.user_id);

is_valid(User=#erlchat_user{}) ->
        is_valid_string(User#erlchat_user.nickname).

is_valid_id(Id) ->
        is_binary(Id) andalso Id =/= <<>>.

is_valid_string(BitString) ->
        is_binary(BitString) andalso BitString =/= <<>>.


