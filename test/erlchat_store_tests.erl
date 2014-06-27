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

-module(erlchat_store_tests).
-author("Dmitry Kataskin").

-include("erlchat.hrl").

-include_lib("eunit/include/eunit.hrl").

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
        {ok, Pid} = erlchat_store:start_link([{type, mnesia}, {data_dir, "./data"}, resync]),
        Pid.

stop(_Pid) ->
        erlchat_store:stop().

%topic_validation_tests(_Pid) ->
%        ?assertMatch({error, invalid_data}, erlchat_store:add_topic(11, [12, 13], <<"subj">>, <<"hey">>)),
%        ?assertMatch({error, invalid_data}, erlchat_store:add_topic(11, 1234, <<"subj">>, <<>>)),
%        ?assertMatch({error, invalid_data}, erlchat_store:add_topic(12, [<<"user1">>], <<"subj">>, <<>>)),
%        ?assertMatch({error, invalid_data}, erlchat_store:add_topic(12, [], <<"subj">>, <<>>)).

add_topic_test(_Pid) ->
        Owner = erlchat_utils:generate_uuid(),
        Users = [erlchat_utils:generate_uuid(), erlchat_utils:generate_uuid()],
        Subject = <<"test">>,
        Text = <<"hey there">>,
        {created, {Topic, Message, MessageAcks}} = erlchat_store:add_topic(Owner, Users, Subject, Text),
        TopicId = Topic#erlchat_topic.id,
        ?assertMatch(Owner, Topic#erlchat_topic.owner),
        ?assertMatch([Owner | Users], Topic#erlchat_topic.users),
        ?assertMatch(Subject, Topic#erlchat_topic.subject),
        ?assertNotMatch(<<>>, TopicId),

        ?assertMatch(Owner, Message#erlchat_message.sender),
        ?assertMatch(Text, Message#erlchat_message.text),
        ?assertMatch(TopicId, Message#erlchat_message.topic_id),
        ?assertNotMatch(<<>>, Message#erlchat_message.id),

        ?assertEqual(lists:flatlength(Topic#erlchat_topic.users), lists:flatlength(MessageAcks)).

get_topic_test(_Pid) ->
        Owner = erlchat_utils:generate_uuid(),
        Users = [erlchat_utils:generate_uuid(), erlchat_utils:generate_uuid()],
        Subject = <<"test">>,
        {created, {Topic, _, _}} = erlchat_store:add_topic(Owner, Users, Subject, <<"hey there">>),
        {ok, Topic1} = erlchat_store:get_topic(Topic#erlchat_topic.id),
        ?assertMatch(Topic, Topic1).

add_message_test(_Pid) ->
        {ok, {_Owner, Users, TopicId}} = create_test_topic(),
        [User1 | _] = Users,
        Text = <<"hey there">>,
        {created, {Message, MessageAcks}} = erlchat_store:add_message(User1, TopicId, Text),
        MessageId = Message#erlchat_message.id,
        ?assertMatch(#erlchat_message { sender = User1,
                                        topic_id = TopicId,
                                        text = Text }, Message),

        Length = lists:flatlength(MessageAcks),
        ?assertMatch(Length, lists:flatlength(Users)),

        lists:foreach(fun(MessageAck) ->
                        ?assertNotMatch(<<>>, MessageAck#erlchat_message_ack.id),
                        ?assertMatch(MessageId, MessageAck#erlchat_message_ack.message_id),
                        ?assertMatch(TopicId, MessageAck#erlchat_message_ack.topic_id),
                        ?assert(lists:any(fun(UserId) ->
                                            MessageAck#erlchat_message_ack.user_id =:= UserId
                                          end, Users))
                      end, MessageAcks).

get_message_ack_test(_Pid) ->
        {ok, {_Owner, [User1 | _], TopicId}} = create_test_topic(),
        {created, {_Message, MessageAcks}} = erlchat_store:add_message(User1, TopicId, <<"hey there">>),
        [MessageAck|_T] = MessageAcks,
        {ok, Retrieved} = erlchat_store:get_message_ack(MessageAck#erlchat_message_ack.id),
        ?assertMatch(MessageAck, Retrieved).

delete_message_ack_test(_Pid) ->
        {ok, {_Owner, [User1 | _], TopicId}} = create_test_topic(),
        {created, {_Message, MessageAcks}} = erlchat_store:add_message(User1, TopicId, <<"hey there">>),
        [MessageAck|_T] = MessageAcks,
        Response = erlchat_store:delete_message_ack(MessageAck#erlchat_message_ack.id),
        ?assertMatch({ok, deleted}, Response),
        Response1 = erlchat_store:get_message_ack(MessageAck#erlchat_message_ack.id),
        ?assertMatch({error, not_found}, Response1).

get_message_acks_test(_Pid) ->
        {ok, {_Owner, [User1 | _], TopicId}} = create_test_topic(),
        {created, _} = erlchat_store:add_message(User1, TopicId, <<"hey there">>),
        Response = erlchat_store:get_message_acks(User1, TopicId),
        ?assertMatch({ok, [_, _]}, Response).

get_message_test(_Pid) ->
        {ok, {_Owner, [User1 | _], TopicId}} = create_test_topic(),
        {created, {Message, _}} = erlchat_store:add_message(User1, TopicId, <<"hey there">>),
        {ok, Message1} = erlchat_store:get_message(Message#erlchat_message.id),
        ?assertMatch(Message, Message1).

create_test_topic() ->
        Owner = erlchat_utils:generate_uuid(),
        User1 = erlchat_utils:generate_uuid(),
        User2 = erlchat_utils:generate_uuid(),
        {created, {Topic, _, _}} = erlchat_store:add_topic(Owner, [User1, User2], <<"subj">>, <<"hey there">>),
        TopicId = Topic#erlchat_topic.id,
        Users = Topic#erlchat_topic.users,
        {ok, {Owner, Users, TopicId}}.