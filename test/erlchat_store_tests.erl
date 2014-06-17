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
              [fun topic_validation_tests/1,
               fun add_topic_test/1,
               fun get_topic_test/1,
               fun add_message_test/1,
               fun get_message_test/1,
               fun add_message_ack_test/1]
            }
          end}.

start() ->
        {ok, Pid} = erlchat_store:start_link([{type, mnesia}, {data_dir, "./data"}]),
        Pid.

stop(_Pid) ->
        erlchat_store:stop().

topic_validation_tests(_Pid) ->
         ?assertMatch({error, invalid_data}, erlchat_store:add_topic([12, 13], <<"subj">>)),
         ?assertMatch({error, invalid_data}, erlchat_store:add_topic(1234, <<"subj">>)),
         ?assertMatch({error, invalid_data}, erlchat_store:add_topic([<<"user1">>], <<"subj">>)),
         ?assertMatch({error, invalid_data}, erlchat_store:add_topic([], <<"subj">>)).

add_topic_test(_Pid) ->
        Users = [<<"user1">>, <<"user2">>],
        Subject = <<"test">>,
        {created, Topic} = erlchat_store:add_topic(Users, Subject),
        ?assertMatch(Users, Topic#erlchat_topic.users),
        ?assertMatch(Subject, Topic#erlchat_topic.subject),
        ?assertNotMatch(<<>>, Topic#erlchat_topic.id).

get_topic_test(_Pid) ->
        Users = [<<"user1">>, <<"user2">>],
        Subject = <<"test">>,
        {created, Topic} = erlchat_store:add_topic(Users, Subject),
        {ok, Topic1} = erlchat_store:get_topic(Topic#erlchat_topic.id),
        ?assertMatch(Topic, Topic1).

add_message_test(_Pid) ->
        Sender = <<"user1">>,
        TopicId = <<"topic1">>,
        Text = <<"hey there">>,
        {created, Message} = erlchat_store:add_message(Sender, TopicId, Text),
        ?assertMatch(#erlchat_message { sender = Sender,
                                        topic_id = TopicId,
                                        text = Text }, Message).

get_message_test(_Pid) ->
        {created, Message} = erlchat_store:add_message(<<"user1">>, <<"topic1">>, <<"hey there">>),
        {ok, Message1} = erlchat_store:get_message(Message#erlchat_message.id),
        ?assertMatch(Message, Message1).

add_message_ack_test(_Pid) ->
        Sender = <<"user1">>,
        TopicId = <<"topic1">>,
        MessageId = <<"message1">>,
        {created, MessageAck} = erlchat_store:add_message_ack(Sender, MessageId, TopicId),
        ?assertMatch(#erlchat_message_ack { message_id = MessageId,
                                            topic_id = TopicId,
                                            user_id = Sender}, MessageAck).