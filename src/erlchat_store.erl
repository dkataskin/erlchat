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
-export([start/1, stop/0]).
-export([get_user/1]).
-export([start_new_topic/2, get_topic/1]).

start(Args) when is_list(Args) ->
        case proplists:get_value(?store_type_key, Args) of
          ?mnesia_backend -> gen_server:start_link({local, ?store_server}, erlchat_mnesia, Args, []);
          _ -> {error, unknown_store_backend}
        end.

stop() ->
        ok.

start_new_topic(Users, Subject) ->
        Topic = #erlchat_topic{ users = Users, subject = Subject},
        case is_valid(Topic) of
          true ->
            gen_server:call(?store_server, {start_new_topic, Topic});
          false ->
            {error, invalid_data}
        end.

get_topic(TopicId) ->
        gen_server:call(?store_server, {get_topic, TopicId}).

get_user(UserId) ->
        gen_server:call(?store_server, {get_user, UserId}).

is_valid(Topic=#erlchat_topic{}) ->
        (lists:flatlength(Topic#erlchat_topic.users) > 0) and
        erlang:is_list(Topic#erlchat_topic.users) and
        lists:foreach(fun(UserId) -> is_binary(UserId) and (UserId =/= <<>>) end,
                      Topic#erlchat_topic.users).
