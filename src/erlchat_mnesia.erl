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

init(_Args) ->
          application:set_env(mnesia, dir, filename:join([code:priv_dir(erlchat), ?data_dir])),
          application:start(mnesia),
          Nodes = erlang:node(),
          create_schema(Nodes),
          {ok, no_state}.

handle_call({get_user, Id}, _From, State) ->
                User = #erlchat_user { id = Id },
                {reply, User, State}.

handle_cast(_Request, State) ->
                {noreply, State}.

handle_info(_Info, State) ->
                {noreply, State}.

terminate(_Reason, _State) ->
                ok.

code_change(_OldVsn, State, _Extra) ->
                {ok, State}.

% Schema
create_schema(Nodes) ->
                {atomic, ok} = mnesia:create_table(erlchat_user, [{attributes, record_info(fields, erlchat_user)},
                                                                  {index, #erlchat_user.id},
                                                                  {disc_only_copies, Nodes},
                                                                  {type, set}]),

                {atomic, ok} = mnesia:create_table(erlchat_conversation, [{attributes, record_info(fields, erlchat_conversation)},
                                                                          {index, #erlchat_conversation.id},
                                                                          {disc_only_copies, Nodes},
                                                                          {type, set}]),

                {atomic, ok} = mnesia:create_table(erlchat_message, [{attributes, record_info(fields, erlchat_message)},
                                                                     {index, #erlchat_message.id},
                                                                     {disc_only_copies, Nodes},
                                                                     {type, set}]),
                ok.


