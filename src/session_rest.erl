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

-module(session_rest).
-author("Dmitry Kataskin").

-include("erlchat.hrl").

-record(session_rest, { auth_token = undefined }).

-define(session_id_param, session_id).

-define(input_not_json, <<"Input wasn't in a valid application/json format.">>).
-define(input_not_session, <<"Input wasn't a valid session object.">>).
-define(session_id_required, <<"Session id required to fullfill the request.">>).
-define(www_authenticate, <<"realm=\"erlchat\"">>).

%% rest handler callbacks
-export([init/3, allowed_methods/2, content_types_accepted/2, content_types_provided/2, delete_resource/2,
         is_authorized/2, rest_init/2]).

%% custom callbacks
-export([init_session/2, get_session/2]).

init(_Transport, Req, Opts) ->
        {upgrade, protocol, cowboy_rest, Req, Opts}.

rest_init(Req, Opts) ->
        case lists:keyfind(auth_token, 1, Opts) of
          false ->
            {ok, Req, no_state};
          {auth_token, AuthToken} ->
            {ok, Req, #session_rest { auth_token = AuthToken }}
        end.

is_authorized(Req, no_state) ->
        {true, Req, no_state};

is_authorized(Req, State=#session_rest { auth_token = AuthToken }) ->
        {ok, AuthHeader, Req1} = cowboy_req:parse_header(<<"authorization">>, Req),
        case AuthHeader of
          {Auth, <<>>} ->
            case Auth =:= AuthToken of
              true ->
                {true, Req1, State};
              false ->
                {{false, ?www_authenticate}, Req1, State}
            end;
          _ ->
            {{false, ?www_authenticate}, Req1, State}
        end.

allowed_methods(Req, State) ->
        {[<<"GET">>, <<"POST">>, <<"DELETE">>], Req, State}.

content_types_accepted(Req, State) ->
        {[{<<"application/json">>, init_session}], Req, State}.

content_types_provided(Req, State) ->
        {[{<<"application/json">>, get_session}], Req, State}.

delete_resource(Req, State) ->
        exec_against_session(Req, State, fun delete_session/3).

init_session(Req, State) ->
        {ok, Body, Req1} = cowboy_req:body_qs(Req),
        case parse_session(Body, Req1, State) of
          {ok, UserId} ->
            case cowboy_req:method(Req1) of
              {<<"POST">>, Req2} ->
                {initiated, Session} = erlchat_sessions:init_session(UserId),
                Req3 = cowboy_req:set_resp_body(session_to_json(Session), Req2),
                {true, Req3, State};

              {_, Req2} ->
                {true, Req2, State}
            end;

          {error, Response} ->
            Response
        end.

get_session(Req, State) ->
        exec_against_session(Req, State, fun get_session/3).

get_session(SessionId, Req, State) ->
        case erlchat_sessions:get_session(SessionId) of
          {ok, Session} ->
            {session_to_json(Session), Req, State};

          {error, not_found} ->
            {ok, Req2} = cowboy_req:reply(404, [], [], Req),
            {ok, Req2, State}
        end.

delete_session(SessionId, Req, State) ->
        {ok, terminated} = erlchat_sessions:terminate_session(SessionId),
        {true, Req, State}.

parse_session([], Req, State) ->
        {error, error_response(bad_request, ?input_not_json, Req, State)};

parse_session([{Body, true}], Req, State) ->
        case jsx:is_json(Body) of
          true ->
            Session = jsx:decode(Body),
            case Session of
              [{<<"user_id">>, UserId}] ->
                {ok, UserId};

              _ ->
                {error, error_response(bad_request, ?input_not_session, Req, State)}
            end;

          false ->
            {error, error_response(bad_request, ?input_not_json, Req, State)}
        end;

parse_session(_, Req, State) ->
        {error, error_response(bad_request, ?input_not_json, Req, State)}.

exec_against_session(Req, State, Fun) ->
        case cowboy_req:binding(?session_id_param, Req) of
          {undefined, Req1} ->
            error_response(bad_request, ?session_id_required, Req1, State);

          {SessionId, Req1} ->
            Fun(SessionId, Req1, State)
        end.

session_to_json(#erlchat_session { id = SessionId, user_id = UserId }) ->
        jsx:encode([{status, ok}, {user_id, UserId}, {session_id, SessionId}]).

error_response(bad_request, Message, Req, State) ->
        Json = jsx:encode([{error, [{reason, Message}]}]),
        {ok, Req1} = cowboy_req:reply(400, [], Json, Req),
        {ok, Req1, State}.