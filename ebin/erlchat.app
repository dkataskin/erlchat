{application,erlchat,
             [{description,"A chat server in Erlang"},
              {vsn,"1"},
              {modules,[erlchat,erlchat_app,erlchat_sup,stream_handler,
                        toppage_handler]},
              {registered,[]},
              {applications,[kernel,stdlib,cowboy]},
              {mod,{erlchat_app,[]}},
              {env,[]}]}.
