{application,erlchat,
             [{description,"A chat server in Erlang"},
              {vsn,"1"},
              {modules,[erlchat_app]},
              {registered,[]},
              {applications,[kernel,stdlib,crypto,ssl,bullet]},
              {mod,{erlchat_app,[]}},
              {env,[]}]}.
