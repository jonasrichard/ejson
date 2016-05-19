-module(ejson_ext1).

-json({account, {string, email}, {string, password},
                {record, owner, [{type, user}, {module, ejson_ext2}]}
      }).

