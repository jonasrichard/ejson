-module(ejson_ext2).

-json_include(ejson_ext3).

%% Will see the group record because of json_include
-json({user, {record, group, [{type, group}]}}).
