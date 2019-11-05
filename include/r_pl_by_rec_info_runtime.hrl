%%%-------------------------------------------------------------------
%%% @author shayu
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 11月 2019 15:08
%%%-------------------------------------------------------------------
-author("shayu").

-define(R2PL(Record),
    r_pl_by_rec_info_runtime:r2pl(Record, record_info_fields(element(1, Record)))
).
-define(PL2R(PL), r_pl_by_rec_info_runtime:pl2r(PL, ?MODULE)).

-define(RECORD_SIZE(MOD, RecordName), MOD:record_info_size(RecordName)).
-define(RECORD_FIELDS(MOD, RecordName), MOD:record_info_fields(RecordName)).

-define(RECORD_NAME, '__record_name__').
-define(NO_VALUE, undefined).