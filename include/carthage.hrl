-ifndef(__CARTHAGE_HRL__).
-define(__CARTHAGE_HRL__, true).

-record(nwk_req, {
        sock,
        data,

        on_send
       }).

-endif.
