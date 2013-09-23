-ifndef(__CARTHAGE_HRL__).
-define(__CARTHAGE_HRL__, true).

-record(nwk_req, {
        sock,
        src_proc,
        reply_to,

        data,

        on_send,
        on_reply
       }).

-endif.
