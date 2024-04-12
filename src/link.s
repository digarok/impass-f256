        dsk impasse.pgz
;-------------- Build PGZ --------------
; http://wiki.f256foenix.com/index.php?title=File_Formats#PGZ

        mx  %11
        org $0

        db  'Z'                  ; PGZ header upper case Z means 24 bit size/length fields

;-------------- PGZ Segment 0
        org $0
        adr main_code_start     ; Address to load into memory
        adr main_code_end-main_code_start ; Length of data to load into their

        org $200
main_code_start
        put main.s
        put f256lib.s
        put textlib.s
        put sprites_dat.s
        put kernel_api.s
        put ../../f256lib/src/minimon
                                ; put blah.s
                                ; put blah2.s
                                ; etc...
main_code_end

;-------------- PGZ Segment 1
        org $0
        adr sprite_data_start
        adr sprite_data_end-sprite_data_start
        org $020000
sprite_data_start
sprite_data putbin data/sprite.spr ; this is fake
sprite_data_end

;-------------- PGZ EOF Marker / Finalizer
        adr start               ;
        adr 0                   ; 0 length, signifies we are done with segments, jump to address above
