CLASS lcl_event_handler DEFINITION DEFERRED.
CLASS zcl_agc_repository_ui DEFINITION LOCAL FRIENDS lcl_event_handler.
CLASS lcl_event_handler DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS on_link_click FOR EVENT link_click OF cl_salv_events_table
      IMPORTING
        row column.

    METHODS on_double_click FOR EVENT double_click OF cl_salv_events_table
      IMPORTING
        row column.

ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_link_click.

    READ TABLE zcl_agc_repository_ui=>mo_repository_ui->mt_repositories[] ASSIGNING FIELD-SYMBOL(<ls_repository>) INDEX row.

    CALL FUNCTION 'CALL_BROWSER'
      EXPORTING
        url                    = CONV char2000( <ls_repository>-url ) " URL of Browser Call
      EXCEPTIONS
        frontend_not_supported = 1                                    " Frontend Not Supported
        frontend_error         = 2                                    " Error occurred in SAPGUI
        prog_not_found         = 3                                    " Program not found or not in executable form
        no_batch               = 4                                    " Front-End Function Cannot Be Executed in Backgrnd
        unspecified_error      = 5                                    " Unspecified Exception
        OTHERS                 = 6.
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

  METHOD on_double_click.

    READ TABLE zcl_agc_repository_ui=>mo_repository_ui->mt_repositories[] ASSIGNING FIELD-SYMBOL(<ls_repository>) INDEX row.

    DATA(lo_customizing_ui) = zcl_agc_ui=>get_instance( ).

    lo_customizing_ui->display( <ls_repository>-key ).

  ENDMETHOD.

ENDCLASS.
