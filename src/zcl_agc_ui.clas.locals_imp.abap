CLASS lcl_event_handler DEFINITION DEFERRED.
CLASS zcl_agc_ui DEFINITION LOCAL FRIENDS lcl_event_handler.
CLASS lcl_event_handler DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS on_user_command FOR EVENT added_function OF cl_salv_events
      IMPORTING e_salv_function.

    METHODS on_link_click FOR EVENT link_click OF cl_salv_events_table
      IMPORTING row column.
  PRIVATE SECTION.

    METHODS display_import_logs
      IMPORTING
        iv_row TYPE i.

ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_user_command.

    CASE e_salv_function.

      WHEN '&TRA'.

        DATA(lo_request_handler) = zcl_agc_request_handler=>get_instance( ).

        lo_request_handler->select( ).

        DATA(lt_customizing_ui) = lo_request_handler->get_data( ).

        LOOP AT lt_customizing_ui[] ASSIGNING FIELD-SYMBOL(<ls_customizing_ui_new>).

          READ TABLE zcl_agc_ui=>mo_abapgit_customizing_ui->mt_customizing_ui[]
            WITH KEY objecttype = <ls_customizing_ui_new>-objecttype
                     objectname = <ls_customizing_ui_new>-objectname
            ASSIGNING FIELD-SYMBOL(<ls_customizing_ui>).

          IF sy-subrc = 0.

            <ls_customizing_ui>-container_local = <ls_customizing_ui_new>-container_local.

            IF <ls_customizing_ui>-container_local->if_bcfg_config_container~equals(
              <ls_customizing_ui>-container_remote ) = abap_false.

              DATA(lt_color) = VALUE lvc_t_scol( ( color-col = 6 color-int = 1 color-inv = 0 ) ).

            ENDIF.

            <ls_customizing_ui>-color[] = lt_color[].

          ELSE.

            APPEND <ls_customizing_ui_new> TO zcl_agc_ui=>mo_abapgit_customizing_ui->mt_customizing_ui[].

          ENDIF.

        ENDLOOP.

        zcl_agc_ui=>mo_abapgit_customizing_ui->mo_customizing_output->refresh( ).

      WHEN 'PUSH'.

        lt_customizing_ui[] = zcl_agc_ui=>get_instance( )->get_selected_customizing( ).

        IF lt_customizing_ui[] IS INITIAL.

          MESSAGE s398(00) WITH 'Select a customizing to stage/pull'
          DISPLAY LIKE 'E'.

          RETURN.

        ENDIF.

        DATA(lo_repository_action) = zcl_agc_repository_action=>get_instance( ).

        lo_repository_action->push( ).

        LOOP AT lt_customizing_ui[] ASSIGNING <ls_customizing_ui_new>.

          READ TABLE zcl_agc_ui=>mo_abapgit_customizing_ui->mt_customizing_ui[]
            WITH KEY objecttype = <ls_customizing_ui_new>-objecttype
                     objectname = <ls_customizing_ui_new>-objectname
            ASSIGNING <ls_customizing_ui>.

          CHECK sy-subrc = 0.

          <ls_customizing_ui>-container_remote = <ls_customizing_ui_new>-container_local.
          CLEAR <ls_customizing_ui>-color[].

        ENDLOOP.

        zcl_agc_ui=>mo_abapgit_customizing_ui->mo_customizing_output->refresh( ).

      WHEN 'PULL'.

        IF zcl_agc_ui=>get_instance( )->get_selected_customizing( ) IS INITIAL.

          MESSAGE s398(00) WITH 'Select a customizing to stage/pull'
          DISPLAY LIKE 'E'.

          RETURN.

        ENDIF.

        TRY.
            lo_repository_action = zcl_agc_repository_action=>get_instance( ).

            DATA(lt_imported_objects) = lo_repository_action->pull( ).

            LOOP AT lt_imported_objects[] ASSIGNING <ls_customizing_ui_new>.

              READ TABLE zcl_agc_ui=>mo_abapgit_customizing_ui->mt_customizing_ui[]
                WITH KEY objecttype = <ls_customizing_ui_new>-objecttype
                         objectname = <ls_customizing_ui_new>-objectname
                ASSIGNING <ls_customizing_ui>.

              CHECK sy-subrc = 0.

              <ls_customizing_ui> = <ls_customizing_ui_new>.

              CASE <ls_customizing_ui>-container_result->get_status( ).

                WHEN cl_bcfg_enum_operation_status=>abort
                OR cl_bcfg_enum_operation_status=>error.
                  <ls_customizing_ui>-import_log = icon_led_red.

                WHEN cl_bcfg_enum_operation_status=>warning.
                  <ls_customizing_ui>-import_log = icon_led_yellow.

                WHEN OTHERS.
                  <ls_customizing_ui>-import_log = icon_led_green.

              ENDCASE.

              <ls_customizing_ui>-celltype[] = VALUE #(
                ( columnname = 'IMPORT_LOG' value = if_salv_c_cell_type=>hotspot ) ).

            ENDLOOP.

            zcl_agc_ui=>mo_abapgit_customizing_ui->mo_customizing_output->refresh( ).
          CATCH cx_root INTO DATA(cx).
            MESSAGE cx->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
        ENDTRY.

    ENDCASE.

  ENDMETHOD.

  METHOD on_link_click.

    CASE column.

      WHEN 'IMPORT_LOG'.

        display_import_logs( row ).

      WHEN 'DIFFERENCE'.

        DATA: lo_data_remote         TYPE REF TO data,
              lo_data_remote_deleted TYPE REF TO data,
              lo_data_local          TYPE REF TO data,
              lo_data_deleted        TYPE REF TO data,
              lo_data_inserted       TYPE REF TO data,
              lo_data_updated        TYPE REF TO data.

        DATA: lv_no_change TYPE flag.

        FIELD-SYMBOLS: <lt_data_remote>         TYPE STANDARD TABLE,
                       <lt_data_remote_deleted> TYPE STANDARD TABLE,
                       <lt_data_local>          TYPE STANDARD TABLE,
                       <lt_data_deleted>        TYPE STANDARD TABLE,
                       <lt_data_inserted>       TYPE STANDARD TABLE,
                       <lt_data_updated>        TYPE STANDARD TABLE.

        READ TABLE zcl_agc_ui=>mo_abapgit_customizing_ui->mt_customizing_ui[] ASSIGNING FIELD-SYMBOL(<ls_customizing_ui>)
                                                                              INDEX row.

        CREATE DATA lo_data_remote TYPE STANDARD TABLE OF (<ls_customizing_ui>-objectname).
        ASSIGN lo_data_remote->* TO <lt_data_remote>.

        <ls_customizing_ui>-container_remote->if_bcfg_config_container~get_lines(
          EXPORTING
            iv_langu = sy-langu
          IMPORTING
            et_lines = <lt_data_remote>[]
        ).

        CREATE DATA lo_data_remote_deleted TYPE STANDARD TABLE OF (<ls_customizing_ui>-objectname).
        ASSIGN lo_data_remote_deleted->* TO <lt_data_remote_deleted>.

        <ls_customizing_ui>-container_remote->if_bcfg_config_container~get_deletions(
          IMPORTING
            et_lines = <lt_data_remote_deleted>[]
        ).

*        APPEND LINES OF <lt_data_remote_deleted>[] TO <lt_data_remote>[].

        CREATE DATA lo_data_local TYPE STANDARD TABLE OF (<ls_customizing_ui>-objectname).
        ASSIGN lo_data_local->* TO <lt_data_local>.

        <ls_customizing_ui>-container_local->if_bcfg_config_container~get_lines(
          EXPORTING
            iv_langu = sy-langu
          IMPORTING
            et_lines = <lt_data_local>[]
        ).

        DATA(lo_config_object) = <ls_customizing_ui>-container_remote->if_bcfg_has_data_manager~get_data_manager( )->get_config_object( CONV #( <ls_customizing_ui>-objectname ) ).

    ENDCASE.

  ENDMETHOD.


  METHOD display_import_logs.

*   Initialize message
    CALL FUNCTION 'MESSAGES_INITIALIZE'
      EXCEPTIONS
        log_not_active       = 1                " Log not active -> no reset
        wrong_identification = 2                " Identification not correct (-> long text)
        OTHERS               = 3.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    READ TABLE zcl_agc_ui=>mo_abapgit_customizing_ui->mt_customizing_ui[] ASSIGNING FIELD-SYMBOL(<ls_customizing_ui>)
                                                                          INDEX iv_row.

    LOOP AT <ls_customizing_ui>-container_result->get_log_messages( ) ASSIGNING FIELD-SYMBOL(<ls_return>).

      CALL FUNCTION 'MESSAGE_STORE'
        EXPORTING
          arbgb                  = <ls_return>-id         " Message ID
          msgty                  = <ls_return>-type       " Type of message (I, S, W, E, A)
          msgv1                  = <ls_return>-message_v1 " First variable parameter of message
          msgv2                  = <ls_return>-message_v2 " Second variable parameter of message
          msgv3                  = <ls_return>-message_v3 " Third variable parameter of message
          msgv4                  = <ls_return>-message_v4 " Fourth variable parameter of message
          txtnr                  = <ls_return>-number     " Message Number
        EXCEPTIONS
          message_type_not_valid = 1                      " Type of message not I, S, W, E or A
          not_active             = 2                      " Collection of messages not activated
          OTHERS                 = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    ENDLOOP.

    CALL FUNCTION 'MESSAGES_SHOW'
      EXPORTING
        show_linno         = abap_false
        i_use_grid         = abap_true
      EXCEPTIONS
        inconsistent_range = 1                " LINE_TO is shorter than LINE_FROM
        no_messages        = 2                " No messages in required interval
        OTHERS             = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
