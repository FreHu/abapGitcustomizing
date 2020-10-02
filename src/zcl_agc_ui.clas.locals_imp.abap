CLASS lcl_event_handler DEFINITION DEFERRED.
CLASS zcl_agc_ui DEFINITION LOCAL FRIENDS lcl_event_handler.
CLASS lcl_event_handler DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS on_user_command FOR EVENT added_function OF cl_salv_events
      IMPORTING e_salv_function.

    METHODS on_link_click FOR EVENT link_click OF cl_salv_events_table
      IMPORTING row column.

ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_user_command.

    CASE e_salv_function.

      WHEN '&TRA'.

        DATA(lo_request_handler) = zcl_agc_request_handler=>get_instance( ).

        lo_request_handler->select( ).

        DATA(lt_customizing_ui) = lo_request_handler->get_data( ).

        LOOP AT lt_customizing_ui[] ASSIGNING FIELD-SYMBOL(<ls_customizing_ui_new>).

          READ TABLE zcl_agc_ui=>mo_abapgit_customizing_ui->mt_customizing_ui[] ASSIGNING FIELD-SYMBOL(<ls_customizing_ui>)
                                                                                WITH KEY objecttype = <ls_customizing_ui_new>-objecttype
                                                                                         objectname = <ls_customizing_ui_new>-objectname.
          IF sy-subrc = 0.

            <ls_customizing_ui>-container_local = <ls_customizing_ui_new>-container_local.

            IF <ls_customizing_ui>-container_local->if_bcfg_config_container~equals( <ls_customizing_ui>-container_remote ) = abap_false.

              DATA(lt_color) = VALUE lvc_t_scol( ( color-col = 6 color-int = 1 color-inv = 0 ) ).

            ENDIF.

            <ls_customizing_ui>-color[] = lt_color[].

          ELSE.

            APPEND <ls_customizing_ui_new> TO zcl_agc_ui=>mo_abapgit_customizing_ui->mt_customizing_ui[].

          ENDIF.

        ENDLOOP.

        zcl_agc_ui=>mo_abapgit_customizing_ui->mo_customizing_output->refresh( ).

      WHEN 'PUSH'.

        DATA(lo_repository_action) = zcl_agc_repository_action=>get_instance( ).

        lo_repository_action->push( ).

        lt_customizing_ui[] = zcl_agc_ui=>get_instance( )->get_selected_customizing( ).

        LOOP AT lt_customizing_ui[] ASSIGNING <ls_customizing_ui_new>.

          READ TABLE zcl_agc_ui=>mo_abapgit_customizing_ui->mt_customizing_ui[] ASSIGNING <ls_customizing_ui>
                                                                                WITH KEY objecttype = <ls_customizing_ui_new>-objecttype
                                                                                         objectname = <ls_customizing_ui_new>-objectname.
          CHECK sy-subrc = 0.

          <ls_customizing_ui>-container_remote = <ls_customizing_ui_new>-container_local.
          CLEAR <ls_customizing_ui>-color[].

        ENDLOOP.

        zcl_agc_ui=>mo_abapgit_customizing_ui->mo_customizing_output->refresh( ).

      WHEN 'PULL'.

        lo_repository_action = zcl_agc_repository_action=>get_instance( ).

        DATA(lt_imported_objects) = lo_repository_action->pull( ).

        LOOP AT lt_imported_objects[] ASSIGNING <ls_customizing_ui_new>.

          READ TABLE zcl_agc_ui=>mo_abapgit_customizing_ui->mt_customizing_ui[] ASSIGNING <ls_customizing_ui>
                                                                                WITH KEY objecttype = <ls_customizing_ui_new>-objecttype
                                                                                         objectname = <ls_customizing_ui_new>-objectname.
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

          <ls_customizing_ui>-celltype[] = VALUE #( ( columnname = 'IMPORT_LOG' value = if_salv_c_cell_type=>hotspot ) ).

        ENDLOOP.

        zcl_agc_ui=>mo_abapgit_customizing_ui->mo_customizing_output->refresh( ).

    ENDCASE.

  ENDMETHOD.

  METHOD on_link_click.

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
                                                                          INDEX row.

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
