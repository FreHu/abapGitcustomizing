CLASS lcl_event_handler DEFINITION DEFERRED.
CLASS zcl_agc_ui DEFINITION LOCAL FRIENDS lcl_event_handler.
CLASS lcl_event_handler DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS on_user_command FOR EVENT added_function OF cl_salv_events
      IMPORTING e_salv_function.

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

      WHEN 'PULL'.

        lo_repository_action = zcl_agc_repository_action=>get_instance( ).

        lo_repository_action->pull( ).

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

    ENDCASE.

  ENDMETHOD.

ENDCLASS.
