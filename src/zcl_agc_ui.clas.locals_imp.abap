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

        APPEND LINES OF lo_request_handler->get_data( ) TO zcl_agc_ui=>mo_abapgit_customizing_ui->mt_customizing_ui[].

        zcl_agc_ui=>mo_abapgit_customizing_ui->mo_customizing_output->refresh( ).

      WHEN 'PUSH'.

        DATA(lo_repository_action) = zcl_agc_repository_action=>get_instance( ).

        lo_repository_action->push( ).

      WHEN 'PULL'.

        lo_repository_action = zcl_agc_repository_action=>get_instance( ).

        lo_repository_action->pull( ).

    ENDCASE.

  ENDMETHOD.

ENDCLASS.
