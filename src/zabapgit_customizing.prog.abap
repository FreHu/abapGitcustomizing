*&---------------------------------------------------------------------*
*& Report zabapgit_customizing
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
  REPORT zabapgit_customizing.

  TYPES: BEGIN OF gty_repository_display,
           key     TYPE char12,
           name    TYPE string,
           url     TYPE string,
           package TYPE devclass,
         END OF gty_repository_display.

  DATA: go_repository_online TYPE REF TO zcl_abapgit_repo_online.

  DATA: gt_repository_display TYPE STANDARD TABLE OF gty_repository_display.

  CLASS lcl_event_handler DEFINITION.

    PUBLIC SECTION.

      METHODS on_double_click FOR EVENT double_click OF cl_salv_events_table
        IMPORTING
          row column.

      METHODS on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function.

  ENDCLASS.

  CLASS lcl_abapgit_customizing DEFINITION.

    PUBLIC SECTION.

      METHODS constructor
        IMPORTING
          iv_key TYPE char12.

      METHODS display.

    PRIVATE SECTION.

      TYPES: BEGIN OF ty_customizing,
               objecttype_description TYPE ddtext,
               objecttype             TYPE trobjtype,
               objeectname            TYPE trobj_name,
             END OF ty_customizing.

      DATA mo_repository TYPE REF TO zcl_abapgit_repo.
      DATA mo_customizing_output TYPE REF TO cl_salv_table.

      DATA mt_abapgit_customizing TYPE STANDARD TABLE OF ty_customizing.

  ENDCLASS.

  CLASS lcl_event_handler IMPLEMENTATION.

    METHOD on_double_click.

      READ TABLE gt_repository_display[] ASSIGNING FIELD-SYMBOL(<ls_repository_display>) INDEX row.

      DATA(lo_abapgit_customizing) = NEW lcl_abapgit_customizing( <ls_repository_display>-key ).

      lo_abapgit_customizing->display( ).

    ENDMETHOD.

    METHOD on_user_command.

      DATA: ls_selected_request TYPE trwbo_request_header.

      CASE e_salv_function.

        WHEN '&TRA'.

          CALL FUNCTION 'TR_F4_REQUESTS'
            EXPORTING
              iv_trfunctions          = 'W'
              iv_via_selection_screen = abap_false
            IMPORTING
              es_selected_request     = ls_selected_request.

        WHEN OTHERS.
      ENDCASE.

    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_abapgit_customizing IMPLEMENTATION.

    METHOD constructor.

      DATA(lo_repository_service) = zcl_abapgit_repo_srv=>get_instance( ).

      mo_repository = lo_repository_service->get( iv_key ).

    ENDMETHOD.

    METHOD display.

      DATA(lt_status) = zcl_abapgit_file_status=>status( io_repo = mo_repository ).


      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = mo_customizing_output
        CHANGING
          t_table      = mt_abapgit_customizing[]
      ).

      mo_customizing_output->set_screen_status(
        EXPORTING
          report        = sy-repid
          pfstatus      = 'STANDARD'
          set_functions = mo_customizing_output->c_functions_all
      ).

      DATA(lo_events) = mo_customizing_output->get_event( ).

      DATA(lo_event_handler) = NEW lcl_event_handler( ).

      SET HANDLER lo_event_handler->on_user_command FOR lo_events.

      mo_customizing_output->display( ).

    ENDMETHOD.

  ENDCLASS.

  START-OF-SELECTION.

    DATA(go_repository_service) = zcl_abapgit_repo_srv=>get_instance( ).

    DATA(gt_repository_list) = go_repository_service->list( ).

    LOOP AT gt_repository_list[] ASSIGNING FIELD-SYMBOL(<ls_repository_list>).

      go_repository_online ?= <ls_repository_list>.

      APPEND VALUE #( key     = <ls_repository_list>->get_key( )
                      name    = <ls_repository_list>->get_name( )
                      url     = go_repository_online->get_url( )
                      package = <ls_repository_list>->get_package( )
                    ) TO gt_repository_display[].

    ENDLOOP.

    cl_salv_table=>factory(
      IMPORTING
        r_salv_table = DATA(go_repository_output)                          " Basis Class Simple ALV Tables
      CHANGING
        t_table      = gt_repository_display[]
    ).

    DATA(go_columns) = go_repository_output->get_columns( ).
    go_columns->set_optimize( ).

    DATA(go_column) = go_columns->get_column( 'NAME' ).
    go_column->set_long_text( 'Display Name' ).

    go_column = go_columns->get_column( 'URL' ).
    go_column->set_long_text( 'Repository' ).

    go_column = go_columns->get_column( 'PACKAGE' ).
    go_column->set_long_text( 'Package' ).

    go_column = go_columns->get_column( 'KEY' ).
    go_column->set_technical( ).

    DATA(go_functions) = go_repository_output->get_functions( ).
    go_functions->set_default( ).

    DATA(lo_events) = go_repository_output->get_event( ).

    DATA(go_event_handler) = NEW lcl_event_handler( ).

    SET HANDLER go_event_handler->on_double_click FOR lo_events.

    go_repository_output->display( ).
