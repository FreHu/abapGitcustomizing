CLASS zcl_agc_repository_ui DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_agc_repository_ui.

    CLASS-METHODS get_instance
      RETURNING VALUE(ro_repository_ui) TYPE REF TO zif_agc_repository_ui.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF ty_repository_display,
             key     TYPE char12,
             name    TYPE string,
             url     TYPE string,
             package TYPE devclass,
           END OF ty_repository_display.

    CLASS-DATA mo_repository_ui TYPE REF TO zcl_agc_repository_ui.

    DATA mt_repositories TYPE STANDARD TABLE OF ty_repository_display.
    DATA mo_repositories_output TYPE REF TO cl_salv_table.

    METHODS create_repository_list
      RAISING
        zcx_agc_repository_ui.

    METHODS display_output
      RAISING
        zcx_agc_repository_ui.

ENDCLASS.



CLASS zcl_agc_repository_ui IMPLEMENTATION.

  METHOD get_instance.

    IF mo_repository_ui IS NOT BOUND.

      mo_repository_ui = NEW #( ).

    ENDIF.

    ro_repository_ui ?= mo_repository_ui.

  ENDMETHOD.

  METHOD zif_agc_repository_ui~display.

*   Create repository list for display
    create_repository_list( ).

*   Display output UI
    display_output( ).

  ENDMETHOD.


  METHOD create_repository_list.

*   Declaration of local object reference
    DATA: lo_repository_online TYPE REF TO zcl_abapgit_repo_online.

*   Get instance
    DATA(lo_repository_service) = zcl_abapgit_repo_srv=>get_instance( ).

    TRY.

*       Get list of repositories found in the system for the user
        DATA(lt_repositories) = lo_repository_service->list( ).

      CATCH zcx_abapgit_exception INTO DATA(lo_abapgit_exception). " Exception

        RAISE EXCEPTION TYPE zcx_agc_repository_ui
          EXPORTING
            previous = lo_abapgit_exception.

    ENDTRY.

    LOOP AT lt_repositories[] ASSIGNING FIELD-SYMBOL(<ls_repository>).

      CHECK <ls_repository>->is_offline( ) = abap_false.

      lo_repository_online ?= <ls_repository>.

      TRY.

          APPEND VALUE #( key     = <ls_repository>->get_key( )
                          name    = <ls_repository>->get_name( )
                          url     = lo_repository_online->get_url( )
                          package = <ls_repository>->get_package( )
                        ) TO mt_repositories[].

        CATCH zcx_abapgit_exception INTO lo_abapgit_exception. " Exception

          RAISE EXCEPTION TYPE zcx_agc_repository_ui
            EXPORTING
              previous = lo_abapgit_exception.

      ENDTRY.

    ENDLOOP.

  ENDMETHOD.


  METHOD display_output.

*   Declaration of local object reference
    DATA: lo_column TYPE REF TO cl_salv_column_table.

    TRY.

*       Create ALV instance
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = mo_repositories_output       " Basis Class Simple ALV Tables
          CHANGING
            t_table      = mt_repositories[] " Repository list
        ).

        DATA(lo_columns) = mo_repositories_output->get_columns( ).
        lo_columns->set_optimize( ).

        lo_column ?= lo_columns->get_column( 'NAME' ).
        lo_column->set_long_text( 'Display Name' ).

        lo_column ?= lo_columns->get_column( 'URL' ).
        lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
        lo_column->set_long_text( 'Repository' ).

        lo_column ?= lo_columns->get_column( 'PACKAGE' ).
        lo_column->set_long_text( 'Package' ).

        lo_column ?= lo_columns->get_column( 'KEY' ).
        lo_column->set_technical( ).

        DATA(lo_functions) = mo_repositories_output->get_functions( ).
        lo_functions->set_default( ).

*       Get events
        DATA(lo_events) = mo_repositories_output->get_event( ).

*       Create event handler instance
        DATA(lo_event_handler) = NEW lcl_event_handler( ).

        SET HANDLER: lo_event_handler->on_link_click   FOR lo_events,
                     lo_event_handler->on_double_click FOR lo_events.

        mo_repositories_output->display( ).

      CATCH cx_salv_msg INTO DATA(lo_slav_message). " ALV: General Error Class with Message

        RAISE EXCEPTION TYPE zcx_agc_repository_ui
          EXPORTING
            previous = lo_slav_message.

      CATCH cx_salv_not_found INTO DATA(lo_salv_not_found).

        RAISE EXCEPTION TYPE zcx_agc_repository_ui
          EXPORTING
            previous = lo_salv_not_found.

    ENDTRY.

  ENDMETHOD.

ENDCLASS.
