CLASS zcl_agc_ui DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_agc_ui .

    CLASS-METHODS get_instance
      RETURNING VALUE(ro_customizing_ui) TYPE REF TO zif_agc_ui.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF ty_bcset_metadata,
             scprattr TYPE scprattr,
             scprtext TYPE STANDARD TABLE OF scprtext WITH DEFAULT KEY,
             scprvals TYPE STANDARD TABLE OF scprvals WITH DEFAULT KEY,
             scprvall TYPE STANDARD TABLE OF scprvall WITH DEFAULT KEY,
             scprreca TYPE STANDARD TABLE OF scprreca WITH DEFAULT KEY,
             scprfldv TYPE STANDARD TABLE OF scprfldv WITH DEFAULT KEY,
             subprofs TYPE STANDARD TABLE OF scprpprl WITH DEFAULT KEY,
           END OF ty_bcset_metadata .

    CLASS-DATA mo_abapgit_customizing_ui TYPE REF TO zcl_agc_ui.

    DATA mo_repository TYPE REF TO zcl_abapgit_repo_online.
    DATA mo_customizing_output TYPE REF TO cl_salv_table.
    DATA mt_customizing_ui TYPE zif_agc_ui=>ty_t_customizing_ui.

    METHODS create_container
      IMPORTING
                is_bcset_metadata   TYPE zcl_agc_ui=>ty_bcset_metadata
                it_mappings         TYPE if_bcfg_config_container=>ty_t_mapping_info
      RETURNING VALUE(ro_container) TYPE REF TO cl_bcfg_bcset_config_container
      RAISING
                cx_bcfg_commit_mode_unsupport
                cx_bcfg_incomplete_key
                cx_bcfg_invalid_mapping
                cx_bcfg_langu_missing
                cx_bcfg_langu_not_allowed
                cx_bcfg_langu_superfluous .

    METHODS create_customizing_list
      RAISING
        cx_bcfg_commit_mode_unsupport
        cx_bcfg_incomplete_key
        cx_bcfg_invalid_mapping
        cx_bcfg_langu_missing
        cx_bcfg_langu_not_allowed
        cx_bcfg_langu_superfluous
        cx_bcfg_operation_failed
        zcx_abapgit_exception .

    METHODS display_output
      RAISING
        zcx_agc_ui.

ENDCLASS.



CLASS zcl_agc_ui IMPLEMENTATION.

  METHOD get_instance.

    IF mo_abapgit_customizing_ui IS NOT BOUND.

      mo_abapgit_customizing_ui = NEW #( ).

    ENDIF.

    ro_customizing_ui ?= mo_abapgit_customizing_ui.

  ENDMETHOD.

  METHOD zif_agc_ui~display.

*   Get instance
    DATA(lo_repository_service) = zcl_abapgit_repo_srv=>get_instance( ).

    TRY.

        mo_repository ?= lo_repository_service->get( iv_repository_key ).

      CATCH zcx_abapgit_exception ##NO_HANDLER.

    ENDTRY.

*   Declaration of local object reference
    DATA: lo_column TYPE REF TO cl_salv_column_table.

*   Create customizing content list
    create_customizing_list( ).

    display_output( ).

  ENDMETHOD.


  METHOD create_container.

    DATA(lt_bcset_language_values) = is_bcset_metadata-scprvall[].

    SORT lt_bcset_language_values[] BY langu.

*   Delete duplicate languages
    DELETE ADJACENT DUPLICATES FROM lt_bcset_language_values[]
    COMPARING langu.

    DATA(lt_languages) = VALUE if_bcfg_config_container=>ty_t_languages( FOR ls_bcset_language_value IN lt_bcset_language_values[]
                                                                             ( ls_bcset_language_value-langu ) ).
    IF lt_languages[] IS INITIAL.

      INSERT sy-langu INTO TABLE lt_languages[].

    ENDIF.

*   Create configuration container for remote file
    ro_container ?= cl_bcfg_config_manager=>create_container( io_container_type  = cl_bcfg_enum_container_type=>classic
                                                              it_langus          = lt_languages[]
                                                              it_object_mappings = it_mappings[]
                                                            ).

*   Content in BC Set format data
    DATA(lt_field_values) = VALUE if_bcfg_config_container=>ty_t_field_values( FOR ls_scprvals IN is_bcset_metadata-scprvals
                                                                                  ( tablename = ls_scprvals-tablename
                                                                                    fieldname = ls_scprvals-fieldname
                                                                                    rec_id    = ls_scprvals-recnumber
                                                                                    value     = ls_scprvals-value ) ).

*   Language dependent content in BC Set format
    LOOP AT is_bcset_metadata-scprvall[] ASSIGNING FIELD-SYMBOL(<ls_scprvall>).

      INSERT VALUE #( tablename = <ls_scprvall>-tablename
                      fieldname = <ls_scprvall>-fieldname
                      rec_id    = <ls_scprvall>-recnumber
                      langu     = <ls_scprvall>-langu
                      value     = <ls_scprvall>-value )
      INTO TABLE lt_field_values[].

    ENDLOOP.

*   Add data from remote file to configuration container
    ro_container->if_bcfg_config_container~add_lines_by_fields( lt_field_values[] ).

  ENDMETHOD.


  METHOD create_customizing_list.

*   Declaration of local internal table
    DATA: lt_objects TYPE scp1_bcs_objects.

*   Declaration of local workarea
    DATA: ls_bcset_metadata TYPE ty_bcset_metadata,
          lv_tr_object_type TYPE e071-object.

    CLEAR: mt_customizing_ui[].

    LOOP AT mo_repository->get_files_remote( ) ASSIGNING FIELD-SYMBOL(<ls_file_remote>)
                                               WHERE path = '/customizing/'.
*     Get object type
      SPLIT to_upper( <ls_file_remote>-filename ) AT '.' INTO DATA(lv_name)
                                                              DATA(lv_object_type)
                                                              DATA(lv_extension).

*     Allow only BC Set format
      CHECK lv_object_type = 'SCP1'.

*     Create file instance
      DATA(lo_object_files) = NEW zcl_abapgit_objects_files( is_item = VALUE #( obj_type = lv_object_type obj_name = lv_name ) ).

*     Add remote file
      lo_object_files->add( <ls_file_remote> ).

      lo_object_files->read_xml( )->read(
        EXPORTING
          iv_name = lv_object_type
        CHANGING
          cg_data = ls_bcset_metadata
      ).

*     Create configuration container for remote file
      DATA(lo_container_remote) = zcl_agc_helper=>create_container( is_bcset_metadata = ls_bcset_metadata ).

*     Create configuration container for local file
      DATA(lo_container_local) = lo_container_remote.

*     Extract key metadata
      DATA(lo_key_container) = lo_container_local->if_bcfg_config_container~extract_key_container( ).

*     Remove the data from container
      lo_container_local->if_bcfg_config_container~remove_all( ).

*     Read data
      lo_container_local->if_bcfg_config_container~add_current_config( lo_key_container ).

      IF lo_container_local->if_bcfg_config_container~equals( lo_container_remote ) = abap_false.

        DATA(lt_color) = VALUE lvc_t_scol( ( color-col = 6 color-int = 1 color-inv = 0 ) ).

      ENDIF.

*     Get mappings
      DATA(lt_mappings) = lo_container_remote->if_bcfg_config_container~get_mappings( ).

      READ TABLE lt_mappings[] ASSIGNING FIELD-SYMBOL(<ls_mapping>) INDEX 1.

      DATA(lv_objecttype) = <ls_mapping>-objecttype.

      CALL FUNCTION 'SCPR_DB_COBJ_TYPE_GET'
        EXPORTING
          custobj           = <ls_mapping>-objectname
        EXCEPTIONS
          object_dont_exist = 1
          OTHERS            = 2.
      IF sy-subrc NE 0.

        lv_objecttype = 'U'.

      ENDIF.

*     Get transport object type
      CALL FUNCTION 'CTO_OBJECT_GET_TROBJECT'
        EXPORTING
          iv_objectname       = <ls_mapping>-objectname " Object Name
          iv_objecttype       = lv_objecttype           " Object Type
        IMPORTING
          ev_object           = lv_tr_object_type
        EXCEPTIONS
          no_transport_object = 1
          OTHERS              = 2.
      CHECK sy-subrc = 0.

      APPEND VALUE #( objecttype             = lv_tr_object_type
                      objectname             = <ls_mapping>-objectname
                      path                   = <ls_file_remote>-path && <ls_file_remote>-filename
                      bcset_id               = ls_bcset_metadata-scprattr-id
                      container_local        = lo_container_local
                      container_remote       = lo_container_remote
                      color                  = lt_color[]
                    ) TO mt_customizing_ui[].

    ENDLOOP.

  ENDMETHOD.


  METHOD display_output.

    DATA lo_column TYPE REF TO cl_salv_column_table.

    TRY.

*       Create ALV instance
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = mo_customizing_output " Basis Class Simple ALV Tables
          CHANGING
            t_table      = mt_customizing_ui[]   " Customizing content list
        ).

*       Set PF-STATUS
        mo_customizing_output->set_screen_status(
          EXPORTING
            report        = sy-cprog
            pfstatus      = 'STANDARD'
            set_functions = mo_customizing_output->c_functions_all
        ).

        DATA(lo_columns) = mo_customizing_output->get_columns( ).
        lo_columns->set_optimize( ).

        lo_column ?= lo_columns->get_column( 'OBJECTTYPE' ).
        lo_column->set_long_text( 'Type' ).

        lo_column ?= lo_columns->get_column( 'OBJECTNAME' ).
        lo_column->set_long_text( 'Name' ).

        lo_column ?= lo_columns->get_column( 'PATH' ).
        lo_column->set_long_text( 'Path' ).

        lo_column ?= lo_columns->get_column( 'BCSET_ID' ).
        lo_column->set_technical( ).

        lo_columns->set_color_column( 'COLOR' ).

        DATA(lo_selections) = mo_customizing_output->get_selections( ).
        lo_selections->set_selection_mode( if_salv_c_selection_mode=>multiple ).

        DATA(lo_events) = mo_customizing_output->get_event( ).

        DATA(lo_event_handler) = NEW lcl_event_handler( ).

        SET HANDLER lo_event_handler->on_user_command FOR lo_events.

        mo_customizing_output->display( ).

      CATCH cx_salv_msg INTO DATA(lo_slav_message). " ALV: General Error Class with Message

        RAISE EXCEPTION TYPE zcx_agc_ui
          EXPORTING
            previous = lo_slav_message.

      CATCH cx_salv_not_found INTO DATA(lo_salv_not_found).

        RAISE EXCEPTION TYPE zcx_agc_ui
          EXPORTING
            previous = lo_salv_not_found.

      CATCH cx_salv_data_error INTO DATA(lo_salv_data_error).

        RAISE EXCEPTION TYPE zcx_agc_ui
          EXPORTING
            previous = lo_salv_data_error.

    ENDTRY.

  ENDMETHOD.

  METHOD zif_agc_ui~get_selected_customizing.

*   Get selected rows
    DATA(lt_selected_rows) = mo_customizing_output->get_selections( )->get_selected_rows( ).

    LOOP AT lt_selected_rows[] ASSIGNING FIELD-SYMBOL(<ls_selected_row>).

      READ TABLE zcl_agc_ui=>mo_abapgit_customizing_ui->mt_customizing_ui[] ASSIGNING FIELD-SYMBOL(<ls_customizing_ui>) INDEX <ls_selected_row>.

      APPEND <ls_customizing_ui> TO rt_customizing_ui[].

    ENDLOOP.

  ENDMETHOD.

  METHOD zif_agc_ui~get_repository.

    ro_repository = mo_repository.

  ENDMETHOD.

ENDCLASS.
