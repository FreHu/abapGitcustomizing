*&---------------------------------------------------------------------*
*& Report zabapgit_customizing
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
  REPORT zabapgit_customizing.

  TABLES sscrfields.

  INCLUDE zabapgit_password_dialog.

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

      CLASS-METHODS get_instance
        RETURNING VALUE(ro_abapgit_customizing) TYPE REF TO lcl_abapgit_customizing.

      METHODS display
        IMPORTING
          iv_key TYPE char12.

      METHODS handle_customzing_request
        IMPORTING
          iv_request TYPE trkorr.

      METHODS stage.

      METHODS pull.

    PRIVATE SECTION.

      TYPES: BEGIN OF ty_customizing,
               objecttype_description TYPE ddtext,
               objecttype             TYPE trobjtype,
               objectname             TYPE trobj_name,
               bcset_id               TYPE string,
               xml_local              TYPE REF TO zcl_abapgit_xml_output,
               xml_remote             TYPE REF TO zcl_abapgit_xml_input,
               color                  TYPE lvc_t_scol,
             END OF ty_customizing.

      TYPES:
        BEGIN OF ty_bcset_metadata,
          scprattr TYPE scprattr,
          scprtext TYPE STANDARD TABLE OF scprtext WITH DEFAULT KEY,
          scprvals TYPE STANDARD TABLE OF scprvals WITH DEFAULT KEY,
          scprvall TYPE STANDARD TABLE OF scprvall WITH DEFAULT KEY,
          scprreca TYPE STANDARD TABLE OF scprreca WITH DEFAULT KEY,
          scprfldv TYPE STANDARD TABLE OF scprfldv WITH DEFAULT KEY,
          subprofs TYPE STANDARD TABLE OF scprpprl WITH DEFAULT KEY,
        END OF ty_bcset_metadata .

      CLASS-DATA mo_abapgit_customizing TYPE REF TO lcl_abapgit_customizing.

      DATA mo_repository TYPE REF TO zcl_abapgit_repo.
      DATA mo_customizing_output TYPE REF TO cl_salv_table.
      DATA mt_abapgit_customizing TYPE STANDARD TABLE OF ty_customizing.
      DATA mt_fielddescrs TYPE scpr_records .

      METHODS create_xml
        IMPORTING
          iv_bcset_id         TYPE scpr_id
          it_record_attribute TYPE scprrecatab
          it_bcset_values     TYPE scprvalstab
        RETURNING
          VALUE(ro_xml_local) TYPE REF TO zcl_abapgit_xml_output.

  ENDCLASS.

  CLASS lcl_event_handler IMPLEMENTATION.

    METHOD on_double_click.

      READ TABLE gt_repository_display[] ASSIGNING FIELD-SYMBOL(<ls_repository_display>) INDEX row.

      DATA(lo_abapgit_customizing) = lcl_abapgit_customizing=>get_instance( ).

      lo_abapgit_customizing->display( <ls_repository_display>-key ).

    ENDMETHOD.

    METHOD on_user_command.

      DATA: lv_selected_request TYPE trkorr.

      CASE e_salv_function.

        WHEN '&TRA'.

          CALL FUNCTION 'TR_F4_REQUESTS'
            EXPORTING
              iv_trfunctions          = 'W'
              iv_via_selection_screen = abap_false
            IMPORTING
              ev_selected_request     = lv_selected_request.

          DATA(lo_abapgit_customizing) = lcl_abapgit_customizing=>get_instance( ).

          lo_abapgit_customizing->handle_customzing_request( lv_selected_request ).

        WHEN 'PUSH'.

          lo_abapgit_customizing = lcl_abapgit_customizing=>get_instance( ).

          lo_abapgit_customizing->stage( ).

        WHEN 'PULL'.

          lo_abapgit_customizing = lcl_abapgit_customizing=>get_instance( ).

          lo_abapgit_customizing->pull( ).

        WHEN OTHERS.

      ENDCASE.

    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_abapgit_customizing IMPLEMENTATION.

    METHOD display.

      DATA: lo_local_container  TYPE REF TO cl_bcfg_bcset_config_container,
            lo_remote_container TYPE REF TO cl_bcfg_bcset_config_container.

      DATA: lt_tr_objects             TYPE STANDARD TABLE OF ko105,
            lt_tr_object_descriptions TYPE STANDARD TABLE OF ko100,
            lt_scprvall               TYPE STANDARD TABLE OF scprvall.

      DATA: ls_bcset_metadata       TYPE ty_bcset_metadata,
            ls_tr_object            TYPE ko105,
            ls_bcset_metadata_local TYPE ty_bcset_metadata.

      DATA(lo_repository_service) = zcl_abapgit_repo_srv=>get_instance( ).

      mo_repository = lo_repository_service->get( iv_key ).

      DATA(lt_remote_files) = mo_repository->get_files_remote( ).

*     Ignore other files
      LOOP AT lt_remote_files[] ASSIGNING FIELD-SYMBOL(<ls_remote_file>)
                                WHERE path = '/customizing/'.

        SPLIT to_upper( <ls_remote_file>-filename ) AT '.' INTO DATA(lv_name)
                                                                DATA(lv_type)
                                                                DATA(lv_ext).

        CHECK lv_type = 'SCP1'.

        DATA(lo_object_files) = NEW zcl_abapgit_objects_files( is_item = VALUE #( obj_type = lv_type obj_name = lv_name ) ).

        lo_object_files->add( <ls_remote_file> ).

        DATA(lo_xml_remote) = lo_object_files->read_xml( ).

        lo_xml_remote->read(
          EXPORTING
            iv_name = 'SCP1'
          CHANGING
            cg_data = ls_bcset_metadata
        ).

        READ TABLE ls_bcset_metadata-scprreca[] ASSIGNING FIELD-SYMBOL(<ls_scprreca>) INDEX 1.

        CALL FUNCTION 'CTO_OBJECT_GET_TROBJECT'
          EXPORTING
            iv_objectname       = <ls_scprreca>-objectname                 " Object Name
            iv_objecttype       = <ls_scprreca>-objecttype                 " Object Type
          IMPORTING
            ev_pgmid            = ls_tr_object-pgmid
            ev_object           = ls_tr_object-object
          EXCEPTIONS
            no_transport_object = 1
            OTHERS              = 2.
        IF sy-subrc =  0.

          APPEND ls_tr_object TO lt_tr_objects[].

        ENDIF.

*       Get object type description
        CALL FUNCTION 'TRINT_OBJECT_TABLE'
          TABLES
            tt_types_in  = lt_tr_objects[]             " Input: Types for search (for IV_COMPLETE = ' ')
            tt_types_out = lt_tr_object_descriptions[]. " Output: Types with texts

        READ TABLE lt_tr_object_descriptions[] ASSIGNING FIELD-SYMBOL(<ls_tr_object_description>) INDEX 1.

        DATA(lt_mappings) = VALUE if_bcfg_config_container=>ty_t_mapping_info( ( objectname = <ls_scprreca>-objectname objecttype = <ls_scprreca>-objecttype ) ).

        lt_scprvall[] = ls_bcset_metadata-scprvall[].

        SORT lt_scprvall[] BY langu.

        DELETE ADJACENT DUPLICATES FROM lt_scprvall[]
        COMPARING langu.

        DATA(lt_languages) = VALUE if_bcfg_config_container=>ty_t_languages( FOR ls_scprvall IN lt_scprvall[] ( ls_scprvall-langu ) ).
        IF lt_languages[] IS INITIAL.

          APPEND sy-langu TO lt_languages[].

        ENDIF.

        DATA(lt_field_values) = VALUE if_bcfg_config_container=>ty_t_field_values( FOR ls_scprvals IN ls_bcset_metadata-scprvals
                                                                                      ( tablename = ls_scprvals-tablename
                                                                                        fieldname = ls_scprvals-fieldname
                                                                                        rec_id    = ls_scprvals-recnumber
                                                                                        value     = ls_scprvals-value ) ).

        LOOP AT ls_bcset_metadata-scprvall[] ASSIGNING FIELD-SYMBOL(<ls_scprvall>).

          INSERT VALUE #( tablename = <ls_scprvall>-tablename
                          fieldname = <ls_scprvall>-fieldname
                          rec_id    = <ls_scprvall>-recnumber
                          langu     = <ls_scprvall>-langu
                          value     = <ls_scprvall>-value )
          INTO TABLE lt_field_values[].

        ENDLOOP.

*       Create configuration container for remote file
        lo_remote_container ?= cl_bcfg_config_manager=>create_container(
                                     io_container_type  = cl_bcfg_enum_container_type=>classic
                                     it_langus          = lt_languages[]
                                     it_object_mappings = lt_mappings[]
                               ).

*       Add data from remote file to configuration container
        lo_remote_container->if_bcfg_config_container~add_lines_by_fields( lt_field_values[] ).

*       Create configuration container for local file
        lo_local_container ?= cl_bcfg_config_manager=>create_container(
                                     io_container_type  = cl_bcfg_enum_container_type=>classic
                                     it_langus          = lt_languages[]
                                     it_object_mappings = lt_mappings[]
                              ).

*       Add data from local file to configuration container
        lo_local_container->if_bcfg_config_container~add_lines_by_fields( lt_field_values[] ).

        DATA(lo_key_container) = lo_local_container->if_bcfg_config_container~extract_key_container( ).

*       Remove the data from container
        lo_local_container->if_bcfg_config_container~remove_all( ).

*       Read data
        lo_local_container->if_bcfg_config_container~add_current_config( lo_key_container ).

*       Convert to SCP1 format
        lo_local_container->if_bcfg_has_data_manager~get_data_manager( )->convert_to_bcset(
          EXPORTING
            iv_id   = lo_local_container->if_bcfg_config_container~get_id( )
          CHANGING
            ct_reca = ls_bcset_metadata_local-scprreca[]
            ct_vals = ls_bcset_metadata_local-scprvals[]
            ct_vall = ls_bcset_metadata_local-scprvall[]
        ).

*       Create XML file
        DATA(lo_xml_local) = create_xml( iv_bcset_id         = ls_bcset_metadata-scprattr-id
                                         it_record_attribute = ls_bcset_metadata-scprreca[]
                                         it_bcset_values     = ls_bcset_metadata_local-scprvals[]
                                       ).

*       Is there a difference between containers?
        DATA(lv_is_equal) = lo_local_container->if_bcfg_config_container~equals( lo_remote_container ).
        IF lv_is_equal = abap_false.
          DATA(lt_color) = VALUE lvc_t_scol( ( color-col = 6 color-int = 1 color-inv = 0 ) ).
        ENDIF.

        APPEND VALUE #( objecttype_description = <ls_tr_object_description>-text
                        objecttype             = <ls_tr_object_description>-object
                        objectname             = <ls_scprreca>-objectname
                        bcset_id               = <ls_scprreca>-id
                        xml_local              = lo_xml_local
                        xml_remote             = lo_xml_remote
                        color                  = lt_color[]
                      ) TO mt_abapgit_customizing[].

      ENDLOOP.

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

      DATA(lo_columns) = mo_customizing_output->get_columns( ).
      lo_columns->set_optimize( ).

      DATA(lo_column) = lo_columns->get_column( 'BCSET_ID' ).
      lo_column->set_technical( ).

      lo_columns->set_color_column( 'COLOR' ).

      DATA(lo_selections) = mo_customizing_output->get_selections( ).
      lo_selections->set_selection_mode( if_salv_c_selection_mode=>multiple ).

      DATA(lo_events) = mo_customizing_output->get_event( ).

      DATA(lo_event_handler) = NEW lcl_event_handler( ).

      SET HANDLER lo_event_handler->on_user_command FOR lo_events.

      mo_customizing_output->display( ).

    ENDMETHOD.

    METHOD get_instance.

      IF mo_abapgit_customizing IS NOT BOUND.

        mo_abapgit_customizing = NEW #( ).

      ENDIF.

      ro_abapgit_customizing = mo_abapgit_customizing.

    ENDMETHOD.

    METHOD handle_customzing_request.

      DATA: lt_transport_objects      TYPE STANDARD TABLE OF e071,
            lt_object_table_keys      TYPE STANDARD TABLE OF e071k,
            lt_record_attributes      TYPE STANDARD TABLE OF scprreca,
            lt_all_values             TYPE STANDARD TABLE OF scprvals,
            lt_recaattr               TYPE STANDARD TABLE OF scprreca,
            lt_values                 TYPE STANDARD TABLE OF scprvals,
            lt_objects                TYPE scp1_act_objects,
            lt_tr_objects             TYPE STANDARD TABLE OF ko105,
            lt_tr_object_descriptions TYPE STANDARD TABLE OF ko100.

      DATA: ls_tr_object TYPE ko105.

      DATA: lv_bcset_id    TYPE scpr_id,
            lv_system_type TYPE sy-sysid.

*     Read request
      CALL FUNCTION 'SCPR_TR_READ_REQUEST'
        EXPORTING
          order_no      = iv_request
        TABLES
          e071          = lt_transport_objects[]
          e071k         = lt_object_table_keys[]
        EXCEPTIONS
          empty_request = 1
          OTHERS        = 2.
      IF sy-subrc NE 0.
        MESSAGE s134(scpr).
        RETURN.
      ENDIF.

*     Convert transport object and table keys to SCP1 format
      CALL FUNCTION 'SCPR_TR_READ_DATA'
        IMPORTING
          tab_fielddescrs = mt_fielddescrs[]
          tab_recattr     = lt_record_attributes[]
          tab_values      = lt_all_values[]
        TABLES
          tab_e071        = lt_transport_objects[]
          tab_e071k       = lt_object_table_keys[]
        EXCEPTIONS
          no_data_found   = 1
          OTHERS          = 2.
      IF sy-subrc <> 0.
*           MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

*     Process table/customizing object
      LOOP AT lt_record_attributes[] ASSIGNING FIELD-SYMBOL(<ls_record_attribute>)
                                     GROUP BY ( tablename  = <ls_record_attribute>-tablename
                                                objectname = <ls_record_attribute>-objectname
                                                objecttype = <ls_record_attribute>-objecttype
                                                clustname  = <ls_record_attribute>-clustname
                                              ) ASSIGNING FIELD-SYMBOL(<ls_recaattr_group>)..

        lt_recaattr[] = VALUE #( FOR ls_recaattr IN GROUP <ls_recaattr_group> ( ls_recaattr ) ).

*       Retrieve the relevant customizing content
        lt_values[] = VALUE #( FOR ls_recaattr_tmp IN lt_recaattr[]
                               FOR ls_values       IN lt_all_values[] WHERE (     tablename = ls_recaattr_tmp-tablename
                                                                              AND recnumber = ls_recaattr_tmp-recnumber )
                               ( ls_values ) ).

*       Extract object
        CALL FUNCTION 'SCPR_ACTIV_EXTRACT_OBJECTS'
          IMPORTING
            act_objects = lt_objects[]
          TABLES
            recattr     = lt_recaattr[].

        READ TABLE lt_objects[] ASSIGNING FIELD-SYMBOL(<ls_object>) INDEX 1.

*       Create BC Set ID
        CONCATENATE <ls_object>-objectname
                    <ls_object>-objecttype
        INTO lv_bcset_id
        SEPARATED BY '_'.

*       Create XML file
        DATA(lo_xml_data) = create_xml( iv_bcset_id         = lv_bcset_id
                                        it_record_attribute = lt_recaattr[]
                                        it_bcset_values     = lt_values[]
                                      ).

        CALL FUNCTION 'CTO_OBJECT_GET_TROBJECT'
          EXPORTING
            iv_objectname       = <ls_object>-objectname                 " Object Name
            iv_objecttype       = <ls_object>-objecttype                 " Object Type
          IMPORTING
            ev_pgmid            = ls_tr_object-pgmid
            ev_object           = ls_tr_object-object
          EXCEPTIONS
            no_transport_object = 1
            OTHERS              = 2.
        IF sy-subrc =  0.

          APPEND ls_tr_object TO lt_tr_objects[].

        ENDIF.

*       Get object type description
        CALL FUNCTION 'TRINT_OBJECT_TABLE'
          TABLES
            tt_types_in  = lt_tr_objects[]             " Input: Types for search (for IV_COMPLETE = ' ')
            tt_types_out = lt_tr_object_descriptions[]. " Output: Types with texts

        READ TABLE lt_tr_object_descriptions[] ASSIGNING FIELD-SYMBOL(<ls_tr_object_description>) INDEX 1.

        APPEND VALUE #( objecttype_description = <ls_tr_object_description>-text
                        objecttype             = <ls_tr_object_description>-object
                        objectname             = <ls_object>-objectname
                        bcset_id               = lv_bcset_id
                        xml_local              = lo_xml_data
                      ) TO mt_abapgit_customizing[].

      ENDLOOP.

      mo_customizing_output->refresh( ).

    ENDMETHOD.

    METHOD create_xml.

*     Declaration of local workarea
      DATA: ls_bcset_metadata TYPE ty_bcset_metadata,
            ls_bcset_text     TYPE scprtext.

*     Declaration of local variable
      DATA: lv_system_type TYPE sy-sysid,
            lv_org_id      TYPE scpr_orgid.

*     Declaration of local field symbols
      FIELD-SYMBOLS: <ls_recattr> TYPE scprreca,
                     <ls_values>  TYPE scprvals.

*     Instantiate the XML object
      CREATE OBJECT ro_xml_local.

*     Get system type
      CALL FUNCTION 'TR_SYS_PARAMS'
        IMPORTING
          systemtype    = lv_system_type
        EXCEPTIONS
          no_systemname = 1
          no_systemtype = 2
          OTHERS        = 3.
      IF sy-subrc = 0.

        lv_org_id = SWITCH #( lv_system_type WHEN 'SAP' THEN '/0SAP/' ELSE '/CUSTOMER/' ).

      ENDIF. " IF sy-subrc <> 0

*     Populate BC set header data
      ls_bcset_metadata-scprattr-id      = iv_bcset_id.
      ls_bcset_metadata-scprattr-version = 'N'.
      ls_bcset_metadata-scprattr-type    = 'GEN'.
      ls_bcset_metadata-scprattr-reftype = 'TRAN'.
*    ls_bcset_metadata-scprattr-refname = ms_request_details-h-trkorr.
      ls_bcset_metadata-scprattr-orgid   = lv_org_id.

*     Populate short text
      ls_bcset_text-id      = ls_bcset_metadata-scprattr-id.
      ls_bcset_text-version = ls_bcset_metadata-scprattr-version.
      ls_bcset_text-langu   = sy-langu.
      ls_bcset_text-text    = 'Generated via ABAPGIT'(004).
      APPEND ls_bcset_text TO ls_bcset_metadata-scprtext[].

*     Populate records and values metadata
      ls_bcset_metadata-scprreca[] = it_record_attribute[].
      LOOP AT ls_bcset_metadata-scprreca[] ASSIGNING <ls_recattr>.
        <ls_recattr>-id = iv_bcset_id.
      ENDLOOP. " LOOP AT ls_bcset_metadata-scprreca[] ASSIGNING <ls_recattr>

      ls_bcset_metadata-scprvals[] = it_bcset_values[].
      LOOP AT ls_bcset_metadata-scprvals[] ASSIGNING <ls_values>.
        <ls_values>-id = iv_bcset_id.
      ENDLOOP. " LOOP AT ls_bcset_metadata-scprvals[] ASSIGNING <ls_values>

*     Get language values
      CALL FUNCTION 'SCPR_TEMPL_CT_LANG_ALL_GET'
        EXPORTING
          bcset_id    = ls_bcset_metadata-scprattr-id
        TABLES
          values      = ls_bcset_metadata-scprvals[]
          valuesl     = ls_bcset_metadata-scprvall[]
          recattr     = ls_bcset_metadata-scprreca[]
          fielddescrs = mt_fielddescrs[].

*     Add metadata to XML
      ro_xml_local->add( iv_name = 'SCP1'
                          ig_data = ls_bcset_metadata
                        ).

    ENDMETHOD.

    METHOD stage.

      DATA: lo_repository TYPE REF TO zcl_abapgit_repo_online.

      DATA: lt_fields TYPE STANDARD TABLE OF sval.

      DATA: ls_commit_fields TYPE zif_abapgit_services_git=>ty_commit_fields.

      DATA(lo_selections) = mo_customizing_output->get_selections( ).

      DATA(lt_selected_rows) = lo_selections->get_selected_rows( ).

      DATA(lo_staged_files) = NEW zcl_abapgit_stage( ).

      LOOP AT lt_selected_rows[] ASSIGNING FIELD-SYMBOL(<lv_selected_row>).

        READ TABLE mt_abapgit_customizing[] ASSIGNING FIELD-SYMBOL(<ls_abapgit_customizing>) INDEX <lv_selected_row>.

        DATA(ls_item) = VALUE zif_abapgit_definitions=>ty_item( obj_type = 'SCP1'
                                                                obj_name = <ls_abapgit_customizing>-bcset_id
                                                                devclass = mo_repository->get_package( )
                                                              ).

        DATA(lo_object_files) = NEW zcl_abapgit_objects_files( is_item = ls_item ).

*       add xml data to file
        lo_object_files->add_xml( <ls_abapgit_customizing>-xml_local ).

        LOOP AT lo_object_files->get_files( ) ASSIGNING FIELD-SYMBOL(<ls_file>).

*         Add files to stage
          lo_staged_files->add( iv_path     = '/customizing/'
                                iv_filename = <ls_file>-filename
                                iv_data     = <ls_file>-data
                              ).

        ENDLOOP.

      ENDLOOP.

      lo_repository ?= mo_repository.

      DATA(lo_user) = zcl_abapgit_persistence_user=>get_instance( ).

      lt_fields[] = VALUE #( ( tabname = 'SUID_ST_NODE_PERSON_NAME'    fieldname = 'NAME_TEXT'   value     = lo_user->get_repo_git_user_name( lo_repository->get_url( ) )  field_obl = abap_true )
                             ( tabname = 'SUID_ST_NODE_COMM_DATA'      fieldname = 'SMTP_ADDR'   value     = lo_user->get_repo_git_user_email( lo_repository->get_url( ) ) field_obl = abap_true )
                             ( tabname = 'SUID_ST_NODE_TECH_USER_DATA' fieldname = 'TECHDESC'    fieldtext = 'Comment'                                                     field_obl = abap_true ) ).

      CALL FUNCTION 'POPUP_GET_VALUES'
        EXPORTING
          popup_title     = 'Commit'    " Text of title line
        TABLES
          fields          = lt_fields[] " Table fields, values and attributes
        EXCEPTIONS
          error_in_fields = 1           " FIELDS were transferred incorrectly
          OTHERS          = 2.
      IF sy-subrc = 0.

        LOOP AT lt_fields[] ASSIGNING FIELD-SYMBOL(<ls_field>).

          CASE <ls_field>-fieldname.

            WHEN 'NAME_TEXT'.
              ls_commit_fields-committer_name = <ls_field>-value.

            WHEN 'SMTP_ADDR'.
              ls_commit_fields-committer_email = <ls_field>-value.

            WHEN 'TECHDESC'.
              ls_commit_fields-comment = <ls_field>-value.

            WHEN OTHERS.
          ENDCASE.

        ENDLOOP.

      ENDIF.

      zcl_abapgit_services_git=>commit( io_repo   = lo_repository
                                        is_commit = ls_commit_fields
                                        io_stage  = lo_staged_files
                                      ).

    ENDMETHOD.

    METHOD pull.

      DATA: ls_bcset_metadata TYPE ty_bcset_metadata.

      DATA(lo_selections) = mo_customizing_output->get_selections( ).

      DATA(lt_selected_rows) = lo_selections->get_selected_rows( ).

      LOOP AT lt_selected_rows[] ASSIGNING FIELD-SYMBOL(<lv_selected_row>).

        READ TABLE mt_abapgit_customizing[] ASSIGNING FIELD-SYMBOL(<ls_abapgit_customizing>) INDEX <lv_selected_row>.

        <ls_abapgit_customizing>-xml_remote->read(
          EXPORTING
            iv_name = 'SCP1'
          CHANGING
            cg_data = ls_bcset_metadata
        ).

        READ TABLE ls_bcset_metadata-scprreca[] ASSIGNING FIELD-SYMBOL(<ls_scprreca>) INDEX 1.

        DATA(lt_mappings) = VALUE if_bcfg_config_container=>ty_t_mapping_info( ( objectname = <ls_scprreca>-objectname objecttype = <ls_scprreca>-objecttype ) ).

        DATA(lt_scprvall) = ls_bcset_metadata-scprvall[].

        SORT lt_scprvall[] BY langu.

        DELETE ADJACENT DUPLICATES FROM lt_scprvall[]
        COMPARING langu.

        DATA(lt_languages) = VALUE if_bcfg_config_container=>ty_t_languages( FOR ls_scprvall IN lt_scprvall[] ( ls_scprvall-langu ) ).
        IF lt_languages[] IS INITIAL.

          APPEND sy-langu TO lt_languages[].

        ENDIF.

        DATA(lt_field_values) = VALUE if_bcfg_config_container=>ty_t_field_values( FOR ls_scprvals IN ls_bcset_metadata-scprvals
                                                                                      ( tablename = ls_scprvals-tablename
                                                                                        fieldname = ls_scprvals-fieldname
                                                                                        rec_id    = ls_scprvals-recnumber
                                                                                        value     = ls_scprvals-value ) ).

        LOOP AT ls_bcset_metadata-scprvall[] ASSIGNING FIELD-SYMBOL(<ls_scprvall>).

          INSERT VALUE #( tablename = <ls_scprvall>-tablename
                          fieldname = <ls_scprvall>-fieldname
                          rec_id    = <ls_scprvall>-recnumber
                          langu     = <ls_scprvall>-langu
                          value     = <ls_scprvall>-value )
          INTO TABLE lt_field_values[].

        ENDLOOP.

*       Create configuration container for remote file
        DATA(lo_container) = cl_bcfg_config_manager=>create_container( io_container_type  = cl_bcfg_enum_container_type=>classic
                                                                       it_langus          = lt_languages[]
                                                                       it_object_mappings = lt_mappings[]
                                                                       io_commit_mode     = cl_bcfg_enum_commit_mode=>auto_commit
                                                                     ).

*       Add data from remote file to configuration container
        lo_container->add_lines_by_fields( lt_field_values[] ).

*       Apply customizing content
        DATA(lo_result) = lo_container->apply( ).

      ENDLOOP.

    ENDMETHOD.

  ENDCLASS.

  INITIALIZATION.

    lcl_password_dialog=>on_screen_init( ).

  AT SELECTION-SCREEN OUTPUT.

    IF sy-dynnr = lcl_password_dialog=>c_dynnr.
      lcl_password_dialog=>on_screen_output( ).
    ENDIF.

  AT SELECTION-SCREEN.

    IF sy-dynnr = lcl_password_dialog=>c_dynnr.
      lcl_password_dialog=>on_screen_event( sscrfields-ucomm ).
    ENDIF.

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

  FORM password_popup
        USING
          pv_repo_url TYPE string
        CHANGING
          cv_user     TYPE string
          cv_pass     TYPE string.

    lcl_password_dialog=>popup(
      EXPORTING
        iv_repo_url     = pv_repo_url
      CHANGING
        cv_user         = cv_user
        cv_pass         = cv_pass ).

  ENDFORM.
