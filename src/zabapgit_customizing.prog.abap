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

      CLASS-METHODS get_instance
        RETURNING VALUE(ro_abapgit_customizing) TYPE REF TO lcl_abapgit_customizing.

      METHODS display
        IMPORTING
          iv_key TYPE char12.

      METHODS handle_customzing_request
        IMPORTING
          iv_request TYPE trkorr.

    PRIVATE SECTION.

      TYPES: BEGIN OF ty_customizing,
               objecttype_description TYPE ddtext,
               objecttype             TYPE trobjtype,
               objectname             TYPE trobj_name,
               xml_output             TYPE REF TO zcl_abapgit_xml_output,
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
          iv_bcset_id          TYPE scpr_id
          it_record_attribute  TYPE scprrecatab
          it_bcset_values      TYPE scprvalstab
        RETURNING
          VALUE(ro_xml_output) TYPE REF TO zcl_abapgit_xml_output.

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

        WHEN OTHERS.

      ENDCASE.

    ENDMETHOD.

  ENDCLASS.

  CLASS lcl_abapgit_customizing IMPLEMENTATION.

    METHOD display.

      DATA(lo_repository_service) = zcl_abapgit_repo_srv=>get_instance( ).

      mo_repository = lo_repository_service->get( iv_key ).

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

      DATA(lo_columns) = mo_customizing_output->get_columns( ).
      lo_columns->set_optimize( ).

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
                        xml_output             = lo_xml_data
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
      CREATE OBJECT ro_xml_output.

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
      ls_bcset_metadata-scprattr-type    = 'AGC'.
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
      ro_xml_output->add( iv_name = 'SCP1'
                          ig_data = ls_bcset_metadata
                        ).

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
