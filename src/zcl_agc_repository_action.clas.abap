CLASS zcl_agc_repository_action DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_agc_repository_action .

    CLASS-METHODS get_instance
      RETURNING VALUE(ro_repository_action) TYPE REF TO zif_agc_repository_action.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA mo_repository_action TYPE REF TO zcl_agc_repository_action.

    METHODS change_bcset_metadata
      IMPORTING
        iv_bcset_id       TYPE scpr_id
      CHANGING
        cs_bcset_metadata TYPE zcl_agc_helper=>ty_bcset_metadata.

    METHODS get_committer_details
      RETURNING VALUE(rs_commit_details) TYPE zif_abapgit_services_git=>ty_commit_fields.

ENDCLASS.



CLASS zcl_agc_repository_action IMPLEMENTATION.

  METHOD zif_agc_repository_action~push.

*   Declaration of local workarea
    DATA: ls_bcset_metadata TYPE zcl_agc_helper=>ty_bcset_metadata.

*   Get instance
    DATA(lo_customizing_ui) = zcl_agc_ui=>get_instance( ).

    DATA(lt_customizing_ui) = lo_customizing_ui->get_selected_customizing( ).

    DATA(lo_staged_files) = NEW zcl_abapgit_stage( ).

    LOOP AT lt_customizing_ui[] ASSIGNING FIELD-SYMBOL(<ls_customizing_ui>).

*     Convert configuration container data to BC Set metadata format
      <ls_customizing_ui>-container_local->if_bcfg_has_data_manager~get_data_manager( )->convert_to_bcset(
        EXPORTING
          iv_id   = <ls_customizing_ui>-container_local->if_bcfg_config_container~get_id( )
        CHANGING
          ct_reca = ls_bcset_metadata-scprreca[]
          ct_vals = ls_bcset_metadata-scprvals[]
          ct_vall = ls_bcset_metadata-scprvall[]
      ).

*     Adjust BC Set metadata
      change_bcset_metadata(
        EXPORTING
          iv_bcset_id       = CONV #( <ls_customizing_ui>-bcset_id )
        CHANGING
          cs_bcset_metadata = ls_bcset_metadata
      ).

*     Instantiate XML object
      DATA(lo_xml_data) = NEW zcl_abapgit_xml_output( ).

      lo_xml_data->add( iv_name = 'SCP1'
                        ig_data = ls_bcset_metadata
                      ).

      CLEAR: ls_bcset_metadata.

      DATA(ls_item) = VALUE zif_abapgit_definitions=>ty_item( obj_type = 'SCP1'
                                                              obj_name = <ls_customizing_ui>-bcset_id
                                                              devclass = zcl_agc_ui=>get_instance( )->get_repository( )->get_package( )
                                                            ).

      DATA(lo_object_files) = NEW zcl_abapgit_objects_files( is_item = ls_item ).

*     Add xml data to file
      lo_object_files->add_xml( lo_xml_data ).

      LOOP AT lo_object_files->get_files( ) ASSIGNING FIELD-SYMBOL(<ls_file>).

*       Add files to stage
        lo_staged_files->add( iv_path     = '/customizing/'
                              iv_filename = <ls_file>-filename
                              iv_data     = <ls_file>-data
                            ).

      ENDLOOP.

    ENDLOOP.

*   Get committer details
    DATA(ls_commit_details) = get_committer_details( ).

*   Commit files
    zcl_abapgit_services_git=>commit( io_repo   = zcl_agc_ui=>get_instance( )->get_repository( )
                                      is_commit = ls_commit_details
                                      io_stage  = lo_staged_files
                                    ).

  ENDMETHOD.

  METHOD get_instance.

    IF mo_repository_action IS NOT BOUND.

      mo_repository_action = NEW #( ).

    ENDIF.

    ro_repository_action ?= mo_repository_action.

  ENDMETHOD.

  METHOD change_bcset_metadata.

*   Declaration of local variable
    DATA: lv_system_type TYPE sy-sysid,
          lv_org_id      TYPE scpr_orgid.

*   Get system type
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
    cs_bcset_metadata-scprattr-id      = iv_bcset_id.
    cs_bcset_metadata-scprattr-version = 'N'.
    cs_bcset_metadata-scprattr-type    = 'GEN'.
    cs_bcset_metadata-scprattr-orgid   = lv_org_id.

*   Populate short text
    cs_bcset_metadata-scprtext[] = VALUE #( ( id      = cs_bcset_metadata-scprattr-id
                                              version = cs_bcset_metadata-scprattr-version
                                              langu   = sy-langu
                                              text    = 'Generated via ABAPGIT'(004) ) ).

    LOOP AT cs_bcset_metadata-scprreca[] ASSIGNING FIELD-SYMBOL(<ls_recattr>).
      <ls_recattr>-id = iv_bcset_id.
    ENDLOOP. " LOOP AT ls_bcset_metadata-scprreca[] ASSIGNING <ls_recattr>

    LOOP AT cs_bcset_metadata-scprvals[] ASSIGNING FIELD-SYMBOL(<ls_values>).
      <ls_values>-id = iv_bcset_id.
    ENDLOOP. " LOOP AT ls_bcset_metadata-scprvals[] ASSIGNING <ls_values>

    LOOP AT cs_bcset_metadata-scprvall[] ASSIGNING FIELD-SYMBOL(<ls_language_values>).
      <ls_language_values>-id = iv_bcset_id.
    ENDLOOP. " LOOP AT ls_bcset_metadata-scprvals[] ASSIGNING <ls_values>

  ENDMETHOD.

  METHOD get_committer_details.

*   Declaration of local internal table
    DATA: lt_fields TYPE STANDARD TABLE OF sval.

*   Get instance
    DATA(lo_customizing_ui) = zcl_agc_ui=>get_instance( ).

    DATA(lo_repository) = lo_customizing_ui->get_repository( ).

*   Get user persistence instance
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
            rs_commit_details-committer_name  = <ls_field>-value.

          WHEN 'SMTP_ADDR'.
            rs_commit_details-committer_email = <ls_field>-value.

          WHEN 'TECHDESC'.
            rs_commit_details-comment         = <ls_field>-value.

          WHEN OTHERS.
        ENDCASE.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD zif_agc_repository_action~pull.

*   Get instance
    DATA(lo_customizing_ui) = zcl_agc_ui=>get_instance( ).

    DATA(lt_customizing_ui) = lo_customizing_ui->get_selected_customizing( ).

    LOOP AT lt_customizing_ui[] ASSIGNING FIELD-SYMBOL(<ls_customizing_ui>).

*     Create request manager
      DATA(lo_request_manager) = cl_cts_request_factory=>create( sysname = sy-sysid ).

*     Get open customizing request
      lo_request_manager->select_requests(
        EXPORTING
          user              = sy-uname
          request_selection = VALUE #( ( client = sy-mandt reqfunctions = 'W' reqstatus = 'D' ) )
        IMPORTING
          requests          = DATA(lt_requests)
      ).

      READ TABLE lt_requests[] ASSIGNING FIELD-SYMBOL(<ls_request>) INDEX 1.

*     Apply
      <ls_customizing_ui>-container_result = <ls_customizing_ui>-container_remote->if_bcfg_config_container~apply( iv_tp_cust = <ls_request>-h-trkorr ).

      <ls_customizing_ui>-container_local = <ls_customizing_ui>-container_remote.
      CLEAR <ls_customizing_ui>-color[].

      APPEND <ls_customizing_ui> TO rt_imported_objects[].

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
