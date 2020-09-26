CLASS zcl_agc_request_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_agc_request_handler .

    CLASS-METHODS get_instance
      RETURNING VALUE(ro_request_handler) TYPE REF TO zif_agc_request_handler.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA mo_request_handler TYPE REF TO zcl_agc_request_handler.

    DATA mt_request_objects   TYPE STANDARD TABLE OF e071.
    DATA mt_object_table_keys TYPE STANDARD TABLE OF e071k.
    DATA mt_field_descrs TYPE scpr_records.

    METHODS convert_to_bcset_format
      EXPORTING
        et_record_attributes TYPE scprrecatab
        et_values            TYPE scprvalstab
        et_language_values   TYPE scprvalltab.

    METHODS convert_record_attributes
      EXPORTING
        et_recaattrs_extended  TYPE scpr_recattrs
        et_orig_recnum_assigns TYPE scpr_orig_recnum_assigns
      CHANGING
        cs_bcset_metadata      TYPE zcl_agc_helper=>ty_bcset_metadata.

ENDCLASS.



CLASS zcl_agc_request_handler IMPLEMENTATION.

  METHOD get_instance.

    IF mo_request_handler IS NOT BOUND.

      mo_request_handler = NEW #( ).

    ENDIF.

    ro_request_handler ?= mo_request_handler.

  ENDMETHOD.

  METHOD zif_agc_request_handler~select.

*   Declaration of local variable
    DATA: lv_selected_request TYPE trkorr.

*   Select transport request
    CALL FUNCTION 'TR_F4_REQUESTS'
      EXPORTING
        iv_trfunctions          = 'W'
        iv_via_selection_screen = abap_false
      IMPORTING
        ev_selected_request     = lv_selected_request.

*   Read request
    CALL FUNCTION 'SCPR_TR_READ_REQUEST'
      EXPORTING
        order_no      = lv_selected_request
      TABLES
        e071          = mt_request_objects[]
        e071k         = mt_object_table_keys[]
      EXCEPTIONS
        empty_request = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

  METHOD convert_to_bcset_format.

*   Declaration of local internal table
    DATA: lt_status_list  TYPE scpr_transp_entries.

*   Initialize EXPORTING parameters
    CLEAR: et_record_attributes[],
           et_values[],
           et_language_values[].

*   Convert transport objects and its table keys to BC Set format
    CALL FUNCTION 'SCPR_TR_READ_DATA'
      EXPORTING
        only_keys       = abap_true
      IMPORTING
        tab_fielddescrs = mt_field_descrs[]
        tab_recattr     = et_record_attributes[]
        tab_values      = et_values[]
      TABLES
        tab_e071        = mt_request_objects[]   " Change & Transport System: Object Entries of Requests/Tasks
        tab_e071k       = mt_object_table_keys[] " Change & Transport System: Key Entries of Requests/Tasks
        tab_statuslist  = lt_status_list[]
      EXCEPTIONS
        no_data_found   = 1
        OTHERS          = 2.
    IF sy-subrc = 0.

    ENDIF.

  ENDMETHOD.

  METHOD zif_agc_request_handler~get_data.

    DATA: lt_scprvals TYPE scprvalstab.

*   Declaration of local workarea
    DATA: ls_bcset_metadata TYPE zcl_agc_helper=>ty_bcset_metadata.

*   Declaration of local variable
    DATA: lv_tr_object_type TYPE e071-object.

*   Convert request data to BC Set format
    convert_to_bcset_format(
      IMPORTING
        et_record_attributes = DATA(lt_record_attributes)
        et_values            = DATA(lt_all_values)
        et_language_values   = DATA(lt_language_all_values)
    ).

*   Process table/customizing object
    LOOP AT lt_record_attributes[] ASSIGNING FIELD-SYMBOL(<ls_record_attribute>)
                                   GROUP BY ( tablename  = <ls_record_attribute>-tablename
                                              objectname = <ls_record_attribute>-objectname
                                              objecttype = <ls_record_attribute>-objecttype
                                              clustname  = <ls_record_attribute>-clustname
                                            ) ASSIGNING FIELD-SYMBOL(<ls_recaattr_group>).

*     Get BC Set records attributes
      ls_bcset_metadata-scprreca[] = VALUE #( FOR ls_recaattr IN GROUP <ls_recaattr_group> ( ls_recaattr ) ).

*     Get the relevant customizing content
      ls_bcset_metadata-scprvals[] = VALUE #( FOR ls_recaattr_tmp IN ls_bcset_metadata-scprreca[]
                                              FOR ls_values       IN lt_all_values[] WHERE (     tablename = ls_recaattr_tmp-tablename
                                                                                             AND recnumber = ls_recaattr_tmp-recnumber )
                                              ( ls_values ) ).

      convert_record_attributes(
        IMPORTING
          et_recaattrs_extended  = DATA(lt_recaattrs_extended)
          et_orig_recnum_assigns = DATA(lt_orig_recnum_assigns)
        CHANGING
          cs_bcset_metadata      = ls_bcset_metadata
      ).

      lt_scprvals[] = ls_bcset_metadata-scprvals[].

      CALL FUNCTION 'SCPR_TEMPL_MN_RECORDS_COMPLETE'
        TABLES
          values              = lt_scprvals[]
          recattr             = lt_recaattrs_extended[]      " Data Record Attributes
          fielddescrs         = mt_field_descrs[]            " Field Descriptions
          orig_recnum_assigns = lt_orig_recnum_assigns[].

      DATA(lv_lines) = lines( lt_recaattrs_extended[] ).

      IF lv_lines NE lines( ls_bcset_metadata-scprreca[] ).

        READ TABLE lt_recaattrs_extended[] ASSIGNING FIELD-SYMBOL(<ls_recaatrr_extended>) INDEX lv_lines.
        IF sy-subrc = 0.

          DATA(lv_record_numder) = <ls_recaatrr_extended>-recnumber.

        ENDIF.

        LOOP AT lt_orig_recnum_assigns[] ASSIGNING FIELD-SYMBOL(<ls_orig_recnum_assign>).

          READ TABLE lt_recaattrs_extended[] TRANSPORTING NO FIELDS
                                             WITH KEY key = <ls_orig_recnum_assign>-key.
          CHECK sy-subrc NE 0.

          DATA(ls_recaattr_extended) = CORRESPONDING scpr_recattr( <ls_orig_recnum_assign> ).

          ls_recaattr_extended-recnumber = lv_record_numder = lv_record_numder + 1.
          CLEAR: ls_recaattr_extended-uncomplete.

*         Mark the record as deleted
          ls_recaattr_extended-deleteflag = 'L'.

          APPEND ls_recaattr_extended TO lt_recaattrs_extended[].

          LOOP AT ls_bcset_metadata-scprvals[] ASSIGNING FIELD-SYMBOL(<ls_scprvals>)
                                               WHERE recnumber = <ls_orig_recnum_assign>-orig_recnum.
            <ls_scprvals>-recnumber = ls_recaattr_extended-recnumber.
            APPEND <ls_scprvals> TO lt_scprvals[].

          ENDLOOP.

        ENDLOOP.

      ENDIF.

      ls_bcset_metadata-scprreca[] = CORRESPONDING #( lt_recaattrs_extended[] ).
      ls_bcset_metadata-scprvals[] = lt_scprvals[].

*     Get language dependent values
      CALL FUNCTION 'SCPR_TEMPL_CT_LANG_ALL_GET'
        TABLES
          values      = ls_bcset_metadata-scprvals[]
          valuesl     = ls_bcset_metadata-scprvall[]
          recattr     = ls_bcset_metadata-scprreca[]
          fielddescrs = mt_field_descrs[].

*     Create container
      DATA(lo_container_local) = zcl_agc_helper=>create_container( ls_bcset_metadata ).

      CLEAR: lt_recaattrs_extended[],
             lt_orig_recnum_assigns[],
             ls_bcset_metadata.

*     Get mappings
      DATA(lt_mappings) = lo_container_local->if_bcfg_config_container~get_mappings( ).

*     Get object
      READ TABLE lt_mappings[] ASSIGNING FIELD-SYMBOL(<ls_mapping>) INDEX 1.

*     Get transport object type
      CALL FUNCTION 'CTO_OBJECT_GET_TROBJECT'
        EXPORTING
          iv_objectname       = <ls_mapping>-objectname " Object Name
          iv_objecttype       = <ls_mapping>-objecttype " Object Type
        IMPORTING
          ev_object           = lv_tr_object_type
        EXCEPTIONS
          no_transport_object = 1
          OTHERS              = 2.
      CHECK sy-subrc = 0.

      APPEND VALUE #( objecttype             = lv_tr_object_type
                      objectname             = <ls_mapping>-objectname
                      bcset_id               = <ls_mapping>-objectname && '_' && <ls_mapping>-objecttype
                      container_local        = lo_container_local
                      color                  = VALUE #( ( color-col = 6 color-int = 1 color-inv = 0 ) )
                    ) TO rt_customizing_ui[].

    ENDLOOP.

  ENDMETHOD.


  METHOD convert_record_attributes.

*   Declaration of local variables
    DATA: lv_key64x   TYPE scpr_x64,
          lv_key128x  TYPE scpr_x128,
          lv_key256x  TYPE scpr_x256,
          lv_key512x  TYPE scpr_x512,
          lv_key1024x TYPE scpr_x1024,
          lv_key2048x TYPE scpr_x2048,
          lv_key4096x TYPE scpr_x4096,
          lv_key8192x TYPE scpr_x8192.

*   Declaration of local field symbols
    FIELD-SYMBOLS <lv_key> TYPE any.

*   Initialize EXPORTING parameters
    CLEAR: et_recaattrs_extended[],
           et_orig_recnum_assigns[].

    LOOP AT cs_bcset_metadata-scprreca[] ASSIGNING FIELD-SYMBOL(<ls_scprreca>).

      DATA(ls_recaattr_extended)  = CORRESPONDING scpr_recattr( <ls_scprreca> ).

      READ TABLE mt_field_descrs[] ASSIGNING FIELD-SYMBOL(<ls_field_descr>)
                                   WITH KEY tabname = <ls_scprreca>-tablename.
      CHECK sy-subrc = 0.

      DATA(lv_key_length) = <ls_field_descr>-keylen + 6.

*     Assign variable
      IF lv_key_length <= 64.
        ASSIGN lv_key64x TO <lv_key>.
      ELSEIF lv_key_length <= 128.
        ASSIGN lv_key128x TO <lv_key>.
      ELSEIF lv_key_length <= 256.
        ASSIGN lv_key256x TO <lv_key>.
      ELSEIF lv_key_length <= 512.
        ASSIGN lv_key512x TO <lv_key>.
      ELSEIF lv_key_length <= 1024.
        ASSIGN lv_key1024x TO <lv_key>.
      ELSEIF lv_key_length <= 2048.
        ASSIGN lv_key2048x TO <lv_key>.
      ELSEIF lv_key_length <= 4096.
        ASSIGN lv_key4096x TO <lv_key>.
      ELSE.
        ASSIGN lv_key8192x TO <lv_key>.
      ENDIF.

      CLEAR <lv_key>.

      CALL FUNCTION 'SCPR_TEMPL_CT_CONV_VAL_TO_RAW'
        EXPORTING
          tabname     = ls_recaattr_extended-tablename
          only_key    = abap_true
          recnumber   = ls_recaattr_extended-recnumber                 " Data record number
          uncomplete  = ls_recaattr_extended-uncomplete
          deleteflag  = ls_recaattr_extended-deleteflag
          keylen      = <ls_field_descr>-keylen
        IMPORTING
          raw_xrecord = <lv_key>
          genkeylen   = ls_recaattr_extended-genkeylng
        TABLES
          profvalues  = cs_bcset_metadata-scprvals[]
          tabledescr  = <ls_field_descr>-descr[].

      ls_recaattr_extended-key    = <lv_key>.
      ls_recaattr_extended-keylng = <ls_field_descr>-keylen.
      APPEND ls_recaattr_extended TO et_recaattrs_extended[].

      et_orig_recnum_assigns[] = VALUE #( BASE et_orig_recnum_assigns ( CORRESPONDING #( ls_recaattr_extended MAPPING orig_recnum = recnumber
                                                                                                                      orig_genref = genref ) ) ).

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
