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

    METHODS convert_to_bcset_format
      EXPORTING
        et_record_attributes TYPE scprrecatab
        et_values            TYPE scprvalstab
        et_language_values   TYPE scprvalltab.

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
    DATA: lt_field_descrs TYPE scpr_records.

*   Initialize EXPORTING parameters
    CLEAR: et_record_attributes[],
           et_values[],
           et_language_values[].

*   Convert transport objects and its table keys to BC Set format
    CALL FUNCTION 'SCPR_TR_READ_DATA'
      IMPORTING
        tab_fielddescrs = lt_field_descrs[]
        tab_recattr     = et_record_attributes[]
        tab_values      = et_values[]
      TABLES
        tab_e071        = mt_request_objects[]   " Change & Transport System: Object Entries of Requests/Tasks
        tab_e071k       = mt_object_table_keys[] " Change & Transport System: Key Entries of Requests/Tasks
      EXCEPTIONS
        no_data_found   = 1
        OTHERS          = 2.
    IF sy-subrc = 0.

*     Get language dependent values
      CALL FUNCTION 'SCPR_TEMPL_CT_LANG_ALL_GET'
        TABLES
          values      = et_values[]
          valuesl     = et_language_values[]
          recattr     = et_record_attributes[]
          fielddescrs = lt_field_descrs[].

    ENDIF.

  ENDMETHOD.

  METHOD zif_agc_request_handler~get_data.



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

*     Get the relevant language dependent customizing content
      ls_bcset_metadata-scprvall[] = VALUE #( FOR ls_recaattr_tmp    IN ls_bcset_metadata-scprreca[]
                                              FOR ls_language_values IN lt_language_all_values[] WHERE (     tablename = ls_recaattr_tmp-tablename
                                                                                                         AND recnumber = ls_recaattr_tmp-recnumber )
                                              ( ls_language_values ) ).

*     Create container
      DATA(lo_container_local) = zcl_agc_helper=>create_container( ls_bcset_metadata ).

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

ENDCLASS.
