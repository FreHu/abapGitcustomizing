CLASS zcl_agc_helper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_bcset_metadata,
             scprattr TYPE scprattr,
             scprtext TYPE STANDARD TABLE OF scprtext WITH DEFAULT KEY,
             scprvals TYPE STANDARD TABLE OF scprvals WITH DEFAULT KEY,
             scprvall TYPE STANDARD TABLE OF scprvall WITH DEFAULT KEY,
             scprreca TYPE STANDARD TABLE OF scprreca WITH DEFAULT KEY,
             scprfldv TYPE STANDARD TABLE OF scprfldv WITH DEFAULT KEY,
             subprofs TYPE STANDARD TABLE OF scprpprl WITH DEFAULT KEY,
           END OF ty_bcset_metadata .

    CLASS-METHODS create_container
      IMPORTING
                is_bcset_metadata   TYPE ty_bcset_metadata
      RETURNING VALUE(ro_container) TYPE REF TO cl_bcfg_bcset_config_container.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_agc_helper IMPLEMENTATION.

  METHOD create_container.

    " Declaration of local internal table
    DATA: lt_objects TYPE scp1_bcs_objects.

    DATA(lt_bcset_language_values) = is_bcset_metadata-scprvall[].

    SORT lt_bcset_language_values[] BY langu.

    " Delete duplicate languages
    DELETE ADJACENT DUPLICATES FROM lt_bcset_language_values[]
    COMPARING langu.

    DATA(lt_languages) = VALUE if_bcfg_config_container=>ty_t_languages(
      FOR ls_bcset_language_value IN lt_bcset_language_values[]
      ( ls_bcset_language_value-langu ) ).

    IF lt_languages[] IS INITIAL.
      INSERT sy-langu INTO TABLE lt_languages[].
    ENDIF.

    DATA(lv_bcset_id) = is_bcset_metadata-scprreca[ 1 ]-id.

    " Extract objects
    CALL FUNCTION 'SCPR_SV_GET_OBJECTS_IN_BCSET'
      EXPORTING
        bcset_id = lv_bcset_id
        category = space
        recattr  = is_bcset_metadata-scprreca[]
      IMPORTING
        objects  = lt_objects[].

    READ TABLE lt_objects[] ASSIGNING FIELD-SYMBOL(<ls_object>) INDEX 1.

    DATA(lv_objecttype) = SWITCH #( <ls_object>-objecttype WHEN 'U' THEN 'T'
                                                           ELSE <ls_object>-objecttype ).

    DATA(lv_tablename) = SWITCH #( <ls_object>-objecttype WHEN 'U' THEN <ls_object>-objectname
                                                          ELSE space ).

    " Create mapping
    DATA(lt_mappings) = VALUE if_bcfg_config_container=>ty_t_mapping_info(
      ( objectname = <ls_object>-objectname
        objecttype = lv_objecttype
        activity = <ls_object>-activity
        tablename = lv_tablename ) ).

    " Create configuration container for remote file
    ro_container ?= cl_bcfg_config_manager=>create_container(
      io_container_type  = cl_bcfg_enum_container_type=>classic
      it_langus          = lt_languages[]
      it_object_mappings = lt_mappings[]
    ).

    " Content in BC Set format data
    DATA(lt_field_values) = VALUE if_bcfg_config_container=>ty_t_field_values(
      FOR ls_scprreca IN is_bcset_metadata-scprreca[]
        WHERE ( deleteflag IS INITIAL )
      FOR ls_scprvals IN is_bcset_metadata-scprvals
        WHERE ( tablename = ls_scprreca-tablename AND recnumber = ls_scprreca-recnumber )

      ( tablename = ls_scprvals-tablename
        fieldname = ls_scprvals-fieldname
        rec_id    = ls_scprvals-recnumber
        value     = ls_scprvals-value ) ).

    LOOP AT is_bcset_metadata-scprreca[] ASSIGNING FIELD-SYMBOL(<ls_scprreca>)
                                         WHERE deleteflag IS INITIAL.

      " Language dependent content in BC Set format
      LOOP AT is_bcset_metadata-scprvall[] ASSIGNING FIELD-SYMBOL(<ls_scprvall>)
                                           WHERE recnumber = <ls_scprreca>-recnumber.

        INSERT VALUE #( tablename = <ls_scprvall>-tablename
                        fieldname = <ls_scprvall>-fieldname
                        rec_id    = <ls_scprvall>-recnumber
                        langu     = <ls_scprvall>-langu
                        value     = <ls_scprvall>-value )
        INTO TABLE lt_field_values[].

      ENDLOOP.

    ENDLOOP.

    " Add data
    ro_container->if_bcfg_config_container~add_lines_by_fields( lt_field_values[] ).

    " Deleted content in BC Set format data
    lt_field_values[] = VALUE if_bcfg_config_container=>ty_t_field_values(
      FOR ls_scprreca IN is_bcset_metadata-scprreca[]
        WHERE ( deleteflag = 'L' )
      FOR ls_scprvals IN is_bcset_metadata-scprvals
        WHERE ( tablename = ls_scprreca-tablename AND recnumber = ls_scprreca-recnumber )

      ( tablename = ls_scprvals-tablename
        fieldname = ls_scprvals-fieldname
        rec_id    = ls_scprvals-recnumber
        value     = ls_scprvals-value ) ).

    LOOP AT is_bcset_metadata-scprreca[] ASSIGNING <ls_scprreca>
                                         WHERE deleteflag = 'L'.

      " Language dependent content in BC Set format
      LOOP AT is_bcset_metadata-scprvall[] ASSIGNING <ls_scprvall>
                                           WHERE recnumber = <ls_scprreca>-recnumber.

        INSERT VALUE #( tablename = <ls_scprvall>-tablename
                        fieldname = <ls_scprvall>-fieldname
                        rec_id    = <ls_scprvall>-recnumber
                        langu     = <ls_scprvall>-langu
                        value     = <ls_scprvall>-value )
        INTO TABLE lt_field_values[].

      ENDLOOP.

    ENDLOOP.

    TRY.
        " Add deleted data
        ro_container->if_bcfg_config_container~add_deletions_by_fields( lt_field_values[] ).
      CATCH cx_root INTO DATA(cx).
        MESSAGE cx->get_text( ) TYPE 'E'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
