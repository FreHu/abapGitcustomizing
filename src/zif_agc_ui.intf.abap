INTERFACE zif_agc_ui
  PUBLIC .

  TYPES: BEGIN OF ty_customizing_ui,
           objecttype       TYPE trobjtype,
           objectname       TYPE trobj_name,
           path             TYPE string,
           bcset_id         TYPE string,
           package          TYPE devclass,
           container_local  TYPE REF TO cl_bcfg_bcset_config_container,
           container_remote TYPE REF TO cl_bcfg_bcset_config_container,
           color            TYPE lvc_t_scol,
         END OF ty_customizing_ui.

  TYPES ty_t_customizing_ui TYPE STANDARD TABLE OF ty_customizing_ui WITH DEFAULT KEY.

  METHODS display
    IMPORTING
              iv_repository_key TYPE char12
    RAISING   zcx_agc_ui.

  METHODS get_selected_customizing
    RETURNING VALUE(rt_customizing_ui) TYPE ty_t_customizing_ui.

  METHODS get_repository
    RETURNING VALUE(ro_repository) TYPE REF TO zcl_abapgit_repo_online.

ENDINTERFACE.
