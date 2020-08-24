*&---------------------------------------------------------------------*
*& Report zabapgit_customizing
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
  REPORT zabapgit_customizing.

  TYPES: BEGIN OF gty_repository_display,
           name    TYPE string,
           url     TYPE string,
           package TYPE devclass,
         END OF gty_repository_display.

  DATA: gt_repository_display TYPE STANDARD TABLE OF gty_repository_display.

  DATA(go_repository_service) = zcl_abapgit_repo_srv=>get_instance( ).

  DATA(gt_repository_list) = go_repository_service->list( ).

  LOOP AT gt_repository_list[] ASSIGNING FIELD-SYMBOL(<ls_repository_list>).

    APPEND VALUE #( name = <ls_repository_list>->get_name( )
                    package = <ls_repository_list>->get_package( )
                  ) TO gt_repository_display[].

  ENDLOOP.

  cl_salv_table=>factory(
    IMPORTING
      r_salv_table   = DATA(go_repository_output)                          " Basis Class Simple ALV Tables
    CHANGING
      t_table        = gt_repository_display[]
  ).

  go_repository_output->set_screen_status(
    EXPORTING
      report        = sy-repid         " ABAP Program: Current Master Program
      pfstatus      = 'STANDARD'                       " Screens, Current GUI Status
      set_functions = go_repository_output->c_functions_all " ALV: Data Element for Constants
  ).

  DATA(go_columns) = go_repository_output->get_columns( ).

  DATA(go_column) = go_columns->get_column( 'NAME' ).
  go_column->set_long_text( 'Display Name' ).

  go_column = go_columns->get_column( 'URL' ).
  go_column->set_long_text( 'Repository' ).

  go_column = go_columns->get_column( 'PACKAGE' ).
  go_column->set_long_text( 'Package' ).

  DATA(go_functions) = go_repository_output->get_functions( ).
  go_functions->set_default( ).

  go_repository_output->display( ).
