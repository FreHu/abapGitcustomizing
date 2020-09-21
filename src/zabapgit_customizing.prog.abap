*&---------------------------------------------------------------------*
*& Report zabapgit_customizing
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
  REPORT zabapgit_customizing.

  TABLES sscrfields.

  INCLUDE zabapgit_password_dialog.

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

*   Get instance
    DATA(go_repository_ui) = zcl_agc_repository_ui=>get_instance( ).

*   Display output
    go_repository_ui->display( ).

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
